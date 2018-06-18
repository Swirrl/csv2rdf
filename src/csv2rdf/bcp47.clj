(ns csv2rdf.bcp47
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [csv2rdf.util :as util]))

(def tag-elements
  ^{:doc "Tag grammar elements in the order the appear within a tag"}
  [:language :script :region :variant :extension :private])

(def ^{:bcp47-spec "2.1"} production-regexes
  {:language #"^[a-zA-Z]{2,3}$"
   :extlang #"^[a-zA-Z]{3}$"
   :script #"^[a-zA-Z]{4}$"
   :region #"^([a-zA-Z]{2}|\d{3})$"
   :variant #"^([a-zA-Z0-9]{5,8}|\d[a-zA-Z0-9]{3})$"
   :singleton #"^(\d|[a-wA-Wy-zY-Z])$"
   :extension-element #"^[a-zA-Z0-9]{2,8}$"
   :private #"^x$"
   :private-element #"^[a-zA-Z0-9]{1,8}$"})

(def ^{:doc "Grammar production which can start each grammar element"} starting-productions
  {:language :language
   :script :script
   :region :region
   :variant :variant
   :singleton :extension
   :private :private})

(defn- matches?
  "Returns whether s matches the regex re"
  [re s]
  (some? (re-find re s)))

(defn- productions-start-elements
  "Returns a set of all tag element types which could be started by the given set of grammar productions."
  [productions]
  (into #{} (remove nil? (map starting-productions productions))))

(defn get-element-type [productions allowed-states]
  (let [candidates (set/intersection allowed-states (productions-start-elements productions))]
    (case (count candidates)
      0 (throw (IllegalArgumentException. (format "Invalid language tag: expected one of %s" (string/join ", " (map name allowed-states)))))
      1 (first candidates)
      (throw (IllegalArgumentException. (str "Ambiguous element for productions " (string/join ", " (map name candidates))))))))

(defn- element-productions
  "Returns a set of all the grammar productions that match the given element string."
  [^String element]
  (into #{} (remove nil? (map (fn [[prod r]]
                                (if (re-find r element)
                                  prod))
                              production-regexes))))

(defmulti parse-element
          "The next part of a language tag given the current element, its type and the elements that follow.
           Returns a pair containing the remaining elements and the parsed tag part."
          (fn [element following-elements state] state))

(defn parse-production-sequence
  "Parses a sequence of tag elements which match the given production in the grammar. Parses at most max-count items
   if specified. Throws an exception if fewer than min-count matches were found when specified."
  [elements production min-count max-count]
  (letfn [(validate-min [remaining matches match-count]
            (if (and (some? min-count) (< match-count min-count))
              (throw (IllegalArgumentException. (format "Expected at least %d occurences of %s" min-count (name production))))
              [remaining matches]))]
    (loop [elements elements
           matches []
           match-count 0]
      (if (and (seq elements)
               (or (nil? max-count) (< match-count max-count)))
        (let [r (get production-regexes production)
              e (first elements)]
          (if (matches? r e)
            (recur (rest elements) (conj matches e) (inc match-count))
            (validate-min elements matches match-count)))
        (validate-min elements matches match-count)))))

(defmethod parse-element :language [lang following-elements _type]
  (let [[remaining ext-langs] (parse-production-sequence following-elements :extlang 0 3)]
    [remaining {:language (string/join "-" (cons lang ext-langs))}]))

(defmethod parse-element :extension [ext following-elements _type]
  (let [[remaining exts] (parse-production-sequence following-elements :extension-element 1 nil)]
    [remaining {:extension {:tag ext :elements exts}}]))

(defmethod parse-element :private [_x following-elements _type]
  (let [[remaining private] (parse-production-sequence following-elements :private-element 1 nil)]
    [remaining {:private private}]))

(defmethod parse-element :default [element following-elements type]
  [following-elements {type element}])

(defn next-states
  "Returns the set of tag elements which can follow the given element"
  [state]
  (into #{} (rest (drop-while #(not= state %) tag-elements))))

(defn- parse
  "Parses a language tag from a sequence of tag segments. Allowed states is the set of grammar elements which are
   allowed at the current point in the parse. partial-tag contains the tag parsed so far."
  [elements allowed-states partial-tag]
  (if (seq elements)
    (let [e (first elements)
          prods (element-productions e)
          element-type (get-element-type prods allowed-states)]
      (if (contains? allowed-states element-type)
        (let [[next-elements tag-item] (parse-element e (next elements) element-type)]
          (recur next-elements (next-states element-type) (merge partial-tag tag-item)))
        (let [msg (str "Invalid language tag - expected one of " (string/join (map name allowed-states)))]
          (throw (IllegalArgumentException. msg)))))
    partial-tag))

(defn parse-language-tag
  "Parses a language tag string. Throws IllegalArgumentException if the string does not represent a valid
   language tag."
  [tag-string]
  (parse (string/split tag-string #"-") #{:language :private} {}))


(defmulti format-language-tag-part (fn [part-key value] part-key))

(defmethod format-language-tag-part :extension [_key {:keys [tag elements]}]
  (string/join "-" (cons tag elements)))

(defmethod format-language-tag-part :private [_key elements]
  (string/join "-" (cons "x" elements)))

(defmethod format-language-tag-part :default [_key value] value)

(defn format-language-tag
  "Returns the string representation of the given format tag."
  [tag]
  (string/join "-" (remove nil? (map (fn [key]
                                       (if (contains? tag key)
                                         (format-language-tag-part key (get tag key))))
                                     tag-elements))))

(defn non-empty-prefixes
  "Returns a collection of the non-empty prefixes of the given vector in descending order of size"
  [nev]
  {:pre [(vector? nev)
         (not (empty? nev))]}
  (loop [v (pop nev)
         acc [nev]]
    (if (empty? v)
      acc
      (recur (pop v) (conj acc v)))))

(defmulti language-tag-part-truncations (fn [part-key lang-tag] part-key))

(defmethod language-tag-part-truncations :extension [_key {{:keys [tag elements]} :extension :as lang-tag}]
  (map (fn [es] (assoc lang-tag :extension {:tag tag :elements es})) (non-empty-prefixes elements)))

(defmethod language-tag-part-truncations :private [key {:keys [private] :as lang-tag}]
  (map (fn [p] (assoc lang-tag :private p)) (non-empty-prefixes private)))

(defmethod language-tag-part-truncations :default [_key tag]
  [tag])

(defn ^{:bcp47-spec "4.4.2"} truncations
  "Returns a sequence of successively shorter version of a format tag where the right-most sub-tag
   is removed from the preceding tag."
  [tag]
  (loop [tag tag
         acc []
         keys (reverse tag-elements)]
    (if (seq keys)
      (let [k (first keys)]
        (if (contains? tag k)
          (recur (dissoc tag k) (into acc (language-tag-part-truncations k tag)) (next keys))
          (recur tag acc (next keys))))
      acc)))

(defn language-tag-string-truncation-strings
  "Returns a sequence of successively truncated language tag strings for the given tag string."
  [tag-string]
  (let [tag (parse-language-tag tag-string)]
    (map format-language-tag (truncations tag))))

(def language-tag-strings-equal? util/equals-ignore-case?)