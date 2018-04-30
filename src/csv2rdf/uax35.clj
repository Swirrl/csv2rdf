(ns csv2rdf.uax35)

{:prefix ""
 :sign #{\- \+ nil}
 :modifier #{\% \u2030 nil}
 :int-primary-group-size #{nil 4}
 :int-secondary-group-size #{nil 2}
 :int-min-length 4}

(def mph-char \u2030)

(defn is-modifier-char? [^Character c]
  (contains? #{\% mph-char} c))

;; <prefix>([#0,])*.([#0,])*E[+-]?(\d)+<modifier>?<suffix>?
(defn parse-prefix [^String format-str]
  (loop [idx 0
         {:keys [prefix-buf sign modifier] :as state} {:prefix-buf (StringBuilder.) :sign nil :modifier nil}]
    (if (>= idx (.length format-str))
      ;;NOTE: guaranteed to error but defer until parsing int part
      [idx {:prefix (.toString prefix-buf) :sign sign :modifier modifier}]
      (let [c (.charAt format-str idx)]
        (cond
          (is-modifier-char? c)
          (if (some? modifier)
            (throw (IllegalArgumentException. "Multiple modifiers in prefix"))
            (do (.append prefix-buf c)
                (recur (inc idx) (assoc state :modifier c))))

          (or (= \+ c) (= \- c))
          (if (some? sign)
            (throw (IllegalArgumentException. "Multiple signs in prefix"))
            (do
              (.append prefix-buf c)
              (recur (inc idx) (assoc state :sign c))))

          ;;reached start of int format
          (or (= \0 c) (= \# c))
          [idx {:prefix (.toString prefix-buf) :sign sign :modifier modifier}]

          ;;literal char
          :else
          (do
            (.append prefix-buf c)
            (recur (inc idx) state)))))))

(defn parse-integer-groups [^String format-str start-index]
  (loop [idx start-index
         state :any-num
         buf (StringBuilder.)
         groups []
         min-length 0]
    (if (>= idx (.length format-str))
      ;;add last group if one is in progress
      (let [groups (if (> (.length buf) 0)
                     (conj groups (.toString buf))
                     groups)]
        {:index idx :groups groups :min-length min-length})
      (let [c (.charAt format-str idx)]
        (case state
          :any-num
          (case c
            \# (recur (inc idx) :any-num-or-group (.append buf c) groups min-length)
            \0 (recur (inc idx) :required-num-or-group (.append buf c) groups (inc min-length))
            (throw (IllegalArgumentException. (format "Illegal group definition - expected # or 0 at index %d" idx))))

          :any-num-or-group
          (case c
            \# (recur (inc idx) :any-num-or-group (.append buf c) groups min-length)
            \0 (recur (inc idx) :required-num-or-group (.append buf c) groups (inc min-length))
            \, (recur (inc idx) :any-num (StringBuilder.) (conj groups (.toString buf)) min-length)
            {:index idx :groups (conj groups (.toString buf) :min-length min-length)})

          :required-num-or-group
          (case c
            \# (throw (IllegalArgumentException. (format "Padding character # at index %d not permitted after first required character" idx)))
            \0 (recur (inc idx) :required-num-or-group (.append buf c) groups (inc min-length))
            \, (recur (inc idx) :required-num (StringBuilder.) (conj groups (.toString buf)) min-length)
            {:index idx :groups (conj groups (.toString buf)) :min-length min-length})

          :required-num
          (case c
            \# (throw (IllegalArgumentException. (format "Padding character # at index %d not permitted after first required character" idx)))
            \0 (recur (inc idx) :required-num-or-group (.append buf c) groups (inc min-length))
            (throw (IllegalArgumentException. (format "Illegal group definition - expected 0 at index %d" idx)))))))))

(defn parse-integer-part [^String format-str start-index]
  (let [{:keys [index groups min-length]} (parse-integer-groups format-str start-index)]
    (if (= 0 min-length)
      (throw (IllegalArgumentException. "At least one required character 0 required in integer format"))
      (case (count groups)
        ;;should never happen since min-length > 0!
        0 (throw (IllegalArgumentException. "At least one integer group required"))

        ;;single group e.g. ###0
        ;;no group separator is expected in the integer part
        ;;all numbers should contain no more than (count group) characters (?)
        ;;TODO: check if max size should apply
        1 (let [group (first groups)]
            [index {:primary-group-size nil :secondary-group-size nil :min-length min-length :max-length (count group)}])

        ;;at least 2 groups exist - length of last group is the primary group size, length of penultimate group is the secondary group size
        ;;any other groups are ignored
        (let [[secondary-group primary-group] (take-last 2 groups)]
          [index {:primary-group-size   (count primary-group)
                  :secondary-group-size (count secondary-group)
                  :min-length           min-length
                  :max-length           nil}])))))

(defn parse-decimal-groups [^String format-string start-index]
  (loop [idx start-index
         state :required-num
         buf (StringBuilder.)
         groups []
         min-length 0]
    (if (>= idx (.length format-string))
      ;;complete current group if one is in progress
      (let [groups (if (pos? (.length buf))
                     (conj groups (.toString buf))
                     groups)]
        {:min-length min-length :groups groups :index idx})
      (let [c (.charAt format-string idx)]
        (case state
          :required-num
          (if (= c \0)
            (recur (inc idx) :any-num-or-group (.append buf c) groups (inc min-length))
            (throw (IllegalArgumentException. "At least one required character 0 required in decimal format")))

          :any-num-or-group
          (case c
            \0 (recur (inc idx) :any-num-or-group (.append buf c) groups (inc min-length))
            \# (recur (inc idx) :padding-num-or-group (.append buf c) groups min-length)
            \, (recur (inc idx) :any-num (StringBuilder.) (conj groups (.toString buf)) min-length)
            {:min-length min-length :groups (conj groups (.toString buf)) :index idx})

          :any-num
          (case c
            \0 (recur (inc idx) :any-num-or-group (.append buf c) groups (inc min-length))
            \# (recur (inc idx) :padding-num-or-group (.append buf c) groups min-length)
            \, (throw (IllegalArgumentException. (format "Invalid decimal group - unexpected , at index %d" idx)))
            {:min-length min-length :groups (conj groups (.toString buf) :index idx)})

          :padding-num-or-group
          (case c
            \0 (throw (IllegalArgumentException. (format "Required decimal character 0 at index %d not permitted after first padding character" idx)))
            \# (recur (inc idx) :padding-num-or-group (.append buf c) groups min-length)
            \, (recur (inc idx) :padding-num (StringBuilder.) (conj groups (.toString buf)) min-length)
            {:min-length min-length :groups (conj groups (.toString buf) :index idx)})

          :padding-num
          (if (= \# c)
            (recur (inc idx) :padding-num-or-group (.append buf c) groups min-length)
            (throw (IllegalArgumentException. (format "Invalid decimal group - expected # at index %d" idx)))))))))

(defn parse-decimal-part [^String format-string start-index]
  (if (>= start-index (.length format-string))
    [start-index nil]
    (let [c (.charAt format-string start-index)]
      (if (= \. c)
        (let [{:keys [min-length groups index]} (parse-decimal-groups format-string (inc start-index))]
          (if (= 0 min-length)
            (throw (IllegalArgumentException. "At least one required character 0 required in decimal part"))
            (case (count groups)
              ;;should never happen since min-length > 0
              0 (throw (IllegalArgumentException. "At least one decimal group required"))

              ;;if one group, set the max length as the length of the group
              1
              [index {:min-length min-length
                      :max-length (count (first groups))
                      :group-size nil}]

              ;;TODO: check how to handle decimal groups
              (let [primary-group (first groups)]
                [index {:min-length min-length
                        :max-length nil
                        :group-size (count primary-group)}]))))
        [start-index nil]))))

(defn parse-exponent-part [^String format-string start-index]
  (if (>= start-index (.length format-string))
    (throw (IllegalArgumentException. "Invalid exponent - expected +, - or 0"))
    (let [c (.charAt format-string start-index)
          [sign min-length] (cond
                              (or (= \+ c) (= \- c)) [c 0]
                              (= \0 c) [nil 1]
                              :else (throw (IllegalArgumentException. (format "Invalid exponent - expected +, - or 0 at index %d" start-index))))]
      (loop [idx (inc start-index)
             state :any-num
             min-length min-length
             max-length min-length]
        (if (>= idx (.length format-string))
          {:sign sign :min-length min-length :max-length max-length :index idx}
          (let [c (.charAt format-string idx)]
            (case state
              :any-num
              (case c
                \0 (recur (inc idx) :any-num (inc min-length) (inc max-length))
                \# (recur (inc idx) :padding-num min-length (inc max-length))
                {:sign sign :min-length min-length :max-length max-length :index idx})

              :padding-num
              (case c
                \0 (throw (IllegalArgumentException. (format "Required decimal character 0 at index %d not permitted in exponent after first padding character" idx)))
                \# (recur (inc idx) :padding-num min-length (inc max-length))
                {:sign sign :min-length min-length :max-length max-length :index idx}))))))))

(defn parse-exponent [^String format-string start-index]
  (if (>= start-index (.length format-string))
    [start-index nil]
    (let [c (.charAt format-string start-index)]
      (if (= \E c)
        (let [{:keys [min-length index] :as exp} (parse-exponent-part format-string (inc start-index))]
          (if (= 0 min-length)
            (throw (IllegalArgumentException. "Invalid exponent - at least one digit required"))
            [index (dissoc exp :index)]))
        [start-index nil]))))

(defn parse-suffix [^String format-string start-index prefix-modifier]
  (if (< start-index (.length format-string))
    (let [maybe-modifier (.charAt format-string start-index)]
      (cond
        (and (some? prefix-modifier) (is-modifier-char? maybe-modifier))
        (throw (IllegalArgumentException. "Cannot specify modifier in both prefix and suffix"))

        (is-modifier-char? maybe-modifier)
        (let [suffix (.substring format-string (inc start-index))]
          (if (some is-modifier-char? suffix)
            (throw (IllegalArgumentException. "Multiple modifiers in suffix"))
            {:suffix suffix :modifier maybe-modifier}))

        :else
        (let [suffix (.substring format-string start-index)]
          (if (some is-modifier-char? suffix)
            (throw (IllegalArgumentException. "Modifier must be first character of suffix"))
            {:suffix suffix :modifier nil}))))))

(defn parse-number-format
  ([format-str] (parse-number-format format-str nil nil))
  ([format-str group-char decimal-char]
   (let [[index {prefix-modifier :modifier :as prefix-state}] (parse-prefix format-str)
         [index integer-state] (parse-integer-part format-str index)
         [index decimal-state] (parse-decimal-part format-str index)
         [index exponent-state] (parse-exponent format-str index)
         suffix-state (parse-suffix format-str index prefix-modifier)]
     {:prefix       (dissoc prefix-state :index)
      :integer      integer-state
      :decimal      decimal-state
      :exponent     exponent-state
      :suffix       suffix-state
      :group-char   (or group-char \,)
      :decimal-char (or decimal-char \.)
      :modifier (or prefix-modifier (:modifier suffix-state))})))
