(ns csv2rdf.uax35
  "Parser for unicode number formats described in http://unicode.org/reports/tr35/tr35-numbers.html#Number_Format_Patterns.")

(def per-mille-char \u2030)

(defn is-modifier-char? [^Character c]
  (contains? #{\% per-mille-char} c))

(defn apply-modifier
  "Modifies the result of parsing a formatted number according to any contained modifier (percent or mph)."
  [value modifier]
  (cond
    (= \% modifier) (/ value 100)
    (= per-mille-char modifier) (/ value 1000)
    :else value))

;; <prefix>([#0,])*.([#0,])*E[+-]?(\d)+<modifier>?<suffix>?
(defn parse-prefix [^String format-str]
  (loop [idx 0
         prefix-buf (StringBuilder.)
         sign nil
         modifier nil]
    (if (>= idx (.length format-str))
      ;;NOTE: guaranteed to error but defer until parsing int part
      [idx {:prefix (.toString prefix-buf) :sign sign :modifier modifier}]
      (let [c (.charAt format-str idx)]
        (cond
          (is-modifier-char? c)
          (if (some? modifier)
            (throw (IllegalArgumentException. "Multiple modifiers in prefix"))
            (do (.append prefix-buf c)
                (recur (inc idx) prefix-buf sign c)))

          (or (= \+ c) (= \- c))
          (if (some? sign)
            (throw (IllegalArgumentException. "Multiple signs in prefix"))
            (recur (inc idx) (.append prefix-buf c) c modifier))

          ;;reached start of int format
          (or (= \0 c) (= \# c))
          [idx {:prefix (.toString prefix-buf) :sign sign :modifier modifier}]

          ;;literal char
          :else
          (recur (inc idx) (.append prefix-buf c) sign modifier))))))

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
            {:index idx :groups (conj groups (.toString buf)) :min-length min-length})

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
    (case (count groups)
      0 (throw (IllegalArgumentException. "At least one integer group required"))

      ;;single group e.g. ###0
      ;;no group separator is expected in the integer part
      1
      [index {:min-length min-length :max-length nil :groups ::single}]

      ;;two groups e.g. ##,#00
      ;;length of last group is the primary group size
      ;;no secondary group
      2
      (let [primary-group (last groups)]
        [index {:min-length min-length
                :max-length nil
                :groups {:primary-group-size (count primary-group)
                         :secondary-group-size nil}}])

      ;;at least 3 groups exist - length of last group is the primary group size, length of penultimate group is the secondary group size
      ;;any other groups are ignored
      (let [[secondary-group primary-group] (take-last 2 groups)]
        [index {:min-length           min-length
                :max-length           nil
                :groups {:primary-group-size (count primary-group)
                         :secondary-group-size (count secondary-group)}}]))))

(defn parse-decimal-groups [^String format-string start-index]
  (loop [idx start-index
         state :any-num
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
          :any-num
          (case c
            \0 (recur (inc idx) :any-num-or-group (.append buf c) groups (inc min-length))
            \# (recur (inc idx) :padding-num-or-group (.append buf c) groups min-length)
            \, (throw (IllegalArgumentException. (format "Invalid decimal group - unexpected , at index %d" idx)))
            {:min-length min-length :groups (conj groups (.toString buf)) :index idx})

          :any-num-or-group
          (case c
            \0 (recur (inc idx) :any-num-or-group (.append buf c) groups (inc min-length))
            \# (recur (inc idx) :padding-num-or-group (.append buf c) groups min-length)
            \, (recur (inc idx) :any-num (StringBuilder.) (conj groups (.toString buf)) min-length)
            {:min-length min-length :groups (conj groups (.toString buf)) :index idx})

          :padding-num-or-group
          (case c
            \0 (throw (IllegalArgumentException. (format "Required decimal character 0 at index %d not permitted after first padding character" idx)))
            \# (recur (inc idx) :padding-num-or-group (.append buf c) groups min-length)
            \, (recur (inc idx) :padding-num (StringBuilder.) (conj groups (.toString buf)) min-length)
            {:min-length min-length :groups (conj groups (.toString buf)) :index idx})

          :padding-num
          (if (= \# c)
            (recur (inc idx) :padding-num-or-group (.append buf c) groups min-length)
            (throw (IllegalArgumentException. (format "Invalid decimal group - expected # at index %d" idx)))))))))

(defn parse-decimal-part [^String format-string start-index]
  (if (>= start-index (.length format-string))
    [start-index ::none]
    (let [c (.charAt format-string start-index)]
      (if (= \. c)
        (let [{:keys [min-length groups index] :as res} (parse-decimal-groups format-string (inc start-index))]
          (case (count groups)
            0
            [index {:min-length 0
                    :max-length nil
                    :group ::single}]

            ;;if one group, set the max length as the length of the group
            1
            [index {:min-length min-length
                    :max-length (count (first groups))
                    :group {:size nil}}]

            (let [primary-group (first groups)]
              [index {:min-length min-length
                      :max-length nil
                      :group {:size (count primary-group)}}])))
        [start-index ::none]))))

(defn parse-exponent-part [^String format-string start-index]
  (if (>= start-index (.length format-string))
    (throw (IllegalArgumentException. "Invalid exponent - expected +, - or 0"))
    (let [c (.charAt format-string start-index)
          [sign min-length max-length] (cond
                                         (or (= \+ c) (= \- c)) [c 0 0]
                                         (= \0 c) [nil 1 1]
                                         (= \# c) [nil 0 1]
                                         :else (throw (IllegalArgumentException. (format "Invalid exponent - expected +, -, # or 0 at index %d" start-index))))]
      (loop [idx (inc start-index)
             state :any-num
             min-length min-length
             max-length max-length]
        (if (>= idx (.length format-string))
          {:sign sign :min-length min-length :max-length max-length :index idx}
          (let [c (.charAt format-string idx)]
            (case state
              :any-num
              (case c
                \0 (recur (inc idx) :required-num (inc min-length) (inc max-length))
                \# (recur (inc idx) :any-num min-length (inc max-length))
                {:sign sign :min-length min-length :max-length max-length :index idx})

              :required-num
              (case c
                \0 (recur (inc idx) :required-num min-length (inc max-length))
                \# (throw (IllegalArgumentException. (format "Exponent padding character # at index %d not permitted in exponent after first required character" idx)))
                {:sign sign :min-length min-length :max-length max-length :index idx}))))))))

(defn parse-exponent [^String format-string start-index]
  (if (>= start-index (.length format-string))
    [start-index ::none]
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
        (let [remaining-suffix (.substring format-string (inc start-index))]
          ;;check no other modifier exists in the rest of the suffix after the first modifier
          (if (some is-modifier-char? remaining-suffix)
            (throw (IllegalArgumentException. "Multiple modifiers in suffix"))
            {:suffix (.substring format-string start-index) :modifier maybe-modifier}))

        :else
        (let [suffix (.substring format-string start-index)]
          (if (some is-modifier-char? suffix)
            (throw (IllegalArgumentException. "Modifier must be first character of suffix"))
            {:suffix suffix :modifier nil}))))
    {:suffix "" :modifier nil}))

(defn parse-number-format
  ([format-str] (parse-number-format format-str nil nil))
  ([format-str group-char decimal-char]
   (let [[index {prefix-modifier :modifier :as prefix-state}] (parse-prefix format-str)
         [index integer-state] (parse-integer-part format-str index)
         [index decimal-state] (parse-decimal-part format-str index)
         [index exponent-state] (parse-exponent format-str index)
         suffix-state (parse-suffix format-str index prefix-modifier)]
     {:prefix       prefix-state
      :integer      integer-state
      :decimal      decimal-state
      :exponent     exponent-state
      :suffix       suffix-state
      :group-char   (or group-char \,)
      :decimal-char (or decimal-char \.)
      :modifier (or prefix-modifier (:modifier suffix-state))})))

(def optional-sign-prefix {:prefix "" :sign nil})
(def flexible-integer {:primary-group-size nil
                       :secondary-group-size nil
                       :min-length 1
                       :max-length nil})

(def flexible-decimal {:group-size nil
                       :min-length nil
                       :max-length nil})

(defn create-floating-format [group-char decimal-char]
  {:prefix optional-sign-prefix
   :integer flexible-integer
   :decimal flexible-decimal
   :exponent {:sign nil
              :min-length nil
              :max-length nil}
   :suffix ::optional-modifier
   :decimal-char (or decimal-char \.)
   :group-char (or group-char \,)})

(defn create-integer-format [group-char decimal-char]
  {:prefix optional-sign-prefix
   :integer flexible-integer
   :decimal ::none
   :exponent ::none
   :suffix ::optional-modifier
   :decimal-char (or decimal-char \.)
   :group-char (or group-char \,)})

(defn create-decimal-format [group-char decimal-char]
  {:prefix optional-sign-prefix
   :integer flexible-integer
   :decimal flexible-decimal
   :exponent ::none
   :suffix ::optional-modifier
   :decimal-char (or decimal-char \.)
   :group-char (or group-char \,)})

(defn allows-decimal-part? [format]
  (not= ::none (:decimal format)))

(defn allows-exponent-part? [format]
  (not= ::none (:exponent format)))

;;numeric parsing

(defn parse-numeric-prefix [^String s {:keys [^String prefix sign] :as prefix-spec}]
  (if (.startsWith s prefix)
    (cond
      (= \+ sign) [(.length prefix) {:negative? false}]
      (= \- sign) [(.length prefix) {:negative? true}]

      ;;sign is optional in format so see if one exists
      (nil? sign) (let [maybe-sign-index (.length prefix)]
                    (if (< maybe-sign-index (.length s))
                      (case (.charAt s maybe-sign-index)
                        \+ [(inc maybe-sign-index) {:negative? false}]
                        \- [(inc maybe-sign-index) {:negative? true}]
                        [maybe-sign-index {:negative false}])
                      ;;NOTE: error if no numeric part but this will be detected later
                      [(.length prefix) {:negative? false}]))

      ;;TODO: create spec for number format
      :else (throw (IllegalArgumentException. (format "Invalid number format - bad sign %c in prefix definition" sign))))
    (throw (IllegalArgumentException. (format "String does not contain expected prefix '%s'" prefix)))))

(defn is-ascii-digit? [^Character c]
  (let [ci (int c)]
    ;;48 - ASCII 0
    ;;57 - ASCII 9
    (and (>= ci 48) (<= ci (int 57)))))

(defn parse-numeric-groups [^String s start-index ^Character group-char]
  (loop [idx start-index
         state :number
         buf (StringBuilder.)
         groups []]
    (if (< idx (.length s))
      (let [c (.charAt s idx)]
        (case state
          :number
          (if (is-ascii-digit? c)
            (recur (inc idx) :number-or-group (.append buf c) groups)
            (throw (IllegalArgumentException. (format "Expected digit at index %d" idx))))

          :number-or-group
          (cond
            (Character/isDigit c)
            (recur (inc idx) :number-or-group (.append buf c) groups)

            (= group-char c)
            (recur (inc idx) :number (StringBuilder.) (conj groups (.toString buf)))

            :else [idx (conj groups (.toString buf))])

          (throw (IllegalStateException. (format "Invalid state: %s" (name state))))))
      (if (pos? (.length buf))
        [idx (conj groups (.toString buf))]
        [idx groups]))))

(defn validate-group-size [group-index ^String group group-size]
  (if (= 0 group-index)
    (if (> (.length group) group-size)
      (throw (IllegalArgumentException. (format "First integer group exceeds expected group size %d" group-size))))
    (if (not= group-size (.length group))
      (throw (IllegalArgumentException. (format "Integer group %d invalid - expected size %d" group-index group-size))))))

(defn validate-group-sizes [groups group-size]
  (doseq [[idx group] (map-indexed vector groups)]
    (validate-group-size idx group group-size)))

(defn validate-numeric-groups [groups {:keys [primary-group-size secondary-group-size] :as group-spec}]
  (cond
    ;;only single group permitted
    (and (= ::single group-spec) (> (count groups) 1))
    (throw (IllegalArgumentException. (format "Expected single group but found %d" (count groups))))

    ;;no defined group sizes
    (nil? primary-group-size)
    nil

    ;;only primary group size is defined
    ;;first group size can be up to primary group size, remaining groups must
    (nil? secondary-group-size)
    (validate-group-sizes groups primary-group-size)

    ;;both primary and secondary group exist
    :else
    (case (count groups)
      ;;NOTE: invalid but will be validated later
      0 nil

      ;;single group so validate against primary group size
      1 (validate-group-size 0 (first groups) primary-group-size)

      ;;mutliple groups - validate last against primary group size, rest against secondary group size
      (let [primary-group (last groups)
            secondary-groups (butlast groups)]
        (validate-group-size (dec (count groups)) primary-group primary-group-size)
        (validate-group-sizes secondary-groups secondary-group-size)))))

(defn validate-digit-count [^String digits part-name {:keys [min-length max-length] :as part-spec}]
  (let [digit-count (.length digits)]
    (when (and (some? min-length) (< digit-count min-length))
      (throw (IllegalArgumentException. (format "Not enough digits in %s part (expected at least %d, got %d)" part-name min-length digit-count))))
    (when (and (some? max-length) (> digit-count max-length))
      (throw (IllegalArgumentException. (format "Too many digits in %s part (expected at most %d, got %d)" part-name max-length digit-count))))))

(defn parse-numeric-integer [^String s start-index group-char {:keys [min-length max-length] :as integer-spec}]
  (let [[index groups] (parse-numeric-groups s start-index group-char)
        digits (apply str groups)
        digit-count (count digits)]
    (validate-numeric-groups groups (:groups integer-spec))
    (cond
      (< digit-count min-length)
      (throw (IllegalArgumentException. (format "Not enough digits in integer part (expected at least %d, got %d)" min-length digit-count)))

      (and (some? max-length) (> digit-count max-length))
      (throw (IllegalArgumentException. (format "Too many digits in integer part (expected at most %d, got %d)" max-length digit-count)))

      :else [index {:integer-digits digits}])))

(defn validate-decimal-groups [groups {{group-size :size :as group} :group :as decimal-spec}]
  ;;if decimal group size specified, then all groups except the last must match the expected size
  ;;the last group must be at most group-size in length
  (cond
    (and (= ::none decimal-spec) (pos? (count groups)))
    (throw (IllegalArgumentException. "Format forbids decimal component"))

    (and (= ::single group) (> (count groups) 1))
    (throw (IllegalArgumentException. "Expected single decimal group, got %d" (count groups)))

    (some? group-size)
    (let [exact-groups (butlast groups)
          ^String last-group (last groups)]
      (if (> (.length last-group) group-size)
        (throw (IllegalArgumentException. (format "Last decimal group exceeds expected group size %d" group-size))))

      (doseq [^String group exact-groups]
        (if (not= (.length group) group-size)
          (throw (IllegalArgumentException. (format "Decimal group does not have expected size %d" group-size))))))

    :else nil))

(defn parse-numeric-decimal [^String s start-index ^Character decimal-char group-char decimal-spec]
  (let [[index groups] (if (< start-index (.length s))
                         (let [c (.charAt s start-index)]
                           (if (= decimal-char c)
                             (if (= ::none decimal-spec)
                               (throw (IllegalArgumentException. (format "Invalid decimal char %c at index %d - format forbids decimal component" decimal-char start-index)))
                               (parse-numeric-groups s (inc start-index) group-char))
                             [start-index []]))
                         [start-index []])
        decimal-digits (apply str groups)]
    (validate-decimal-groups groups decimal-spec)
    (validate-digit-count decimal-digits "decimal" decimal-spec)
    [index {:decimal-digits decimal-digits}]))

(defn parse-exponent-digits [^String s start-index]
  (loop [idx start-index
         buf (StringBuilder.)]
    (if (< idx (.length s))
      (let [c (.charAt s idx)]
        (if (Character/isDigit c)
          (recur (inc idx) (.append buf c))
          [idx (.toString buf)]))
      [idx (.toString buf)])))

(defn parse-numeric-exponent-part [^String s start-index {:keys [^Character sign] :as exponent-spec}]
  ;;NOTE: number format does allow an empty exponent?
  (if (< start-index (.length s))
    (let [c (.charAt s start-index)]
      (cond
        ;;sign is optional within exponent
        (nil? sign)
        (let [[exp-index negative?] (case c
                                      \+ [(inc start-index) false]
                                      \- [(inc start-index) true]
                                      [start-index false])
              [index digits] (parse-exponent-digits s exp-index)]
          [index {:exponent-digits digits :exponent-negative? negative?}])

        ;;sign is equal to required value
        (= sign c)
        (let [[index digits] (parse-exponent-digits s (inc start-index))
              exponent-negative? (case sign
                                   \+ false
                                   \- true
                                   (throw (IllegalStateException. (format "Invalid sign character %c" sign))))]
          [index {:exponent-digits digits :exponent-negative? exponent-negative?}])

        ;;sign is required but actual character does not match
        :else
        (throw (IllegalArgumentException. (format "Invalid exponent - expect sign character %c at index %d" sign start-index)))))
    [start-index {:exponent-digits "" :exponent-negative? false}]))

(defn parse-numeric-exponent [^String s start-index exponent-spec]
  (if (< start-index (.length s))
    (let [c (.charAt s start-index)]
      (if (= \E c)
        (if (= ::none exponent-spec)
          (throw (IllegalArgumentException. (format "Invalid exponent at index %d - format forbids exponent" start-index)))
          (let [[index {:keys [exponent-digits] :as exp}] (parse-numeric-exponent-part s (inc start-index) exponent-spec)]
            (validate-digit-count exponent-digits "exponent" exponent-spec)
            [index exp]))
        [start-index {:exponent-digits "" :exponent-negative? false}]))
    [start-index {:exponent-digits "" :exponent-negative? false}]))

(defn parse-numeric-suffix [^String s start-index suffix-spec]
  (let [remaining (.substring s start-index)]
    (if (= ::optional-modifier suffix-spec)
      (case (.length remaining)
        0 {:suffix remaining :suffix-modifier nil}
        1 (let [c (.charAt remaining 0)]
            (if (is-modifier-char? c)
              {:suffix remaining :suffix-modifier c}
              (throw (IllegalArgumentException. (format "Invalid suffix %s - expected modifier character" remaining)))))
        (throw (IllegalArgumentException. (format "Invalid suffix %s - expected modifier character" remaining))))

      (let [{expected-suffix :suffix modifier :modifier} suffix-spec]
        (if (= expected-suffix remaining)
          {:suffix remaining :suffix-modifier modifier}
          (throw (IllegalArgumentException. (format "Invalid suffix - expected '%s' got '%s'" expected-suffix remaining))))))))

(defn parse-number [^String s {:keys [prefix integer decimal exponent suffix modifier group-char decimal-char] :as fmt}]
  (let [[index prefix-def] (parse-numeric-prefix s prefix)
        [index integer-def] (parse-numeric-integer s index group-char integer)
        [index decimal-def] (parse-numeric-decimal s index decimal-char group-char decimal)
        [index exponent-def] (parse-numeric-exponent s index exponent)
        {:keys [suffix-modifier] :as suffix-data} (parse-numeric-suffix s index suffix)
        modifier (or suffix-modifier modifier)]
    (assoc (merge prefix-def integer-def decimal-def exponent-def) :modifier modifier)))

(defn parse-number-with-format [format-string numeric-string]
  (parse-number numeric-string (parse-number-format format-string)))