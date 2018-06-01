(ns csv2rdf.tabular.csv.reader
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [csv2rdf.util :as util]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.source :as source])
  (:import [java.io PushbackReader InputStream InputStreamReader]
           [java.util Iterator]))

(defn line-terminators->trie [terminator-strings]
  (reduce (fn [acc terminator-string]
            (assoc-in acc terminator-string ::done))
          {}
          terminator-strings))

(defn try-read-line-terminator [^PushbackReader reader trie]
  (let [sb (StringBuilder.)]
    (loop [root trie]
      (if (= ::done root)
        [true (.toString sb)]
        (let [ci (.read reader)]
          (if (= -1 ci)
            [false (.toString sb)]
            (let [c (char ci)]
              (if-let [child (get trie c)]
                (do
                  (.append sb c)
                  (recur child))
                (do
                  (.unread reader ci)
                  [false (.toString sb)])))))))))

(defn read-quoted-value [^PushbackReader reader ^Character escape-char ^Character quote-char]
  (loop [sb (.append (StringBuilder.) quote-char)]
    (let [ci (.read reader)]
      (if (= -1 ci)
        (throw (IllegalArgumentException. "Encountered unclosed quote"))
        (let [c (char ci)]
          (cond
            (= escape-char c)
            (let [ni (.read reader)]
              ;;if the escape and quote chars are the same, this could be an escape sequence or the closing quote
              ;;if they are different, it must be an escape sequence so ni must be a character not EOF
              (if (= -1 ni)
                (if (= escape-char quote-char)
                  (.toString (.append sb c))
                  (throw (IllegalArgumentException. "Dangling escape character at end of stream")))
                (let [n (char ni)]
                  (if (and (= escape-char quote-char) (not= quote-char n))
                    (do
                      ;;encountered closing quote followed by next character
                      ;;push following character back in the stream and return the quoted value
                      (.unread reader ni)
                      (.toString (.append sb c)))
                    (recur (.. sb (append c) (append n)))))))

            (= quote-char c)
            (.toString (.append sb c))

            :else
            (recur (.append sb c))))))))

(defn read-next-row-content [^PushbackReader reader ^Character escape-char ^Character quote-char line-terminator-trie]
  (loop [sb (StringBuilder.)]
    (let [ci (.read reader)]
      (if (= -1 ci)
        (.toString sb)
        (let [c (char ci)]
          (cond
            (= escape-char c)
            (let [ni (.read reader)]
              (if (= -1 ni)
                (throw (IllegalArgumentException. "Dangling escape character at end of stream"))
                (let [n (char ni)]
                  ;;if escape char is same as quote char then:
                  ;;[escape quote] is treated as an escaped quote character
                  ;;[escape other] is treated as the start of a quoted string
                  ;;if escape and quote char are different then [escape other] is treated as an escaped character
                  (if (and (= escape-char quote-char) (not= n quote-char))
                    (do
                      ;;push-back ni as the first character of the quoted string
                      (.unread reader ni)
                      (let [quoted (read-quoted-value reader escape-char quote-char)]
                        (recur (.append sb quoted))))
                    (recur (.. sb (append c) (append n)))))))

            (= quote-char c)
            (let [quoted (read-quoted-value reader escape-char quote-char)]
              (recur (.append sb quoted)))

            (contains? line-terminator-trie c)
            (let [[is-newline? s] (try-read-line-terminator reader (get line-terminator-trie c))]
              ;;if newline is encountered, return row content, otherwise append all non-matching characters and continue
              (if is-newline?
                (.toString sb)
                (recur (.. sb (append c) (append s)))))

            :else
            (recur (.append sb c))))))))

(defn row-content-iterator [^PushbackReader reader {:keys [quoteChar escapeChar lineTerminators]}]
  (let [line-terminators-trie (line-terminators->trie lineTerminators)]
    (reify Iterator
      (hasNext [_this]
        (let [ci (.read reader)]
          (if (= -1 ci)
            false
            (do
              (.unread reader ci)
              true))))
      (next [_this]
        (read-next-row-content reader escapeChar quoteChar line-terminators-trie))

      (remove [_this]
        (throw (UnsupportedOperationException. "Cannot .remove from row content iterator"))))))

(defn row-contents-seq [^PushbackReader reader options]
  (iterator-seq (util/owning-iterator reader #(row-content-iterator % options))))

(defn maybe-char-at [^String s index]
  (if (and (>= index 0) (< index (.length s)))
    (.charAt s index)))

(defn trim-cell [value trim-mode]
  (case trim-mode
    :all (string/trim value)
    :start (string/triml value)
    :end (string/trimr value)
    value))

(defn ^{:tabular-spec "8"} parse-row-cells [^String row-content {:keys [^Character escapeChar ^Character quoteChar ^Character delimiter trim-mode] :as options}]
  (loop [idx 0
         cells []
         quoted false
         sb (StringBuilder.)]
    (if (< idx (.length row-content))
      (let [c (.charAt row-content idx)
            next-index (inc idx)
            next-char (maybe-char-at row-content next-index)]
        (cond
          ;;3.1
          (and (= escapeChar c) (= quoteChar next-char))
          (recur (inc next-index) cells quoted (.append sb next-char))

          ;;3.2
          (and (= escapeChar c) (not= escapeChar quoteChar))
          (if (nil? next-char)
            (throw (IllegalArgumentException. (format "Dangling escape character at index %d" idx)))
            (recur (inc next-index) cells quoted (.append sb next-char)))

          (= quoteChar c)
          (if quoted
            ;;3.3.2 - peek following char if it exists, and raise an error if not the delimiter
            (if (and (< (inc idx) (.length row-content))
                     (not= delimiter (.charAt row-content (inc idx))))
              (throw (IllegalArgumentException. (format "Expected delimiter following quoted cell value at index %d" (inc idx))))
              (recur (inc idx) cells false sb))

            ;;3.3.1 - opening quote should be at start of cell
            (if (pos? (.length sb))
              (throw (IllegalArgumentException. (format "Opening quote must be at start of cell value at index %d" idx)))
              (recur (inc idx) cells true sb)))

          (= delimiter c)
          (if quoted
            ;;3.4.1
            (recur (inc idx) cells quoted (.append sb c))

            ;;3.4.2
            (recur (inc idx) (conj cells (trim-cell (.toString sb) trim-mode)) quoted (StringBuilder.)))

          :else
          (recur (inc idx) cells quoted (.append sb c))))
      (conj cells (trim-cell (.toString sb) trim-mode)))))

(s/def ::source-row-number (s/and integer? pos?))
(s/def ::cells (s/coll-of string? :kind vector? :into []))
(s/def ::content string?)
(s/def ::comment string?)
(s/def ::type #{:comment :data})

(defn is-comment-row? [{:keys [type]}]
  (= :comment type))

(defn make-row [^String row-content source-row-num {:keys [commentPrefix] :as options}]
  (let [is-comment? (and (some? commentPrefix)
                         (.startsWith row-content (str commentPrefix)))]
    {:source-row-number source-row-num
     :content row-content
     :comment (if is-comment?
                (.substring row-content 1))
     :cells (if-not is-comment?
              (parse-row-cells row-content options))
     :type (if is-comment? :comment :data)}))

(defn make-tabular-reader [^InputStream stream {:keys [^String encoding] :as options}]
  (PushbackReader. (InputStreamReader. stream encoding)))

(defn make-row-contents-seq [input-stream options]
  (row-contents-seq (make-tabular-reader input-stream options) options))

(defn read-tabular-source [csv-source dialect]
  (let [{:keys [headers ^InputStream stream]} (source/request-input-stream csv-source)]
    (try
      (let [options (dialect/resolve-options dialect headers)]
        {:rows (make-row-contents-seq stream options)
         :options options})
      (catch Exception ex
        (.close stream)
        (throw ex)))))

(defn row-contents->rows
  "Converts a sequence of row contents into a sequence of rows"
  [row-contents options]
  (map-indexed (fn [idx row-content]
                 (make-row row-content (inc idx) options))
               row-contents))

(defn make-row-seq [input-stream options]
  (row-contents->rows (make-row-contents-seq input-stream options) options))

(defn read-rows
  "Returns a lazy sequence of CSV rows from the underlying reader. The row records contain
   the source row number (reader is initially assumed to be positioned on row 1), the parsed
   content and cells along with any comment. Rows are classified as comments or data rows containing
   cell data. Cell data values are trimmed according to the trim-mode specified by the options."
  [csv-source dialect]
  (let [{:keys [rows options]} (read-tabular-source csv-source dialect)]
    (row-contents->rows rows options)))
