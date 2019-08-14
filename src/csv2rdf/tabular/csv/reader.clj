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

(defn- skip-leading-chars
  "Returns the first character index after the specified start index within the string s which does not
   contain the character to-skip. The returned index will be one past the end of the string if s ends
   without a different character being encountered."
  [^String s index ^Character to-skip]
  (if (and (< index (.length s))
           (= to-skip (.charAt s index)))
    (recur s (inc index) to-skip)
    index))

(defn parse-cell-double-quote
  "Parses a tabular cell within a file where the escape and quote
  character are the same, starting from a given index in a source row
  string. Returns a map containing the raw cell contents and the
  string index following the cell within the source string. The index
  will be one past the end of the string if the cell is the last one
  within the row."
  [^String row-content source-row-number start-index {:keys [^Character quoteChar ^Character delimiter] :as options}]
  ;;NOTE: The specification requires that when the quote and escape character are the same (i.e. a double-quote
  ;;is used to escape quote characters), double quotes are eagerly consumed and interpreted as an escaped quote.
  ;;This is a problem at the start of the cell since an opening quote followed by an escaped quote will be interpreted
  ;;as an escaped quote followed by an (illegal) cell quote.
  ;;The CSV RFC (https://tools.ietf.org/html/rfc4180) only allows escaped quotes within quoted cell values, but the csv2rdf
  ;;specification allows escaped quotes anywhere. This is ambiguous since e.g. """"" could be either two escaped quotes or
  ;;a single escaped quote within a quoted cell. We want to support CSV parsers that follow RFC4180 so we will resolve
  ;;the ambiguity by:
  ;;1. Read n leading quotes in the cell
  ;;2. If there are an even number then treat this as (n / 2) escaped quote characters
  ;;3. If there are an odd number then treat this as an opening quote followed by ((n - 1) / 2) escaped quote characters
  ;;At other points in the cell, double quotes are treated as escaped quotes.
  (let [non-quote-index (skip-leading-chars row-content start-index quoteChar)
        quote-char-count (- non-quote-index start-index)
        quoted (odd? quote-char-count)
        quote-count (quot quote-char-count 2)
        quote-prefix (String. (char-array quote-count quoteChar))]
    ;;resume parsing from the first non-quote index
    (loop [idx non-quote-index
           sb (StringBuilder. quote-prefix)]
      (if (< idx (.length row-content))
        (let [c (.charAt row-content idx)
              next-index (inc idx)
              next-char (maybe-char-at row-content next-index)]
          (cond
            (= quoteChar c)
            (cond
              ;;3.1
              (= quoteChar next-char)
              (recur (inc next-index) (.append sb next-char))

              ;;3.3.2 - peek following char if it exists, and raise an error if not the delimiter
              quoted
              (cond
                ;;end of string so ok
                (nil? next-char)
                {:cell (.toString sb) :next-index next-index}

                ;;followed by delimiter so ok
                ;;NOTE: delimiter is not consumed
                (= delimiter next-char)
                {:cell (.toString sb) :next-index next-index}

                ;;not end of string and not the delimiter so invalid                
                :else
                (throw (IllegalArgumentException. (format "Expected delimiter following quoted cell value at row %d index %d but got '%s'"
                                                          source-row-number
                                                          next-index
                                                          next-char))))

              ;;3.3.1 - opening quote should be at start of cell
              ;;NOTE: leading quotes are consumed at the start of the cell so at least one non-quote character
              ;;must have been consumed before reaching the current quote
              :else
              (throw (IllegalArgumentException. (format "Opening quote must be at start of cell value at row %d index %d" source-row-number idx))))          
            
            (= delimiter c)
            (if quoted
              ;;3.4.1
              (recur (inc idx) (.append sb c))

              ;;3.4.2
              ;;TODO: return current char?
              ;;row parser will need to look it up again!
              ;;NOTE: delimiter is not consumed
              {:cell (.toString sb) :next-index idx})

            :else
            (recur (inc idx) (.append sb c))))
        (if quoted
          (throw (IllegalArgumentException. (format "Missing closing quote on row %d for cell starting at index %d" source-row-number start-index)))
          {:cell (.toString sb) :next-index idx})))))

(defn- throw-unclosed-quote [source-row-number cell-start-index]
  (throw (IllegalArgumentException. (format "Missing closing quote on row %d for cell starting at index %d" source-row-number cell-start-index))))

(defn parse-cell-escape
  "Parses a tabular cell within a file where the escape and quote
  character are different, starting from a given index in a source row
  string. Returns a map containing the raw cell contents and the
  string index following the cell within the source string. The index
  will be one past the end of the string if the cell is the last one
  within the row."
  [^String row-content source-row-number start-index {:keys [^Character escapeChar ^Character quoteChar ^Character delimiter trim-mode] :as options}]
  (loop [idx start-index
         quoted false
         sb (StringBuilder.)]
    (if (< idx (.length row-content))
      (let [c (.charAt row-content idx)
            next-index (inc idx)
            next-char (maybe-char-at row-content next-index)]        
        (cond
          ;;3.2
          (= escapeChar c)
          (if (nil? next-char)
            (throw (IllegalArgumentException. (format "Dangling escape character at row %d index %d" source-row-number idx)))
            (recur (inc next-index) quoted (.append sb next-char)))

          (= quoteChar c)
          (if quoted
            ;;3.3.2 - peek following char if it exists, and raise an error if not the delimiter
            (cond
              ;;end of string so ok
              (nil? next-char)
              {:cell (.toString sb) :next-index next-index}

              ;;following character is delimiter so ok
              ;;NOTE: delimiter is not consumed
              (= delimiter next-char)
              {:cell (.toString sb) :next-index next-index}

              :else
              (throw (IllegalArgumentException. (format "Expected delimiter following quoted cell value at row %d index %d" source-row-number (inc idx)))))

            ;;3.3.1 - opening quote should be at start of cell
            (if (pos? (.length sb))
              (throw (IllegalArgumentException. (format "Opening quote must be at start of cell value at row %d index %d" source-row-number idx)))
              (recur (inc idx) true sb)))

          (= delimiter c)
          (if quoted
            ;;3.4.1
            (recur (inc idx) quoted (.append sb c))

            ;;3.4.2
            ;;NOTE: delimiter is not consumed
            {:cell (.toString sb) :next-index idx})

          :else
          (recur (inc idx) quoted (.append sb c))))

      (if quoted
        (throw-unclosed-quote source-row-number start-index)
        {:cell (.toString sb) :next-index idx}))))

(defn parse-row-cells
  "Parses the cells within a single tabular row and trims them according
  to the dialect trim mode."
  [^String row-content source-row-number {:keys [^Character escapeChar ^Character quoteChar ^Character delimiter trim-mode] :as options}]
  (if (zero? (.length row-content))
      []
      (let [;;NOTE: cells are parsed slightly differently depending on
            ;;whether the quote and escape characters are the same or
            ;;different. Each parser parses the next cell contents
            ;;from the specified starting position within the string
            ;;and returns a map containing the parsed cell contents
            ;;and the index within the string to continue
            ;;parsing. This should always point to the delimiter
            ;;character or be one passed the end of the string.
            cell-parser (if (= escapeChar quoteChar)
                          parse-cell-double-quote
                          parse-cell-escape)]        
        (loop [idx 0
               cells []]
          (let [{:keys [cell ^long next-index]} (cell-parser row-content source-row-number idx options)]
            ;;if there is any remaining input, next-index should refer to the delimiter
            ;;consume it and move to the start of the next cell
            ;;otherwise entire row has been parsed
            ;;TODO: move quoted cell followed by delimiter validation stuff here?
            (if (< next-index (.length row-content))
              (let [next-char (.charAt row-content next-index)]
                #_(assert (= delimiter next-char "Expected delimiter after parsed cell"))
                (recur (inc next-index) (conj cells (trim-cell cell trim-mode))))
              (conj cells (trim-cell cell trim-mode))))))))

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
              (parse-row-cells row-content source-row-num options))
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
