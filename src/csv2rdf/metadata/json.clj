(ns csv2rdf.metadata.json)

(defn array? [x]
  (vector? x))

(def object? map?)

(defn get-json-type [x]
  (cond (array? x) :array
        (number? x) :number
        (string? x) :string
        (boolean? x) :boolean
        (object? x) :object
        :else (throw (ex-info (str "Unknown JSON type for type " (type x))
                              {:type ::json-type-error
                               :value x}))))

(defn get-json-type-name [x]
  (name (get-json-type x)))

