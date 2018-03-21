(ns csv2rdf.metadata.types
  (:require [csv2rdf.metadata.validator :refer [make-warning variant invalid array-of kvp kvps optional-key
                                                any map-of string invalid? chain try-parse-with]]
            [csv2rdf.metadata.context :refer [resolve-uri expand-uri-string append-path language-code-or-default]]
            [csv2rdf.validation :as v]
            [csv2rdf.metadata.json :refer [array? object?] :as mjson]
            [csv2rdf.uri-template :as template]
            [csv2rdf.xml.datatype :as xml-datatype]
            [clojure.string :as string]
            [csv2rdf.util :as util])
  (:import [java.util Locale$Builder IllformedLocaleException]
           [java.net URI URISyntaxException]))

(defn validate-language-code [context s]
  (try
    (do
      (.setLanguage (Locale$Builder.) s)
      ;;TODO: validate against known list of language codes?
      (v/pure s))
    (catch IllformedLocaleException _ex
      (make-warning context (str "Invalid language code: '" s "'") invalid))))

(defn language-code [context x]
  (if (string? x)
    (validate-language-code context x)
    (make-warning context (str "Expected language code, got " (mjson/get-json-type-name x)) invalid)))

(defn ^{:metadata-spec "6.6"} normalise-string-natural-language-property [context code]
  {(language-code-or-default context) [code]})

(defn ^{:metadata-spec "6.6"} normalise-array-natural-language-property [context codes]
  {(language-code-or-default context) codes})

(def ^{:metadata-spec "5.1.6"
       :doc "Validates a value in a natual language property defined as an object. Values can
        be either strings or string arrays, and returns an array if valid."} language-code-map-value
  (variant {:string (fn [_context s] (v/pure [s]))
            :array (array-of string)
            :default []}))

(defn ^{:metadata-spec "5.1.6"} natural-language
  "Parses a natural language property according to the specification. Normalises the result into a map of language
   codes to an array of associated values. Uses the default language if none specified in the context."
  [context x]
  (cond
    (string? x) (v/pure (normalise-string-natural-language-property context x))
    (array? x) (v/fmap #(normalise-array-natural-language-property context %)
                       ((array-of string) context x))
    (object? x) ((map-of language-code language-code-map-value) context x)
    :else (make-warning context "Expected string, array or object for natual language property" {(language-code-or-default context) []})))

(def default-uri (URI. ""))

(defn ^{:metadata-spec "6.3"} normalise-link-property
  "Normalises a link property URI by resolving it against the current base URI."
  [context uri]
  (v/pure (resolve-uri context uri)))

(defn ^{:metadata-spec "5.1.2"} parse-link-property
  "Converts link properties to URIs, or logs a warning if the URI is invalid. Link properties are resolved
   at a higher level."
  [context x]
  (if (string? x)
    (try
      (v/pure (URI. x))
      (catch URISyntaxException _ex
        (make-warning context (format "Link property '%s' cannot be parsed as a URI" x) default-uri)))
    (make-warning context (format "Invalid link property '%s': expected string containing URI, got %s" x (mjson/get-json-type-name x)) default-uri)))

(def link-property (chain parse-link-property normalise-link-property))

(def ^{:metadata-spec "5.1.3"} template-property
  (variant {:string (fn [context s]
                      (if-let [t (template/try-parse-template s)]
                        (v/pure t)
                        (make-warning context (str "Invalid URI template: '" s "'") invalid)))}))

(defn expand-compact-uri [context s]
  (try
    (v/pure (URI. (expand-uri-string s)))
    (catch Exception ex
      (make-warning context (str "Invalid compact URI: '" s "'") invalid))))

;;common propeties
;;TODO: move into separate namespace?
(defn ordinary-common-property-value-key [special-keys]
  (fn [context x]
    (if (and (string? x) (.startsWith x "@"))
      (make-warning context (str "Only keys " (string/join special-keys) " can start with an @") invalid)
      (v/pure x))))

(def ^{:metadata-spec "5.8.2"} common-property-value-type
  (variant {:string (fn [context s]
                      ;;TODO: normalise into data type record?
                      (if (xml-datatype/valid-type-name? s)
                        (v/pure s)
                        (expand-compact-uri context s)))}))

(defn ^{:metadata-spec "5.8"} validate-common-property-value [context v]
  (cond
    (array? v)
    ((array-of validate-common-property-value) context v)

    (object? v)
    (if (contains? v "@value")
      (let [allowed-keys (util/partition-keys v ["@value" "@type" "@language"])
            [allowed remaining] allowed-keys]
        (cond
          (seq remaining)
          (make-warning context (str "Common property values specifying @value must only contain keys " (string/join ", " allowed-keys)) invalid)

          (= 3 (count allowed))
          (make-warning context "Common property values specifying @value can only contain one of @type or @language" invalid)

          :else
          (let [special-keys (kvps [(optional-key "@language" language-code)
                                    (optional-key "@value" (variant {:string any :boolean any :number any}))
                                    (optional-key "@type" common-property-value-type)])]
            (special-keys context v))))
      (let [special-keys ["@id" "@type"]           ;;NOTE: @language not allowed unless @value specified
            [special remaining] (util/partition-keys v special-keys)
            special-validator (kvps [(optional-key "@id" any) ;;NOTE: @id will be expanded during normalisation
                                     (optional-key "@type" common-property-value-type)])
            remaining-validator (map-of (ordinary-common-property-value-key ["@id" "@type"]) validate-common-property-value)]
        (v/combine-with merge (special-validator context special) (remaining-validator context remaining))))

    :else (v/pure v)))

(def compact-uri (chain string expand-compact-uri))
(def common-property-key compact-uri)

(defn normalise-common-property-id [context id]
  (v/bind (fn [uri]
            (if (invalid? uri)
              (v/pure uri)
              (v/pure (resolve-uri context uri))))
          (compact-uri context id)))

(defn ^{:metadata-spec "6.1"} normalise-common-property-value [{:keys [language] :as context} v]
  (cond
    (array? v)
    (v/collect (map #(normalise-common-property-value context %) v))

    (string? v)
    (if (some? language)
      (v/pure {"@value" v "@language" language})
      (v/pure {"@value" v}))

    (and (object? v) (contains? v "@value"))
    (v/pure v)

    (object? v)
    (let [[special common-properties] (util/partition-keys v ["@id" "@type"])
          special-validator (kvps [(optional-key "@id" normalise-common-property-id)
                                   (optional-key "@type" any)])
          common-validator (map-of common-property-key normalise-common-property-value)]
      (v/combine-with merge (special-validator context special) (common-validator context common-properties)))

    :else (v/pure v)))

(def common-property-value (chain validate-common-property-value normalise-common-property-value))

(defn validate-id [context s]
  (if (.startsWith s "_:")
    (make-warning context "Ids cannot start with _:" invalid)
    ((try-parse-with #(URI. %)) context s)))

(def ^{:doc "An id is a link property whose value cannot begin with _:"} id
  (variant {:string validate-id}))
