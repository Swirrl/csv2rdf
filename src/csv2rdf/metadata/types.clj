(ns csv2rdf.metadata.types
  (:require [csv2rdf.metadata.validator :refer [make-warning invalid chain variant]]
            [csv2rdf.metadata.context :refer [resolve-uri]]
            [csv2rdf.validation :as v]
            [csv2rdf.metadata.json :as mjson]
            [csv2rdf.uri-template :as template])
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
