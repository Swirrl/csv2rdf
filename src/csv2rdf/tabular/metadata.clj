(ns csv2rdf.tabular.metadata
  (:require [csv2rdf.http :as http]
            [csv2rdf.util :as util]
            [csv2rdf.uri-template :as template]
            [clojure.string :as string]
            [csv2rdf.tabular.csv :as csv]
            [csv2rdf.source :as source]
            [csv2rdf.metadata :as meta]
            [clojure.java.io :as io]
            [csv2rdf.metadata.table-group :as table-group]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.logging :as log]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.tabular.csv.reader :as reader])
  (:import [java.net URI]
           (java.io File InputStream)))

(def ^{:tabular-spec "5.2"} metadata-link-header-content-types
  #{"application/csvm+json" "application/ld+json" "application/json"})

(defn ^{:tabular-spec "5.2"} is-metadata-link?
  "Indicates whether the given CSV file response header is a valid Link header for the
   associated metadata file."
  [{:keys [rel type]}]
  (and (http/relation-type= "describedby" rel)
       (contains? metadata-link-header-content-types type)))

(defn ^{:tabular-spec "5.2"} get-metadata-link [csv-headers]
  (let [links (http/find-links csv-headers)]
    (last (filter is-metadata-link? links))))

(defn ^{:tabular-spec "5.2"} get-metadata-link-uri [^URI csv-uri csv-headers]
  (when-let [link-header (get-metadata-link csv-headers)]
    (let [^URI link-uri (::http/link-uri link-header)]
      (.resolve csv-uri link-uri))))

(defn try-get-linked-metadata
  "Tries to fetch the linked metadata from the given URI. Returns nil on any errors or if the request fails."
  [metadata-uri]
  (let [{:keys [body] :as response} (http/get-uri metadata-uri)]
    (when (http/is-ok-response? response)
      (source/get-json body))))


(defn ^{:tabular-spec "5.2"} linked-metadata-references-data-file? [csv-uri ^URI metadata-uri metadata-doc]
  ;;from the spec: If the metadata file found at this location does not explicitly include a reference
  ;; to the requested tabular data file then it MUST be ignored
  (let [tables (cond
                 (table-group/looks-like-table-group-json? metadata-doc)
                 (get metadata-doc "tables")

                 (table/looks-like-table-json? metadata-doc)
                 [metadata-doc]

                 :else [])
        table-uris (->> tables
                        (map (fn [{:strs [url]}]
                               (if-let [^URI tabular-uri (util/ignore-exceptions (URI. url))]
                                 (util/normalise-uri (.resolve metadata-uri tabular-uri)))))
                        (remove nil?)
                        (into #{}))
        has-reference? (contains? table-uris csv-uri)]
    (when-not has-reference?
      (log/log-warning (format "Metadata document at URI %s does not contain a reference to tabular file at URI %s" metadata-uri csv-uri)))
    has-reference?))

(defn ^{:tabular-spec "5.2"} try-resolve-linked-metadata [csv-uri metadata-uri]
  (when (some? metadata-uri)
    (when-let [metadata (try-get-linked-metadata metadata-uri)]
      (when (linked-metadata-references-data-file? csv-uri metadata-uri metadata)
        metadata))))

(def ^{:tabular-spec "5.3"} ^URI well-known-site-wide-configuration-uri (URI. "/.well-known/csvm"))

(defn ^{:template-spec "5.3"} parse-response-location-templates [^String body]
  ;;NOTE: specification states "This file MUST contain a URI template, as defined by [URI-TEMPLATE], on each line"
  ;;this is trivially true if there are no lines (i.e. body is empty) but split-lines returns a singleton sequence
  ;;containing the empty string on an empty input.
  (if (string/blank? body)
    []
    (string/split-lines body)))

(defn ^{:tabular-spec "5.3"} try-get-location-templates [uri]
  (let [{:keys [body] :as response}
        (try
          (http/get-uri uri)
          (catch clojure.lang.ExceptionInfo exi
            (select-keys (ex-data exi)
                         [:status :body])))]
    (when-not (http/is-not-found-response? response)
      (parse-response-location-templates body))))

(defn try-get-site-wide-configuration-templates [^URI csv-uri]
  (let [config-uri (.resolve csv-uri well-known-site-wide-configuration-uri)]
    (try-get-location-templates config-uri)))

(def ^{:tabular-spec "5.3"} default-location-templates
  ["{+url}-metadata.json"
   "csv-metadata.json"])

(defn ^{:tabular-spec "5.3"} get-site-wide-configuration-templates [csv-uri]
  (or (try-get-site-wide-configuration-templates csv-uri)
      default-location-templates))

(defn ^{:tabular-spec "5.3"} ^URI try-expand-location-template [csv-uri template-string]
  (when-let [template (template/try-parse-template template-string)]
    (template/expand-template template {:url (util/remove-fragment csv-uri)})))

(defn try-resolve-template-uri [^URI csv-uri template]
  (when-let [template-uri (try-expand-location-template csv-uri template)]
    (.resolve csv-uri template-uri)))

(defn try-resolve-template-metadata [csv-uri template-uris]
  (when (seq template-uris)
    (if-let [metadata (try-resolve-linked-metadata csv-uri (first template-uris))]
      metadata
      (recur csv-uri (rest template-uris)))))

(defn resolve-template-metadata-uris [csv-uri uri-template-strings]
  (remove nil? (map #(try-resolve-template-uri csv-uri %) uri-template-strings)))

(defn try-locate-site-wide-configurations-metadata [csv-uri]
  (let [templates (get-site-wide-configuration-templates csv-uri)
        metadata-uris (resolve-template-metadata-uris csv-uri templates)]
    (try-resolve-template-metadata csv-uri metadata-uris)))

(defn resolve-associated-metadata [csv-uri csv-link]
  (or
    (try-resolve-linked-metadata csv-uri csv-link)
    (try-locate-site-wide-configurations-metadata csv-uri)))

(defn io-source->embedded-metadata [io-source]
  (let [options (dialect/get-default-options)
        rows (reader/make-row-seq (io/input-stream io-source) options)]
    (csv/rows->embedded-metadata (source/->uri io-source) options rows)))

(defmulti get-uri-metadata (fn [^URI uri] (keyword (.getScheme uri))))

(defmethod get-uri-metadata :file [uri]
  (io-source->embedded-metadata uri))

(defn- fetch-web [uri]
  (let [{:keys [headers ^InputStream stream]} (source/request-input-stream uri)
        metadata-link (get-metadata-link-uri uri headers)]
    (if-let [metadata-doc (resolve-associated-metadata uri metadata-link)]
      (do
        (.close stream)
        (meta/parse-metadata-json uri metadata-doc))
      (let [dialect (dialect/get-default-dialect headers)
            options (dialect/dialect->options dialect)
            rows (reader/make-row-seq stream options)]
        (csv/rows->embedded-metadata uri options rows)))))

(defmethod get-uri-metadata :http [uri]
  (fetch-web uri))

(defmethod get-uri-metadata :https [uri]
  (fetch-web uri))

(defprotocol MetadataLocator
  (get-metadata [tabular-source]))

(extend-protocol MetadataLocator
  File
  (get-metadata [f] (io-source->embedded-metadata f))

  URI
  (get-metadata [uri] (get-uri-metadata uri)))
