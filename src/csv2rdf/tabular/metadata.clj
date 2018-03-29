(ns csv2rdf.tabular.metadata
  (:require [csv2rdf.http :as http]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [csv2rdf.util :as util]
            [csv2rdf.uri-template :as template]
            [clojure.string :as string]
            [csv2rdf.tabular.csv :as csv]
            [csv2rdf.source :as source]
            [csv2rdf.metadata :as meta]
            [csv2rdf.validation :as v])
  (:import [java.io File FileNotFoundException]
           [java.net URI]))

;;TODO: move into http namespace?
(defn ^{:table-spec "5.3"} is-not-found-response?
  "Indicates whether the response map represents a 'not found' response."
  [{:keys [status]}]
  (and (>= status 400) (<= status 600)))

(def ^{:table-spec "5.2"} metadata-link-header-content-types
  #{"application/csvm+json" "application/ld+json" "application/json"})

(defn ^{:table-spec "5.2"} is-metadata-link?
  "Indicates whether the given CSV file response header is a valid Link header for the
   associated metadata file."
  [{:keys [rel type]}]
  (and (http/relation-type= "describedby" rel)
       (contains? metadata-link-header-content-types type)))

(defn ^{:table-spec "5.2"} get-metadata-link [csv-response]
  (let [links (http/find-links csv-response)]
    (last (filter is-metadata-link? links))))

(defn ^{:table-spec "5.2"} get-metadata-link-uri [csv-uri csv-response]
  (if-let [link-header (get-metadata-link csv-response)]
    ;;TODO: URIs must be normalised (section 6.3)
    (.resolve csv-uri (::http/link-uri link-header))))

;;TODO: validate JSON represents a valid metadata file? or perform at a higher level?
(defn read-metadata-file [file]
  (try
    (with-open [r (io/reader file)]
      (json/read r))
    (catch FileNotFoundException ex
      (throw (ex-info
               (format "Error reading metadata file %s: not found" (.getAbsolutePath file))
               {:type ::resolve-metadata-error
                :metadata-file file})))))

(defn try-get-linked-metadata
  "Tries to fetch the linked metadata from the given URI. Returns nil on any errors or if the request fails."
  [metadata-uri]
  (let [{:keys [body] :as response} (http/get-uri metadata-uri)]
    (if (http/is-ok-response? response)
      ;;TODO: handle non-string body types?
      (json/read-str body))))

(defn ^{:table-spec "5.2"} linked-metadata-references-data-file? [csv-uri {:strs [tables] :as metadata}]
  ;;from the spec: If the metadata file found at this location does not explicitly include a reference
  ;; to the requested tabular data file then it MUST be ignored
  (let [table-urls (->> tables
                        (map (fn [{:strs [url]}]
                               (util/ignore-exceptions (URI. url))))
                        (remove nil?)
                        (into #{}))]
    (contains? table-urls csv-uri)))

(defn ^{:table-spec "5.2"} try-resolve-linked-metadata [csv-url metadata-uri]
  (if (some? metadata-uri)
    (if-let [metadata (try-get-linked-metadata metadata-uri)]
      (if (linked-metadata-references-data-file? csv-url metadata)
        metadata))))

(def ^{:table-spec "5.3"} well-known-site-wide-configuration-uri (URI. "/.well-known/csvm"))

(defn ^{:template-spec "5.3"} parse-response-location-templates [body]
  ;;TODO: handle non-string body types?
  ;;NOTE: specification states "This file MUST contain a URI template, as defined by [URI-TEMPLATE], on each line"
  ;;this is trivially true if there are no lines (i.e. body is empty) but split-lines returns a singleton sequence
  ;;containing the empty string on an empty input.
  (if (.isEmpty body)
    []
    (string/split-lines body)))

(defn ^{:table-spec "5.3"} try-get-location-templates [uri]
  (let [{:keys [body] :as response} (http/get-uri uri)]
    (if-not (is-not-found-response? response)
      (parse-response-location-templates body))))

(defn try-get-site-wide-configuration-templates [csv-uri]
  (let [config-uri (.resolve csv-uri well-known-site-wide-configuration-uri)]
    (try-get-location-templates config-uri)))

(def ^{:table-spec "5.3"} default-location-templates
  ["{+url}-metadata.json"
   "csv-metadata.json"])

(defn ^{:table-spec "5.3"} get-site-wide-configuration-templates [csv-uri]
  (or (try-get-site-wide-configuration-templates csv-uri)
      default-location-templates))

(defn ^{:table-spec "5.3"} try-expand-location-template [csv-uri template-string]
  (if-let [template (template/try-parse-template template-string)]
    (template/expand-template template {:url (util/remove-fragment csv-uri)})))

(defn ^{:table-spec "5.3"} try-resolve-location-template-metadata [csv-uri template-string]
  (if-let [expanded-uri (try-expand-location-template csv-uri template-string)]
    (let [metadata-uri (.resolve csv-uri expanded-uri)]
      (try-resolve-linked-metadata csv-uri metadata-uri))))

(defn try-resolve-template-uri [csv-uri template]
  (if-let [template-uri (try-expand-location-template csv-uri template)]
    (.resolve csv-uri template-uri)))

(defn try-resolve-template-metadata [csv-uri template-uris]
  (if (seq template-uris)
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

(defprotocol CSVLocator
  (locate-csv [this]))

(extend-protocol CSVLocator
  File
  (locate-csv [file]
    {::csv-link nil
     ::csv-uri nil                                          ;;TODO: use file URI?
     ::csv-source file})

  URI
  (locate-csv [uri]
    ;;TODO: handle exceptions
    (let [{:keys [status body] :as response} (http/get-uri uri)]
      (if (is-not-found-response? response)
        (throw (ex-info
                 (format "Error resolving CSV at URI %s: not found" uri)
                 {:type ::resolve-csv-error
                  :csv-uri uri
                  :status status}))
        {::csv-link (get-metadata-link-uri uri response)
         ::csv-uri uri
         ::csv-source body}))))

(defmulti get-metadata (fn [csv-source] (keyword (.getScheme (source/->uri csv-source)))))

(defmethod get-metadata :file [csv-source]
  (csv/extract-embedded-metadata csv-source))

(defmethod get-metadata :http [csv-source]
  ;;TODO: handle exceptions
  (let [csv-uri (source/->uri csv-source)
        {:keys [status body] :as response} (http/get-uri csv-uri)]
    (if (is-not-found-response? response)
      (throw (ex-info
               (format "Error resolving CSV at URI %s: not found" csv-uri)
               {:type ::resolve-csv-error
                :csv-uri csv-uri
                :status status}))
      (let [metadata-link (get-metadata-link-uri csv-uri response)]
        (if-let [metadata-doc (resolve-associated-metadata csv-uri metadata-link)]
          (let [metadata-validation (meta/parse-metadata-json csv-uri metadata-doc)]
            (v/get-value metadata-validation "Invalid metadata document"))
          (csv/extract-embedded-metadata (source/io-source csv-uri body)))))))
