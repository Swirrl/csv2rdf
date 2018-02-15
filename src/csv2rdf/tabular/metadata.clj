(ns csv2rdf.tabular.metadata
  (:require [csv2rdf.http :as http]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [csv2rdf.util :as util]
            [csv2rdf.uri-template :as template])
  (:import [java.io File FileNotFoundException]
           [java.net URI]))

;;TODO: move into http namespace?
(defn is-not-found-response?
  "Indicates whether the response map represents a 'not found' response.

   SPEC Section 5.3:
   If no such file is located (i.e. the response results in a client error 4xx status code
   or a server error 5xx status code)"
  [{:keys [status]}]
  (and (>= status 400) (<= status 600)))

(def ^{:table-spec "5.2"} link-header-name "Link")

(def ^{:table-spec "5.2"} metadata-link-header-content-types
  #{"application/csvm+json" "application/ld+json" "application/json"})

(defn ^{:table-spec "5.2"} is-metadata-link-header?
  "Indicates whether the given CSV file response header is a valid Link header for the
   associated metadata file."
  [{:keys [rel type]}]
  (and (= "describedby" rel)
       (contains? metadata-link-header-content-types type)))

(defn ^{:table-spec "5.2"} get-link-header [headers]
  (last (filter is-metadata-link-header? (get headers link-header-name))))

(defn ^{:table-spec "5.2"} get-metadata-link-uri [csv-uri {:keys [headers] :as csv-response}]
  (if-let [link-header (get-link-header headers)]
    ;;TODO: handle exception if header value is invalid URI
    ;;TODO: URIs must be normalised (section 6.3)
    (.resolve csv-uri (::http/header-value link-header))))

(defprotocol MetadataSource
  (locate-metadata [this csv]))

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
  (try
    (let [{:keys [body] :as response} (http/get-uri metadata-uri)]
      (if (http/is-ok-response? response)
        (with-open [r (io/reader body)]
          (json/read r))))
    (catch Exception _ex nil)))

(defn ^{:table-spec "5.2"} linked-metadata-references-data-file? [csv-url metadata]
  ;;TODO: from the spec: If the metadata file found at this location does not explicitly include a reference to the requested tabular data file then it MUST be ignored
  true)

(defn ^{:table-spec "5.2"} try-resolve-linked-metadata [csv-url metadata-uri]
  (if (some? metadata-uri)
    (if-let [metadata (try-get-linked-metadata metadata-uri)]
      (if (linked-metadata-references-data-file? csv-url metadata)
        metadata))))

(def ^{:table-spec "5.3"} well-known-site-wide-configuration-uri (URI. "/.well-known/csvm"))

(defn ^{:table-spec "5.3"} try-get-location-templates [uri]
  (util/ignore-exceptions
    (let [{:keys [body] :as response} (http/get-uri uri)]
      (if-not (is-not-found-response? response)
        (util/read-lines body)))))

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

(defn try-locate-site-wide-configurations-metadata [csv-uri]
  (let [templates (get-site-wide-configuration-templates csv-uri)]
    (first (filter some? (map #(try-resolve-location-template-metadata csv-uri %) templates)))))

(defn resolve-associated-metadata [csv-uri csv-link]
  (or
    (try-resolve-linked-metadata csv-uri csv-link)
    (try-locate-site-wide-configurations-metadata csv-uri)))

(extend-protocol MetadataSource
  File
  (locate-metadata [file csv]
    {::csv-source (::csv-source csv)
     ::metadata (read-metadata-file file)})

  nil
  (locate-metadata [_ csv]
    {::csv-source (::csv-source csv)
     ::metadata (resolve-associated-metadata (::csv-uri csv) (::csv-link csv))}))

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

(defn resolve-inputs [csv-source metadata-source]
  (let [csv (locate-csv csv-source)]
    (locate-metadata metadata-source csv)))
