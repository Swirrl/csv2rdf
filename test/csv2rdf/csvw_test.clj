(ns csv2rdf.csvw-test
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all :as test]
            [csv2rdf.csvw :as csvw]
            [grafter.rdf :as rdf]
            [grafter.rdf4j.io :as rdf4j-io]
            [grafter.rdf4j.formats :as formats]
            [csv2rdf.http :as http])
  (:import [java.net URI URL]
           [org.eclipse.rdf4j.model.util Models]))

(def test-data-dir (io/file "csvw_data"))
(def test-base-uri (URI. "http://www.w3.org/2013/csvw/tests/"))
(def manifest (io/file test-data-dir "manifest.csv"))

(defn test-path->file [path]
  (io/file test-data-dir path))

(defn test-path->uri [path]
  (.resolve test-base-uri path))

(defn get-file-extension [file]
  (let [filename (.getName file)
        idx (.lastIndexOf filename ".")]
    (if (= -1 idx)
      ""
      (.substring filename (inc idx)))))

(defn read-manifest [manifest-file]
  (with-open [r (io/reader manifest-file)]
    (doall (csv/read-csv r))))

(defn is-nil-or-whitespace? [s]
  (or (nil? s)
      (.. s trim isEmpty)))

(defn strip-quotes [s]
  (if-let [match (re-find #"^\"(.*)\"$" s)]
    (second match)
    s))

(defn parse-extras [extras]
  (if (is-nil-or-whitespace? extras)
    {}
    (let [kvs (string/split extras #"\s+")
          kvs (map (fn [p]
                     (let [[k v & ignored] (string/split p #"=")
                           v (strip-quotes v)
                           v (if (.contains v ",") (string/split v #",") v)]
                       [(keyword k) v]))
                   kvs)]
      (into {} kvs))))

(defn ->coll [x]
  (if (coll? x) x [x]))

(defn parse-row
  "Parses a single row in manifest.csv from the CSVW test cases repository. See mk_manifest.rb in the test data repository
   for how each row is structured"
  [{:strs [test name comment approval rdf json validate action-metadata user-metadata link-metadata extra] :as row}]
  (let [extras (parse-extras extra)
        is-rdf-test? (not= "FALSE" rdf)
        is-json-test? (not= "FALSE" json)
        has-rdf-result? (not (contains? #{"FALSE" "negative"} rdf))
        has-json-result? (not (contains? #{"FALSE" "negative"} json))
        extras (if-not (= "FALSE" rdf) (assoc extras :rdf rdf) extras)
        extras (if-not (= "FALSE" json) (assoc extras :json json) extras)
        extras (if-not (= "FALSE" validate) (assoc extras :validation validate) extras)
        has-dir? (contains? extras :dir)
        extra-action? (contains? extras :action)
        m (if has-dir?
            (let [action (cond
                           extra-action? (:action extras)
                           (= "TRUE" action-metadata) (str test "/csv-metadata.json")
                           :else (str test "/action.csv"))]
              {:action     action
               :result-rdf (if has-rdf-result? (str test "/result.ttl"))
               :result-json (if has-json-result? (str test "/result.json"))
               :user-metadata (if (= "TRUE" user-metadata) (str test "/user-metadata.json"))
               :link-metadata (if (= "TRUE" link-metadata) (str test "/linked-metadata.json"))})
            (let [action (cond
                           extra-action? (:action extras)
                           (= "TRUE" action-metadata) (str test "-metadata.json")
                           :else (str test ".csv"))]
              {:action action
               :result-rdf (if has-rdf-result? (str test ".ttl"))
               :result-json (if has-json-result? (str test ".json"))
               :user-metadata (if (= "TRUE" user-metadata) (str test "-user-metadata.json"))
               :link-metadata (if (= "TRUE" link-metadata) (str test "-linked-metadata.json"))}))
        implicit (cond
                   (contains? extras :implicit)
                   (->coll (:implicit extras))

                   (and (not extra-action?)
                        (= "TRUE" action-metadata))
                   [(if has-dir?
                      (str test "/action.csv")
                      (str test ".csv"))]
                   :else [])
        implicit (->> implicit
                      (concat [(:user-metadata m)
                               (:link-metadata m)])
                      (remove nil?)
                      (vec))
        http-link (if-let [link-meta (:link-metadata m)]
                    (format "<%s>; rel=\"describedby\"; type=\"application/csvm+json\"" (.getName (io/file link-meta))))]
    (merge
      m
      {:id test
       :name name
       :comment comment
       :approval approval
       :extra extras
       :implicit implicit
       :http-link http-link})))

(defn parse-manifest
  ([] (parse-manifest manifest))
  ([manifest-file]
   (let [[header & rows] (read-manifest manifest-file)
         row-maps (map (fn [row] (zipmap header row)) rows)]
     (map parse-row row-maps))))

(defn file->request [file]
  (let [result-file (test-path->file file)
        content-type (if (= "csv" (get-file-extension result-file)) "text/csv" "application/csvm+json")]
    {:body    result-file
     :uri     (test-path->uri file)
     :headers {"Content-Type" content-type}}))

(defn minimal-mode? [test-row]
  (= "true" (get-in test-row [:extra :minimal])))

(defn get-mode [test-row]
  (if (minimal-mode? test-row) :minimal :standard))

(defn from-metadata-test-descriptor
  "Tests that start from a metadata file use it to locate the CSV file (which always exists?). Any additional files
   required by the test should appear in the list of implicit files"
  [{:keys [action implicit] :as test-row}]
  {:type :metadata
   :metadata-file (test-path->file action)
   :schema-path action
   :requests (mapv file->request implicit)})

(def well-known-metadata-file (io/resource "csvm"))

(def well-known-metadata-response
  {:uri (URI. "http://www.w3.org/.well-known/csvm")
   :body well-known-metadata-file
   :headers {}})

(defn has-well-known-metadata? [test-row]
  (.contains (:name test-row) "/.well-known/csvm"))

(defn from-csv-test-descriptor
  "Tests that start from the URI of the CSV file. These always contain a request for the CSV file itself, and may include
   one for the well-known metadata file and any implicit files."
  [{:keys [action implicit http-link user-metadata] :as test-row}]
  (let [action-file (test-path->file action)
        action-uri (test-path->uri action)
        action-headers {"Content-Type" "text/csv; charset=UTF-8"}
        action-headers (if (some? http-link)
                         (assoc action-headers "Link" http-link)
                         action-headers)
        action-request {:body action-file
                        :uri action-uri
                        :headers action-headers}
        metadata-requests (if (has-well-known-metadata? test-row)
                             [well-known-metadata-response]
                             [])]
    {:type        :csv
     :action-uri  action-uri
     :schema-path user-metadata
     :requests    (vec (cons action-request (concat (map file->request implicit) metadata-requests)))}))

(defn rdf-test-type [test-row]
  (let [type (some-> (get-in test-row [:extra :rdf]) keyword)]
    (#{:positive :negative :warning} type)))

(defn test-descriptor [{:keys [id action result-rdf] :as test-row}]
  (let [action-file (test-path->file action)
        action-ext (get-file-extension action-file)
        from-metadata? (= "json" action-ext)
        test-type (rdf-test-type test-row)]
    ;;ignore any non-rdf tests
    (if (some? test-type)
      (let [m (if from-metadata?
                (from-metadata-test-descriptor test-row)
                (from-csv-test-descriptor test-row))]
        (merge
          m
          {:id id
           :mode (get-mode test-row)
           :expect-errors? (= :negative test-type)
           :expect-warnings? (= :warning test-type)
           :result-file (some-> result-rdf (test-path->file))})))))

(defn get-tests []
  (->> (parse-manifest)
       (filter #(> (.length (:id %)) 0))  ;;test026 appears to be invalid? Has no name and missing result file
       (map test-descriptor)
       (remove nil?)))

(defmulti escape-read type)

(defmethod escape-read java.io.File [f]
  `(io/file ~(.getPath f)))

(defmethod escape-read java.net.URI [uri]
  `(URI. ~(str uri)))

(defmethod escape-read java.net.URL [url]
  `(URL. ~(str url)))

(defmethod escape-read clojure.lang.IPersistentMap [m]
  (into {} (map (fn [[k v]]
                  [(escape-read k) (escape-read v)])
                m)))

(defmethod escape-read :default [x] x)

(defn is-isomorphic? [expected-statements actual-statements]
  (Models/isomorphic
    (map rdf4j-io/->backend-type expected-statements)
    (map rdf4j-io/->backend-type actual-statements)))

(defn build-request-map [requests]
  (into {} (map (fn [{:keys [uri] :as m}]
                  [uri (dissoc m :uri)])
                requests)))

(defrecord TestMetadataLocator [csv-uri responses])

(defrecord TestHttpClient [request-map]
  http/HttpClient
  (http-get [_this uri]
    (if-let [response (get request-map uri)]
      (assoc response :status 200)
      {:status 404 :headers {} :body ""})))

(defn make-test [{:keys [action-uri metadata-file requests id expect-errors? expect-warnings? mode result-file] :as test}]
  (let [result-sym (gensym)
        request-map (build-request-map requests)]
    `(test/deftest ~(symbol id)
       (let [http-client# (->TestHttpClient ~(escape-read request-map))
             csv-uri# ~(escape-read action-uri)
             metadata-source# ~(escape-read metadata-file)
             ~result-sym (http/with-http-client http-client#
                           (csvw/csv->rdf csv-uri# metadata-source# {:mode ~mode}))]
         ~(if (some? result-file)
            `(let [expected-statements# (rdf/statements ~(escape-read result-file))]
               (test/is (= true (is-isomorphic? expected-statements# (:result ~result-sym))))))
         ~(if expect-warnings?
            `(test/is (pos? (count (:warnings ~result-sym))) "Expected warnings but none was found")
            `(test/is (= 0 (count (:warnings ~result-sym))) "Received warnings but none was expected"))
         ~(if expect-errors?
            `(test/is (pos? (count (:errors ~result-sym))) "Expected errors but none was found")
            `(test/is (= 0 (count (:errors ~result-sym))) "Received errors but none was expected"))))))

(doseq [t (get-tests)]
  (eval (make-test t)))
