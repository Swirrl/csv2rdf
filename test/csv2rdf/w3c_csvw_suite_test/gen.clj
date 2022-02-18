(ns csv2rdf.w3c-csvw-suite-test.gen
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [csv2rdf.http :as http]
            [csv2rdf.w3c-csvw-suite-test.impl :refer :all]
            [clojure.pprint :as pp])
  (:import [java.io File]
           [java.net URI URL]))

(def test-data-dir (io/file "w3c-csvw/tests"))
(def test-base-uri (URI. "http://www.w3.org/2013/csvw/tests/"))
(def manifest (io/file test-data-dir "manifest.csv"))

(defn test-path->file [path]
  ;;NOTE: some test path contain URI query fragments
  ;;extract just the path from these before resolving against the base path
  (let [path-uri (URI. path)]
    (io/file test-data-dir (.getPath path-uri))))

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

(defn is-nil-or-whitespace? [^String s]
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
  (let [metadata-file (test-path->file action)
        metadata-uri (test-path->uri action)
        metadata-request {:uri     metadata-uri
                          :body    metadata-file
                          :headers {}}]
    {:type          :metadata
     :csv-uri       nil
     :action-uri metadata-uri
     :metadata-uri metadata-uri
     :metadata-file (test-path->file action)
     :schema-path   action
     :requests      (vec (cons metadata-request (map file->request implicit)))}))

(def well-known-metadata-file (slurp (io/resource "csvm")))

(def well-known-metadata-response
  {:uri (URI. "http://www.w3.org/.well-known/csvm")
   :body well-known-metadata-file
   :headers {}})

(defn has-well-known-metadata? [test-row]
  (.contains (:name test-row) "/.well-known/csvm"))

(defn from-csv-test-descriptor
  "Tests where the 'action' is a CSV file. These always contain a request for the CSV file itself, and may include
   one for the well-known metadata file and any implicit files. If user-metadata is specified, processing will begin
   from the metadata rather than CSV file."
  [{:keys [action implicit http-link user-metadata] :as test-row}]
  (let [csv-file (test-path->file action)
        csv-uri (test-path->uri action)
        metadata-uri (some-> user-metadata test-path->uri)
        action-headers {"Content-Type" "text/csv; charset=UTF-8"}
        action-headers (if (some? http-link)
                         (assoc action-headers "Link" http-link)
                         action-headers)
        action-request {:body csv-file
                        :uri csv-uri
                        :headers action-headers}
        ;;NOTE: user metadata request expected to always be included in the implicit file list
        metadata-requests (if (has-well-known-metadata? test-row)
                            [well-known-metadata-response]
                            [])]
    {:type        :csv
     :action-uri  csv-uri
     :csv-uri csv-uri
     :metadata-uri metadata-uri
     :schema-path user-metadata
     :requests    (vec (cons action-request (concat (map file->request implicit) metadata-requests)))}))

(defn rdf-test-type [test-row]
  (let [type (some-> (get-in test-row [:extra :rdf]) keyword)]
    (#{:positive :negative :warning} type)))

(defn test-descriptor [{:keys [id action result-rdf comment] :as test-row}]
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
           :test-metadata {:title (:name test-row) :description comment}
           :mode (get-mode test-row)
           :expect-errors? (= :negative test-type)
           :expect-warnings? (= :warning test-type)
           :result-file (some-> result-rdf (test-path->file))})))))

(defn get-tests []
  (->> (parse-manifest)
       (filter #(> (.length (:id %)) 0))  ;;test026 appears to be invalid? Has no name and missing result file
       (map test-descriptor)
       (remove nil?)))

(defn find-test [id]
  (first (filter (fn [test] (= id (:id test))) (get-tests))))

(defmulti escape-read type)

(defmethod escape-read File [^File f]
  `(~'io/file ~(.getPath f)))

(defmethod escape-read URI [uri]
  `(~'URI. ~(str uri)))

(defmethod escape-read URL [url]
  `(~'URL. ~(str url)))

(defmethod escape-read clojure.lang.IPersistentMap [m]
  (into {} (map (fn [[k v]]
                  [(escape-read k) (escape-read v)])
                m)))

(defmethod escape-read :default [x] x)

(defn build-request-map [requests]
  (into {} (map (fn [{:keys [uri] :as m}]
                  [uri (dissoc m :uri)])
                requests)))

(defn make-test [{:keys [csv-uri metadata-uri action-uri requests id expect-errors? expect-warnings? mode result-file test-metadata] :as test}]
  (let [request-map (build-request-map requests)]
    `(~'deftest ~(symbol "^") ~test-metadata ~(symbol id)
       (~'let [~'http-client (~'->TestHttpClient ~(escape-read request-map))
               ~'csv-uri ~(escape-read csv-uri)
               ~'metadata-uri ~(escape-read metadata-uri)
               ~'{:keys [warnings errors result]} (~'http/with-http-client ~'http-client (~'test-csv->rdf ~'csv-uri ~'metadata-uri {:mode ~mode}))]
         ~(if (some? result-file)
            `(~'let [~'expected-statements (~'gio/statements ~(escape-read result-file) :base-uri ~(escape-read action-uri))]
              (~'is (~'= true (~'is-isomorphic? ~'expected-statements ~'result)))))

         ~(if expect-warnings?
            `(~'is (~'pos? (~'count ~'warnings)) "Expected warnings but none was found")
            `(~'is (~'= 0 (~'count ~'warnings)) "Received warnings but none was expected"))

         ~(if expect-errors?
            `(~'is (~'pos? (~'count ~'errors)) "Expected errors but none was found")
            `(~'is (~'= 0 (~'count ~'errors)) "Received errors but none was expected"))))))

(def ns-decl
  '(ns csv2rdf.w3c-csvw-suite-test
     "WARNING THIS FILE IS GENERATED DO NOT EDIT BY HAND.

  Generated test cases from the official W3C CSV Working Group's
  test suite.

  See csv2rdf.csvw-test.gen/write-tests-file for more details"
     (:require [clojure.test :refer [deftest is]]
               [clojure.java.io :as io]
               [csv2rdf.w3c-csvw-suite-test.impl :refer [is-isomorphic? test-csv->rdf]]
               [csv2rdf.test-common :refer [->TestHttpClient]]
               [csv2rdf.csvw :as csvw]
               [csv2rdf.http :as http]
               [grafter-2.rdf4j.io :as gio])
     (:import [java.net URI URL])))

(defn write-tests-file [{:keys [test-file] :or {test-file "test/csv2rdf/w3c_csvw_suite_test.clj"}}]
  (with-open [writer (io/writer (io/file test-file))]
    (binding [pp/*print-right-margin* 120
              pp/*print-pprint-dispatch* pp/code-dispatch]
      (pp/pprint ns-decl writer)
      (.write writer "\n")
      (doseq [t (get-tests)]
        (pp/pprint (make-test t) writer)
        (.write writer "\n")))))
