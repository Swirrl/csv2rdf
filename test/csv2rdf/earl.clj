(ns csv2rdf.earl
  (:require [clojure.test :as ct]
            [grafter.rdf :refer [add statements]]
            [grafter.rdf.templater :refer [triplify]]
            [grafter.rdf.io :refer [rdf-serializer]]
            [csv2rdf.csvw-test]
            [clojure.java.io :as io])
  (:import [java.net URI]
           [java.util Date]
           [java.io PushbackReader]))

(def ^:dynamic *report-dest*)

(defn- get-project
  "Reads project.clj as a clojure form"
  []
  (with-open [r (PushbackReader. (io/reader "project.clj"))]
    (read r)))

(defn- get-version
  "Gets the version number from project.clj"
  []
  (nth (get-project) 2))

;;NOTE: see assertor in earl-report.ttl
(def assertor (URI. "https://github.com/Swirrl/csv2rdf/csvw/test"))

(def csv2rdf-uri (URI. "https://github.com/Swirrl/csv2rdf"))

(def rdf:type (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
(def earl:Assertion (URI. "http://www.w3.org/ns/earl#Assertion"))
(def earl:TestCase (URI. "http://www.w3.org/ns/earl#TestCase"))
(def earl:assertedBy (URI. "http://www.w3.org/ns/earl#assertedBy"))
(def earl:subject (URI. "http://www.w3.org/ns/earl#subject"))
(def earl:test (URI. "http://www.w3.org/ns/earl#test"))
(def earl:result (URI. "http://www.w3.org/ns/earl#result"))
(def earl:TestResult (URI. "http://www.w3.org/ns/earl#TestResult"))
(def earl:outcome (URI. "http://www.w3.org/ns/earl#outcome"))
(def earl:passed (URI. "http://www.w3.org/ns/earl#passed"))
(def earl:failed (URI. "http://www.w3.org/ns/earl#failed"))
(def earl:mode (URI. "http://www.w3.org/ns/earl#mode"))
(def earl:automatic (URI. "http://www.w3.org/ns/earl#automatic"))

(def dc:title (URI. "http://purl.org/dc/terms/title"))
(def dc:description (URI. "http://purl.org/dc/terms/description"))
(def dc:date (URI. "http://purl.org/dc/terms/date"))

(def doap:revision (URI. "http://usefulinc.com/ns/doap#revision"))

(defn- assertion-statements [test-uri test-title test-description outcome]
  (let [assertion-node (keyword (str "assert" (gensym)))
        result-node (keyword (str "result" (gensym)))]
    (triplify [test-uri
               [rdf:type earl:TestCase]
               [dc:title test-title]
               [dc:description test-description]]
              [assertion-node
               [rdf:type earl:Assertion]
               [earl:assertedBy assertor]
               [earl:subject csv2rdf-uri]
               [earl:test test-uri]
               [earl:result result-node]
               [earl:mode earl:automatic]]
              [result-node
               [rdf:type earl:TestResult]
               [earl:outcome outcome]
               [dc:date (Date.)]])))

(defn- get-test-uri [test-var]
  (URI. (str "http://www.w3.org/2013/csvw/tests/manifest-rdf#" (:name (meta test-var)))))

(defn add-test-assertion [outcome]
  (let [test-var (first ct/*testing-vars*)
        {test-description :description test-title :title} (meta test-var)]
    (add *report-dest* (assertion-statements (get-test-uri test-var) test-title test-description outcome))))

(defmulti report-earl "Report function with the same type as clojure.test/report" :type)

(defmethod report-earl :default [m])

(defmethod report-earl :pass [m]
  (add-test-assertion earl:passed))

(defmethod report-earl :fail [m]
  (add-test-assertion earl:failed))

(defn project-statements []
  (triplify
    [csv2rdf-uri
     [doap:revision (get-version)]]))

(defn write-csvw-earl-report
  "Runs the CSVW test cases and writes an EARL report to the specified output file"
  [output-file]
  (with-open [w (io/writer output-file)]
    (binding [ct/report report-earl
              *report-dest* (rdf-serializer w :format :ttl :prefixes nil)]
      (add *report-dest* (statements (io/resource "earl-report.ttl") :format :ttl))
      (add *report-dest* (project-statements))
      (ct/run-tests 'csv2rdf.csvw-test))))
