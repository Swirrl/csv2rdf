(ns csv2rdf.main
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [csv2rdf.csvw :as csvw]
            [clojure.java.io :as io]
            [grafter-2.rdf4j.io :as gio]
            [grafter-2.rdf4j.formats :as formats]
            [clojure.tools.logging :as log])
  (:import [java.net URI URISyntaxException]
           [org.eclipse.rdf4j.rio RDFFormat]))

(def options-spec
  [["-t" "--tabular TABULAR" "Location of the tabular file"]
   ["-u" "--user-metadata METADATA" "Location of the metadata file"]
   ["-o" "--output-file OUTPUT" "Output file to write to"]
   ["-m" "--mode MODE" "CSVW mode to run"
    :validate [#(contains? #{:minimal :standard :annotated} %)]
    :default :standard
    :parse-fn keyword]])

(defn parse-source
  "Converts a string into a tabular or metadata source. If the string is a valid absolute URI, a URI is returned
   otherwise it is assumed to reference a file on disk."
  [source-str]
  (try
    (let [uri (URI. source-str)]
      (if (.isAbsolute uri)
        uri
        (io/file source-str)))
    (catch URISyntaxException _ex
      (io/file source-str))))

(defn get-errors
  "Returns the collection of invocation option errors from the parsed options map."
  [{:keys [errors options] :as parse-result}]
  (if (or (contains? options :tabular) (contains? options :user-metadata))
    errors
    (cons "User metadata or tabular data file required" errors)))

(defn- parse-cli-options
  "Parses a sequence of command-line arguments into an options map. Throws an exception if
   the arguments are invalid."
  [args]
  (let [{:keys [summary options] :as parse-result} (cli/parse-opts args options-spec)
        errors (get-errors parse-result)]
    (if (seq errors)
      (throw (ex-info "Invalid command-line arguments" {:type ::invalid-cli-arguments
                                                        :errors errors
                                                        :summary summary}))
      options)))

(defn- write-output [writer {:keys [rdf-format tabular-source metadata-source mode]}]
  (let [dest (gio/rdf-writer writer :format rdf-format :prefixes nil)]
    (csvw/csv->rdf->destination tabular-source metadata-source dest {:mode mode})))

(defmulti display-error
          "Displays an exception in the UI"
          (fn [ex] (:type (ex-data ex))))

(defmethod display-error :default [ex]
  (log/error ex))

(defmethod display-error ::invalid-cli-arguments [ex]
  (let [{:keys [errors summary]} (ex-data ex)]
    (doseq [e errors]
      (log/error e))
    (println "Usage:")
    (println summary)))

(defn- -main [& args]
  (try
    (let [options (parse-cli-options args)
          {:keys [mode tabular user-metadata output-file]} options
          opts {:tabular-source (some-> tabular parse-source)
                :metadata-source (some-> user-metadata parse-source)
                :rdf-format (or (some-> output-file formats/->rdf-format) RDFFormat/TURTLE)
                :mode mode}
          output-file (some-> output-file io/file)]
      (if output-file
        (with-open [w (io/writer output-file)]
          (write-output w opts))
        (write-output (io/writer *out*) opts))
      (System/exit 0))
    (catch Throwable ex
      (display-error ex)
      (System/exit 1))))
