(ns csv2rdf.main
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [csv2rdf.csvw :as csvw]
            [clojure.java.io :as io]
            [grafter.rdf.io :as gio]
            [grafter.rdf.formats :as formats]
            [clojure.tools.logging :as log])
  (:import [java.net URI URISyntaxException]
           [org.openrdf.rio RDFFormat]))

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

(defn usage-error
  "Displays the given invocation errors followed by the usage summary before exiting."
  [errors summary]
  (doseq [e errors]
    (log/error e))
  (log/info "Usage:")
  (log/info summary)
  (System/exit 1))

(defn -main [& args]
  (let [{:keys [summary options] :as parse-result} (cli/parse-opts args options-spec)
        errors (get-errors parse-result)]
    (if (seq errors)
      (usage-error errors summary)
      (let [{:keys [mode tabular user-metadata output-file]} options
            tabular-source (some-> tabular parse-source)
            metadata-source (some-> user-metadata parse-source)
            output-file (some-> output-file io/file)
            rdf-format (or (some-> output-file formats/->rdf-format) RDFFormat/TURTLE)]
        (with-open [w (io/writer (or output-file *out*))]
          (try
            (let [dest (gio/rdf-serializer w :format rdf-format :prefixes nil)]
              (csvw/csv->rdf->destination tabular-source metadata-source dest {:mode mode}))
            (catch Exception ex
              (log/error ex))))))))
