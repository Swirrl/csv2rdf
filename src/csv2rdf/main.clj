(ns csv2rdf.main
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [csv2rdf.csvw :as csvw]
            [clojure.java.io :as io]
            [grafter-2.rdf4j.io :as gio]
            [grafter-2.rdf4j.formats :as formats]
            [csv2rdf.logging :as log])
  (:import [java.net URI URISyntaxException]
           [org.eclipse.rdf4j.rio RDFFormat]))

(set! *warn-on-reflection* true)

(def options-spec
  [["-t" "--tabular TABULAR" "Location of the tabular file"]
   ["-u" "--user-metadata METADATA" "Location of the metadata file"]
   ["-s" "--validate-schema" "Validate the schema only"]
   ["-d" "--validate-data" "Validate the data against the schema only (no RDFization)"]
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
  (log/log-error ex))

(defmethod display-error ::invalid-cli-arguments [ex]
  (let [{:keys [errors summary]} (ex-data ex)]
    (doseq [e errors]
      (log/log-error e))
    (println "Usage:")
    (println summary)))



(defn inner-main [args]
  (let [options (parse-cli-options args)
        {:keys [mode tabular user-metadata output-file validate-data annotate-tables]} options
        opts {:tabular-source (some-> tabular parse-source)
              :metadata-source (some-> user-metadata parse-source)
              :rdf-format (or (some-> output-file formats/->rdf-format) RDFFormat/TURTLE)
              :mode mode}
        output-file (some-> output-file io/file)]

    (cond validate-data (csvw/only-validate-schema opts)

          :else (if output-file
                  (with-open [w (io/writer output-file)]
                    (write-output w opts))
                  (write-output (io/writer *out*) opts)))))

(defn main-with-exit! [args]
  (if (:data-validation-errors? (inner-main args))
    (System/exit 2)
    (System/exit 0)))

(defn- -main [& args]
  (try
    ;; Check if we're running on GraalVM
    ;; https://github.com/oracle/graal/blob/39c80292e2f92822a3882c1350226706abd78917/sdk/src/org.graalvm.nativeimage/src/org/graalvm/nativeimage/ImageInfo.java#L132
    (if (System/getProperty "org.graalvm.nativeimage.imagecode")
      (log/with-logger (log/->PrintlnLogger)
        (main-with-exit! args))
      (main-with-exit! args))
    (catch Throwable ex
      (display-error ex)
      (System/exit 1))))


(comment

  (inner-main ["-s" "-t" "/Users/rick/repos/dclg-epcs/resources/public/csvw/basic/certificates.csv" "-u" "/Users/rick/repos/dclg-epcs/resources/public/csvw/basic/epc_domestic.json"])
  (time (inner-main ["-t" "out/hmrc-rts-small-area.csv" "-u" "out/hmrc-rts-small-area.csv-metadata.json" "-m" "annotated" "-o" "cube.nt"]))

  (require '[clj-async-profiler.core :as prof])

;; Profile the following expression:
  (prof/profile (inner-main ["-t" "out/hmrc-rts-small-area.csv" "-u" "out/hmrc-rts-small-area.csv-metadata.json" "-m" "annotated" "-o" "cube.nt"]))

;; The resulting flamegraph will be stored in /tmp/clj-async-profiler/results/
;; You can view the SVG directly from there or start a local webserver:

(prof/serve-files 8080) ; Serve on port 8080

  )
