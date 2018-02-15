(ns csv2rdf.csvw)

(defn csv->rdf
  ([csv-source metadata-source] (csv->rdf csv-source metadata-source {}))
  ([csv-source metadata-source options]
    {:errors [] :warnings [] :result nil}))
