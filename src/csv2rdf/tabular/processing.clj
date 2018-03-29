(ns csv2rdf.tabular.processing
  (:require [csv2rdf.tabular.metadata :as tmeta]))

(defn ^{:table-spec "6.1"} from-tabular-file [file-source]
  (tmeta/get-metadata file-source))
