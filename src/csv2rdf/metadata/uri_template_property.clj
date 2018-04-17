(ns csv2rdf.metadata.uri-template-property
  (:require [clojure.spec.alpha :as s]
            [csv2rdf.uri-template :as template]
            [csv2rdf.json-ld :as json-ld]
            [csv2rdf.util :as util]
            [csv2rdf.tabular.cell :as cell]
            [csv2rdf.metadata.column :as column]
            [clojure.string :as string])
  (:import [java.net URI]))

(s/def ::_column pos?)
(s/def ::_sourceColumn pos?)
(s/def ::_row pos?)
(s/def ::_name string?)

(s/def ::column-bindings (s/keys :req-un [::_column ::_sourceColumn ::_row ::_name]))

(defn ^{:metadata-spec "5.1.3"} resolve-uri-template-property [uri-template column-bindings {:keys [^URI url] :as table}]
  ;;TODO: expanded template strings are percent encoded. See if there's a way to avoid the immediate decoding
  (let [resolved-uri-string (util/percent-decode (template/expand-template-to-string uri-template column-bindings))
        expanded-uri-string (json-ld/expand-uri-string resolved-uri-string)]
    (if (nil? url)
      (URI. expanded-uri-string)
      (util/resolve-uri url expanded-uri-string))))

(defn ^{:tabular-spec "6.4.9"
        :justification "The value URL annotation is null if the cell value is null and the column virtual annotation is false"
        } resolve-value-uri-template-property
  [uri-template cell column column-bindings table]
  (let [cell-value (cell/semantic-value cell)]
    (if (and (nil? cell-value) (not (column/is-virtual? column)))
      nil
      (resolve-uri-template-property uri-template column-bindings table))))
