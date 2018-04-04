(ns csv2rdf.metadata.uri-template-property
  (:require [clojure.spec.alpha :as s]
            [csv2rdf.uri-template :as template]
            [csv2rdf.json-ld :as json-ld]
            [csv2rdf.util :as util])
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
      (.resolve url expanded-uri-string))))
