(ns csv2rdf.vocabulary
  (:import [java.net URI]))

(def rdf:nil (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
(def rdf:first (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
(def rdf:rest (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))

