(ns csv2rdf.vocabulary
  (:import [java.net URI]))

(def rdf:type (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
(def rdf:nil (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
(def rdf:first (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
(def rdf:rest (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))

(def csvw (URI. "http://www.w3.org/ns/csvw#"))
(def csvw:TableGroup (URI. "http://www.w3.org/ns/csvw#TableGroup"))
(def csvw:table (URI. "http://www.w3.org/ns/csvw#table"))
(def csvw:Table (URI. "http://www.w3.org/ns/csvw#Table"))
(def csvw:url (URI. "http://www.w3.org/ns/csvw#url"))
(def csvw:row (URI. "http://www.w3.org/ns/csvw#row"))
(def csvw:Row (URI. "http://www.w3.org/ns/csvw#Row"))
(def csvw:rownum (URI. "http://www.w3.org/ns/csvw#rownum"))
(def csvw:describes (URI. "http://www.w3.org/ns/csvw#describes"))

