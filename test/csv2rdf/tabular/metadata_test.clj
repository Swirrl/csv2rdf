(ns csv2rdf.tabular.metadata-test
  (:require [csv2rdf.tabular.metadata :refer :all]
            [clojure.test :refer :all]
            [csv2rdf.http :as http]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import [java.net URI]))

(def metadata-content-type-gen (gen/elements metadata-link-header-content-types))
(defn shuffle-case [string-gen]
  (gen/let [s string-gen
            flags (gen/vector gen/boolean (.length s))]
           (let [chars (map (fn [c flip?]
                              (if flip?
                                (cond (Character/isLowerCase c) (Character/toUpperCase c)
                                      (Character/isUpperCase c) (Character/toLowerCase c)
                                      :else c)
                                c)) s flags)]
             (apply str chars))))
(def metadata-rel-gen (shuffle-case (gen/return "describedby")))
(def metadata-uri-gen (gen/return (URI. "test-metadata.json")))
(def metadata-link-gen (gen/fmap #(zipmap [::http/link-uri :rel :type] %)
                                 (gen/tuple metadata-uri-gen metadata-rel-gen metadata-content-type-gen)))

(def non-metadata-link-gen
  (let [rel-types #{"stylesheet" "next" "prev"}
        content-types #{"text/css" "text/html" "application/edn"}
        rel-gen (gen/one-of [(gen/return nil) (gen/elements rel-types)])
        type-gen (gen/one-of [(gen/return nil) (gen/elements content-types)])
        ;;NOTE: URI value not used to identify metadata links
        uri-gen (gen/elements [(URI. "style.css") (URI. "http://example.com")])]
    (gen/fmap
      (fn [elems]
        (->> (zipmap [::http/link-uri :rel :type] elems)
             (remove (fn [[k v]] (nil? v)))
             (into {})))
      (gen/tuple uri-gen rel-gen type-gen))))

(defn format-link [link]
  (let [uri (::http/link-uri link)
        params (dissoc link ::http/link-uri)
        formatted-params (map (fn [[k v]]
                                (format "; %s=%s" (name k) v))
                              params)]
    (str "<" uri "> " (apply str formatted-params))))

(def non-metadata-headers-gen
  (gen/let
    [links (gen/vector non-metadata-link-gen)]
    (let [other-headers {"Content-Type" "text/csv"
                         "Content-Length" "45566"}]
      (condp = (count links)
        0 other-headers
        1 (assoc other-headers http/link-header-name (format-link (first links)))
        (assoc other-headers http/link-header-name (mapv format-link links))))))

(defn make-link-headers [link other-links]
  (if (empty? other-links)
    (format-link link)
    (map format-link (concat other-links) [link])))

(deftest get-metadata-link-exists-test
  (prop/for-all
    [[link other-links] (gen/tuple metadata-link-gen (gen/vector metadata-link-gen))]
    (let [headers {"Content-Type" "text/csv"
                   "Content-Length" "1024"
                   http/link-header-name (make-link-headers link other-links)}
          found (get-metadata-link {:headers headers})]
      (is (= link found)))))

(deftest get-metadata-link-missing-test
  (prop/for-all
    [headers non-metadata-headers-gen]
    (is (nil? (get-metadata-link {:headers headers})))))

(deftest get-metadata-link-malformed-test
  (let [headers {http/link-header-name "<invalid uri> ; rel=\"describedby\" type=\"application/json\""}]
    (is (nil? (get-metadata-link {:headers headers})))))
