(ns csv2rdf.util-test
  (:require [csv2rdf.util :refer :all]
            [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(defspec filter-values-test 100
  (prop/for-all
    [m (gen/map gen/keyword gen/int)]
    (let [pred pos?
          filtered (filter-values pred m)]
      (every? pred (vals filtered)))))

(defspec filter-values-submap-test 100
  (prop/for-all
    [m (gen/map gen/keyword gen/int)]
    (let [filtered (filter-values pos? m)]
      (= filtered (select-keys m (keys filtered))))))

(defspec percent-encode-decode-test 100
  (prop/for-all
    [s gen/string-ascii]
    (= s (percent-decode (percent-encode s)))))

(defspec hex-string-parse-format-test 100
  (prop/for-all
    [bs gen/bytes]
    (= (seq bs) (seq (parse-hex-string (to-hex-string bs))))))


