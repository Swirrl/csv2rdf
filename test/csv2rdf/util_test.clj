(ns csv2rdf.util-test
  (:require [csv2rdf.util :refer :all]
            [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]])
  (:import [java.net URI]))


(deftest set-fragment-test
  (testing "transparent uri"
    (is (= (URI. "http:8080//example.org/path?param=bar#new-fragment")
           (set-fragment (URI. "http:8080//example.org/path?param=bar#fragment") "new-fragment"))))
  (testing "opaque uri"
    (is (= (URI. "jar:file:///some/path!/inside/zip/path#new-fragment")
           (set-fragment (URI. "jar:file:///some/path!/inside/zip/path#fragment") "new-fragment")))))

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

(deftest normalise-uri-test
  (testing "HTTP"
    (are [expected-str un-normalised-str] (= (URI. expected-str) (normalise-uri (URI. un-normalised-str)))
      "http://example.com/" "http://example.com:80"
      "http://example.com/" "http://example.com:"
      "http://example.com:8080/" "http://example.com:8080"
      "http://example.com/some/path" "http://example.com/some/path"))

  (testing "HTTPS"
    (are [expected-str un-normalised-str] (= (URI. expected-str) (normalise-uri (URI. un-normalised-str)))
      "https://example.com/" "https://example.com:443"
      "https://example.com/" "https://example.com:"
      "https://example.com:4433/" "https://example.com:4433"
      "https://example.com/some/path" "https://example.com/some/path")))

(def split-gen (gen/let [xs (gen/list gen/int)
                         n (gen/choose 0 (count xs))]
                 [n xs]))

(defspec eager-split-at-test 100
  (prop/for-all
    [[n xs] split-gen]
    (let [[expected-prefix expected-suffix] (split-at n xs)
          [prefix suffix] (eager-split-at n xs)]
      (and (= expected-prefix prefix)
           (= (seq expected-suffix) (seq suffix))))))
