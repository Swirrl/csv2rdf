(ns csv2rdf.tabular.metadata-test
  (:require [csv2rdf.tabular.metadata :refer :all]
            [clojure.test :refer :all]
            [csv2rdf.http :as http]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [csv2rdf.uri-template :as template])
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

;;associated metadata

(def host-gen (gen/fmap
                (fn [t] (string/join "." (remove nil? t)))
                (gen/tuple (gen/elements [nil "odin" "thor" "hiemdall"])
                           (gen/elements ["example" "test" "object"])
                           (gen/elements ["com" "net" "org" "uk"]))))

(def directory-name-gen (gen/elements ["usr" "bin" "local" "opt" "home" "src"]))
(def dir-path-gen (gen/fmap
                    (fn [dirs]
                      (apply str "/" (map #(str % "/") dirs)))
                    (gen/vector directory-name-gen)))
(def file-stem-gen (gen/fmap #(apply str %) (gen/vector gen/char-alpha 3 10)))

(defn file-uri-gen [file-extension]
  (gen/let [schema (gen/elements ["http" "https"])
            host host-gen
            dir dir-path-gen
            file-stem file-stem-gen]
           (URI. schema host (str dir file-stem "." file-extension) nil)))

(def linked-metadata-uri-gen (file-uri-gen "json"))
(def csv-uri-gen (file-uri-gen "csv"))

(defn failing-metadata-request-gen [csv-uri metadata-uri]
  (gen/let [reason (gen/elements [:not-found :missing-reference])
            content-type metadata-content-type-gen]
           (case reason
             :not-found {:uri metadata-uri
                         :response {:status 404}}

             ;;TODO: generate docs with table references to other CSV URIs
             :missing-reference {:uri      metadata-uri
                                 :response {:status  200
                                            :headers {"Content-Type" content-type}
                                            :body    (json/write-str {})}})))

(defn valid-metadata-gen [csv-uri]
  (gen/return {"tables" [{"url" (str csv-uri)}]}))

(defn valid-metadata-request-gen [metadata-uri metadata-doc]
  (gen/let [content-type metadata-content-type-gen]
           {:uri metadata-uri
            :response {:status 200
                       :headers {"Content-Type" content-type}
                       :body (json/write-str metadata-doc)}}))

(def link-uri-state-gen
  (gen/let [csv-uri csv-uri-gen
            metadata-uri (gen/frequency [[1 (gen/return nil)] [4 linked-metadata-uri-gen]])]
           {:state (if (nil? metadata-uri)
                     ::site-wide-configuration
                     ::metadata-uri)
            :metadata-uri metadata-uri
            :csv-uri csv-uri
            :requests []}))

(defmulti gen-state-transition :state)

(defmethod gen-state-transition ::metadata-uri [{:keys [csv-uri metadata-uri requests]}]
  (let [ok-gen (gen/let [metadata (valid-metadata-gen csv-uri)
                         ok-request (valid-metadata-request-gen metadata-uri metadata)]
                        (gen-state-transition
                          {:state        ::done
                           :csv-uri      csv-uri
                           :metadata-uri metadata-uri
                           :requests     (conj requests ok-request)
                           :result       metadata}))
        failing-gen (gen/bind (failing-metadata-request-gen csv-uri metadata-uri)
                              (fn [req]
                                (gen-state-transition
                                  {:state        ::site-wide-configuration
                                   :csv-uri      csv-uri
                                   :metadata-uri metadata-uri
                                   :requests     (conj requests req)})))]
    (gen/frequency [[1 ok-gen] [3 failing-gen]])))

(def uri-template-gen (gen/fmap #(str "{+url}-" % ".json") file-stem-gen))

(defmethod gen-state-transition ::site-wide-configuration [{:keys [csv-uri metadata-uri requests]}]
  (let [site-wide-config-uri (.resolve csv-uri well-known-site-wide-configuration-uri)]
    (gen/let [has-site-wide-config? gen/boolean]
             (if has-site-wide-config?
               (gen/let [templates (gen/vector uri-template-gen)]
                        (let [req {:uri site-wide-config-uri
                                   :response {:status 200
                                              :headers {"Content-Type" "text/plain"}
                                              :body (string/join "\n" templates)}}]
                          (gen-state-transition {:state ::resolve-templates
                                                 :csv-uri csv-uri
                                                 :metadata-uri metadata-uri
                                                 :valid-templates templates
                                                 :requests (conj requests req)})))
               (let [req {:uri site-wide-config-uri
                          :response {:status 404}}]
                 (gen-state-transition {:state ::resolve-templates
                                        :csv-uri csv-uri
                                        :metadata-uri metadata-uri
                                        :valid-templates default-location-templates
                                        :requests (conj requests req)}))))))

(defn resolve-metadata-template [csv-uri template-string]
  (let [template-uri (template/expand-template-string template-string {:url csv-uri})]
    (.resolve csv-uri template-uri)))

(defmethod gen-state-transition ::resolve-templates [{:keys [csv-uri metadata-uri valid-templates requests]}]
  (gen/let
    [ok-index (gen/choose 0 (count valid-templates))]
    (let [[failing-templates [ok-template & ignored-templates]] (split-at ok-index valid-templates)
          failing-uris (map #(resolve-metadata-template csv-uri %) failing-templates)
          failing-requests-gen (apply gen/tuple (map #(failing-metadata-request-gen csv-uri %) failing-uris))
          ok-uri (some->> ok-template (resolve-metadata-template csv-uri))]
      ;;NOTE: gen/choose generates a value in the inclusive range [0 (count valid-templates)]
      ;;so there could be no valid request (also if count is 0)
      (if (nil? ok-uri)
        (gen/bind failing-requests-gen
                  (fn [failing-requests]
                    (gen/return
                      {:state        ::done
                       :csv-uri      csv-uri
                       :metadata-uri metadata-uri
                       :requests     (vec (concat requests failing-requests))
                       :result       nil})))
        (gen/let [failing-requests failing-requests-gen
                  ok-metadata (valid-metadata-gen csv-uri)
                  ok-request (valid-metadata-request-gen ok-uri ok-metadata)]
                 {:state ::done
                  :csv-uri csv-uri
                  :metadata-uri metadata-uri
                  :requests (vec (concat requests failing-requests [ok-request]))
                  :result ok-metadata})))))

(defmethod gen-state-transition ::done [state]
  (gen/return state))

(def resolve-metadata-state-gen (gen/bind link-uri-state-gen gen-state-transition))

(defrecord StrictTestHttpClient [requests]
  http/HttpClient
  (http-get [_this request-uri]
    (if-let [[{:keys [uri response] :as next-req} & reqs] @requests]
      (if (= request-uri uri)
        (do (reset! requests reqs)
            response)
        (throw (IllegalArgumentException.
                 (str "Unexpected next request URI <" request-uri "> expected <" uri ">"))))
      (throw (IllegalStateException. (str "Made request to URI <" request-uri "> when none was expected"))))))

(defn run-associated-metadata-test [{:keys [csv-uri metadata-uri requests] :as state}]
  (let [client (->StrictTestHttpClient (atom requests))]
    (http/with-http-client client (resolve-associated-metadata csv-uri metadata-uri))))

(defspec resolve-associated-metadata-test 100
  (prop/for-all
    [{:keys [result] :as state} resolve-metadata-state-gen]
    (let [actual (run-associated-metadata-test state)]
      (= result actual))))

