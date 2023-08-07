(ns build
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]
            [clojure.string :as str]
            [juxt.pack.api :as pack])
  (:import [com.google.cloud.tools.jib.api JibContainer])
  (:refer-clojure :exclude [test]))

(def lib 'swirrl/csv2rdf)
(def version (str/replace (or (System/getenv "CIRCLE_TAG")
                              "v0.5.999-SNAPSHOT")
                          #"^v" ""))
(def class-dir "target/classes")


(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn test
  "Run the tests"
  [opts]
  (bb/run-tests (assoc opts :aliases [:with-logging :dev])))

(defn build-lib
  "Run the CI pipeline of tests (and build the JAR)."
  [opts]
  (-> opts
      (assoc :lib lib
             :version version
             :src-pom "template/pom.xml")
      (bb/clean)
      (bb/jar)))

(defn install
  "Install the JAR locally."
  [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/install)))

(defn deploy
  "Deploy the JAR to Clojars.

  NOTE: this expects a tag to be set; typically done via the github release UI."
  [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/deploy)))

(defn clean [_]
  (b/delete {:path "target"}))

;; NOTE this is not yet built by CI
(defn build-app
  "Build an uberjar for the command line app"
  [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["resources" "profiles/with-logging/resources"]
               :target-dir class-dir})
  (let [basis (b/create-basis {:aliases [:cli :with-logging]})]
    (b/compile-clj {:basis basis
                    :class-dir class-dir
                    :src-dirs ["src"]})
    (bb/uber {:main 'csv2rdf.main
              :basis basis
              :class-dir class-dir
              :uber-file "target/csv2rdf-app.jar"
              })))

;; A tag name must be valid ASCII and may contain lowercase and uppercase
;; letters, digits, underscores, periods and dashes
;; https://docs.docker.com/engine/reference/commandline/tag/#extended-description
(defn tag [s]
  (when s (str/replace s #"[^a-zA-Z0-9_.-]" "_")))

(def repo "europe-west2-docker.pkg.dev/swirrl-devops-infrastructure-1/public")

(defn docker [opts]
  (let [tags (->> ["rev-parse HEAD"
                   "describe --tags --always"
                   "branch --show-current"]
                  (map #(tag (b/git-process {:git-args %})))
                  (remove nil?))
        image-type (get opts :image-type :docker)
        build-args {:basis (b/create-basis {:project "deps.edn" :aliases [:docker]})
                    ;; If we don't include a tag in the :image-name, then pack implicitly
                    ;; tags the image with latest, even when we specify additional tags. So
                    ;; choose a tag arbitrarily to be part of the :image-name, and then
                    ;; provide the rest in :tags.
                    :image-name (str repo "/csv2rdf:" (first tags))
                    :tags (set (rest tags))
                    :image-type image-type
                    :base-image "eclipse-temurin:17" ;; An openJDK 17 base docker provided by https://github.com/adoptium/containers#containers
                    }
        publish-args {:platforms #{:linux/amd64 :linux/arm64}

                      ;; NOTE Not as documented!
                      ;; The docstring states that these should be
                      ;;     :to-registry {:username ... :password ...}
                      ;; but alas, that is a lie.
                      ;; https://github.com/juxt/pack.alpha/issues/101
                      :to-registry-username "_json_key"
                      :to-registry-password (System/getenv "GCLOUD_SERVICE_KEY")}
        args (if (= :remote (:to opts))
               (merge build-args publish-args)
               build-args)
        ^JibContainer container (pack/docker args)]
    (println (.. container (getDigest) (getHash)))))
