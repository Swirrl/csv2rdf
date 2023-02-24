(ns build
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]
            [clojure.string :as str])
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
