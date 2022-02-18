(ns build
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]
            [clojure.string :as str])
  (:refer-clojure :exclude [test]))

(def lib 'swirrl/csv2rdf)
(def version (str/replace (or (System/getenv "CIRCLE_TAG")
                              "v0.5.999-SNAPSHOT")
                          #"^v" ""))

(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn test
  "Run the tests"
  [opts]
  (bb/run-tests (assoc opts :aliases [:with-logging :dev])))

(defn build
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
