(ns csv2rdf.metadata.properties
  (:require [csv2rdf.metadata.column :as column]
            [csv2rdf.metadata.datatype :as datatype]))

(defn- declared
  ([key] (declared key nil))
  ([key default]
    (fn [obj]
      (get obj key default))))

(defn- required [key]
  (fn [obj]
    (if-let [value (get obj key)]
      value
      (throw (ex-info (format "missing required property %s" (name key)) {:property key
                                                                          :metadata obj})))))

(defn- find-inherited-property [obj key default]
  (if-let [value (get obj key)]
    value
    (if-let [parent (::parent obj)]
      (recur parent key default)
      default)))

(defn- inherited
  ([key] (inherited key nil))
  ([key default]
   (fn [obj] (find-inherited-property obj key default))))

(def ^{:metadata-spec "5"} id (declared :id))
(def ^{:metadata-spec "5"} type-uri (declared :type))

(def ^{:metadata-spec "5.3.1"} tables (required :tables))

;;TODO: should this be inherited?
(def ^{:metadata-spec ["5.3.2" "5.4.2"]} dialect (inherited :dialect))
(def ^{:metadata-spec ["5.3.2" "5.4.2"]} notes (declared :notes))
(def ^{:metadata-spec ["5.3.2" "5.4.2"]} table-direction (inherited :tableDirection "auto"))
(def ^{:metadata-spec ["5.3.2" "5.4.2"]} table-schema (inherited :tableSchema))
(def ^{:metadata-spec ["5.3.2" "5.4.2"]} transformations (inherited :transformations))
(def ^{:metadata-spec "5.4.1"} url (required :url))

;;NOTE: this property exists on both table and column but is not inherited
(def ^{:metadata-spec ["5.4.2" "5.6"]} suppress-output? (declared :suppressOutput false))
(def ^{:metadata-spec "5.5"} columns (declared :columns))

;;TODO: add properties for foreign keys
(def ^{:metadata-spec "5.5"} foreign-keys (declared :foreignKeys))

(def ^{:metadata-spec "5.5"} primary-keys (declared :primaryKeys))
(def ^{:metadata-spec "5.5"} row-titles (declared :rowTitles))

;;NOTE: this refers to the effective name of the column, not the name property defined in the specification
(def ^{:metadata-spec "5.6"} column-name (required ::column/name))
(def ^{:metadata-spec "5.6"} titles (declared :titles))
(def ^{:metadata-spec "5.6"} virtual? (declared :virtual false))

(def ^{:metadata-spec "5.7"} about-url (inherited :aboutUrl))
(def ^{:metadata-spec "5.7"} datatype (inherited :datatype datatype/datatype-defaults))
(def ^{:metadata-spec "5.7"} default (inherited :default ""))
(def ^{:metadata-spec "5.7"} lang (inherited :lang "und"))
(def ^{:metadata-spec "5.7"} null (inherited :null [""]))
(def ^{:metadata-spec "5.7"} ordered? (inherited :ordered false))
(def ^{:metadata-spec "5.7"} property-url (inherited :propertyUrl))
(def ^{:metadata-spec "5.7"} required? (inherited :required false))
(def ^{:metadata-spec "5.7"} separator (inherited :separator))
(def ^{:metadata-spec "5.7"} text-direction (inherited :textDirection "inherit"))
(def ^{:metadata-spec "5.7"} value-url (inherited :valueUrl))