(ns csv2rdf.metadata.properties
  (:require [csv2rdf.metadata.column :as column]
            [csv2rdf.metadata.datatype :as datatype])
  (:import [clojure.lang IDeref]
           [java.io Writer]))

;;read-only container for metadata parent references. Overrides toString and print-method
;;to reduce the printed output in the REPL.
(defrecord ParentRef [parent]
  IDeref
  (deref [_this] parent)

  Object
  (toString [_this] "Parent reference"))

(defmethod print-method ParentRef [x ^Writer writer]
  (.write writer "Parent reference"))

(defn- child
  "Returns a function which takes a parent metadata record and associates in the child item at the given
   key a reference to its parent. If child-fn is provided it is used to recursively set parent references
   under the child record."
  ([child-key]
   (child child-key identity))
  ([child-key child-fn]
    (fn [parent]
      (update parent child-key (fn [child]
                                 (child-fn (assoc child ::parent (->ParentRef parent))))))))

(defn- children
  "Returns a function which takes a parent metadata record and associates a reference to the parent in each child
  item contained in the collection at the given key. If child-fn is provided it is used to recursively set parent
  references under the child record"
  ([children-key]
   (children children-key identity))
  ([children-key child-fn]
    (fn [parent]
      (update parent children-key (fn [cs]
                                    (mapv (fn [c] (child-fn (assoc c ::parent (->ParentRef parent)))) cs))))))

(def set-table-parent-references
  (child :tableSchema
         (children :columns)))

(def set-table-group-parent-references
  (children :tables set-table-parent-references))

(defn- declared
  "Defines a property accessor which returns the value at the given key from the given metadata, or the
   default value if no value exists."
  ([key] (declared key nil))
  ([key default]
    (fn [obj]
      (get obj key default))))

(defn- required
  "Defines a property accessor which requires the key to exist on the given metadata"
  [key]
  (fn [obj]
    (if-let [value (get obj key)]
      value
      (throw (ex-info (format "missing required property %s" (name key)) {:property key
                                                                          :metadata obj})))))

(defn- find-inherited-property [obj key default]
  (if-let [value (get obj key)]
    value
    (if-let [parent-ref (::parent obj)]
      (recur @parent-ref key default)
      default)))

(defn- inherited
  "Defines a property accessor which look for key on the given metadata item before recursively searching its
   ancestors if not found. Returns nil or the specified default if the key is not defined on the metadata or any
   of its ancestors."
  ([key] (inherited key nil))
  ([key default]
   (fn [obj] (find-inherited-property obj key default))))

(def ^{:metadata-spec "5"} id (declared :id))
(def ^{:metadata-spec "5"} type-uri (declared :type))

(def ^{:metadata-spec "5.3.1"} tables (required :tables))

(def ^{:metadata-spec ["5.3.2" "5.4.2"]} dialect (inherited :dialect))
(def ^{:metadata-spec ["5.3.2" "5.4.2"]} notes (declared :notes))
(def ^{:metadata-spec ["5.3.2" "5.4.2"]} table-direction (inherited :tableDirection "auto"))
(def ^{:metadata-spec ["5.3.2" "5.4.2"]} table-schema (inherited :tableSchema))
(def ^{:metadata-spec ["5.3.2" "5.4.2"]} transformations (inherited :transformations))
(def ^{:metadata-spec "5.4.1"} url (required :url))

;;NOTE: this property exists on both table and column but is not inherited
(def ^{:metadata-spec ["5.4.2" "5.6"]} suppress-output? (declared :suppressOutput false))
(def ^{:metadata-spec "5.5"} columns (declared :columns))

(def ^{:metadata-spec "5.5"} foreign-keys (declared :foreignKeys))

;;NOTE: exists on both foreign key and foreign key reference and is required on both
(def ^{:metadata-spec "5.5"} column-reference (required :columnReference))
(def ^{:metadata-spec "5.5"} reference (required :reference))
(def ^{:metadata-spec "5.5"} resource (declared :resource))
(def ^{:metadata-spec "5.5"} schema-reference (declared :schemaReference))

(def ^{:metadata-spec "5.5"} primary-keys (declared :primaryKeys))
(def ^{:metadata-spec "5.5"} row-titles (declared :rowTitles))

;;NOTE: this refers to the effective name of the column, not the name property defined in the specification
(def ^{:metadata-spec "5.6"} column-name (required ::column/name))
(def ^{:metadata-spec "5.6"} titles (declared :titles))
(def ^{:metadata-spec "5.6"} virtual? (declared :virtual false))

(def ^{:metadata-spec "5.7"} about-url (inherited :aboutUrl))
(def ^{:metadata-spec "5.7"} datatype (inherited :datatype datatype/datatype-defaults))
(def ^{:metadata-spec "5.7"} default (inherited :default ""))

;;NOTE: specification defines default as und but using nil as default results in strings without a language as required
(def ^{:metadata-spec "5.7"} lang (inherited :lang))
(def ^{:metadata-spec "5.7"} null (inherited :null [""]))
(def ^{:metadata-spec "5.7"} ordered? (inherited :ordered false))
(def ^{:metadata-spec "5.7"} property-url (inherited :propertyUrl))
(def ^{:metadata-spec "5.7"} required? (inherited :required false))
(def ^{:metadata-spec "5.7"} separator (inherited :separator))
(def ^{:metadata-spec "5.7"} text-direction (inherited :textDirection "inherit"))
(def ^{:metadata-spec "5.7"} value-url (inherited :valueUrl))