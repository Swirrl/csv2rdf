(ns csv2rdf.xml.datatype.canonical
  (:require [csv2rdf.xml.datatype :as xml-datatype]
            [csv2rdf.util :as util])
  (:import [java.util Base64]
           [javax.xml.datatype XMLGregorianCalendar Duration]
           [java.time LocalDate LocalDateTime ZonedDateTime LocalTime OffsetTime OffsetDateTime]
           [java.time.format DateTimeFormatter]
           [java.net URI]))

(defprotocol ToXmlRepresentation
  (canonical-value [this type-name]))

(extend-protocol ToXmlRepresentation
  String
  (canonical-value [s _type-name] s)

  URI
  (canonical-value [uri _type-name] (str uri))

  XMLGregorianCalendar
  (canonical-value [cal _type-name] (.toXMLFormat cal))

  Boolean
  (canonical-value [b _type-name] (str b))

  LocalDate
  (canonical-value [date _type-name]
    (.format DateTimeFormatter/ISO_DATE date))

  LocalDateTime
  (canonical-value [datetime _type-name]
    (.format DateTimeFormatter/ISO_DATE_TIME datetime))

  ZonedDateTime
  (canonical-value [datetime _type-name]
    (.format DateTimeFormatter/ISO_ZONED_DATE_TIME datetime))

  OffsetDateTime
  (canonical-value [datetime _type-name]
    (.format DateTimeFormatter/ISO_OFFSET_DATE datetime))

  LocalTime
  (canonical-value [time _type-name]
    (.format DateTimeFormatter/ISO_TIME time))

  OffsetTime
  (canonical-value [time _type-name]
    (.format DateTimeFormatter/ISO_OFFSET_TIME time))

  Duration
  (canonical-value [duration type-name]
    (.toString duration))

  Number
  (canonical-value [num _type-name]
    (str num)))

(extend-protocol ToXmlRepresentation
  (class (byte-array 0))
  (canonical-value [^bytes arr type-name]
    (case (xml-datatype/dispatch-key type-name)
      :base64Binary (.encodeToString (Base64/getEncoder) arr)
      :hexBinary (util/to-hex-string arr)
      (throw (IllegalArgumentException. (format "Unknown binary type '%s'" type-name))))))
