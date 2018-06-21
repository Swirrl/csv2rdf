# Overview of the code

csv2rdf is an implementation of the [Generating RDF from Tabular Data on the Web](https://www.w3.org/TR/2015/REC-csv2rdf-20151217/) specification. This specification
in turn directly depends on two main specifications: [Model for Tabular Data and Metadata on the Web](https://www.w3.org/TR/2015/REC-tabular-data-model-20151217/)
and [Metadata Vocabulary for Tabular Data](https://www.w3.org/TR/2015/REC-tabular-metadata-20151217/). These specifications themselves also rely on other specifications -
all specifications required for the implementation of csv2rdf are listed in the External Specifications section.

The majority of the code implements [Metadata Vocabulary for Tabular Data](https://www.w3.org/TR/2015/REC-tabular-metadata-20151217/),
[Model for Tabular Data and Metadata on the Web](https://www.w3.org/TR/2015/REC-tabular-data-model-20151217/) and
[Generating RDF from Tabular Data on the Web](https://www.w3.org/TR/2015/REC-csv2rdf-20151217/) within the `csv2rdf.metadata`, `csv2rdf.tabular` and
 `csv2rdf.csvw` namespaces respectively. An overview of the main concepts within the implementations of those namespaces follows.

## Metadata

The [Metadata Vocabulary for Tabular Data](https://www.w3.org/TR/2015/REC-tabular-metadata-20151217/) specification defines the structure of metadata JSON files along
with how they should be processed and validated. Metadata documents are traversed from the root document down to the leaves (i.e. primitive JSON values: strings,
numbers and booleans) and then validated and normalised from the leaves back up to the root. Each part of the document is processed by a 'validator' function
responsible for validating the input and normalising the output.

### Validators

Conceptually a validator is a function `Validator[TOut](ParseContext, JSONValue & ErrorHandler): (TOut | invalid)` i.e. a function which accepts two or three arguments and
returns either the normalised output or a special `invalid` value indicating an error. Later validators may handle invalid values by e.g. using a specified
default value or ignoring the property. Parsed metadata documents should not contain any instances of the `invalid` value. Validators should throw an exception to indicate
an error (.e.g if a required property is missing), or log a warning message if the invalid input is non-fatal.

### Combinators

Validator functions are composed using the combinators defined in the `csv2rdf.metadata.validator` namespace. Metadata documents are then validated using either the
`table-group` or `table` validators depending on the keys contained in the root metadata object.

#### ParseContext

The ParseContext, defined in `csv2rdf.metadata.context` contains information about the item current being validated such as its location within the document or the URI
of the containing metadata document. The parse context is updated during parsing by the JSON-LD context of contained metadata documents as well as by validators for
composite JSON types (objects and arrays). The path of the validating item describes its location within the metadata document, for example the value 4

    {"docs": [{"key1": 1, "key2": "foo"}, {"key1": 4}, {"key1": 3, "key2": "bar"}]}

has path `["docs", 1, "key1"]` i.e. the value for the `key1` key within the object at index 1 within the array under the `docs` key in the root object.

#### ErrorHandler

Some validators need to behave differently on invalid input in different circumstances e.g. sometimes an invalid URI value is fatal, while sometimes a default value
can be returned after logging a warning. Such validators can be parameterised by an error callback function responsible for notifying the error. Error callbacks should
have signature `ErrorHandler[TOut](String): (TOut | invalid)` i.e. they should take a single parameter string describing the error and return a default value, invalid,
or raise an exception if the error is fatal.

## Tabular data

The [Model for Tabular Data and Metadata on the Web](https://www.w3.org/TR/2015/REC-tabular-data-model-20151217/) specification defines how metadata documents
should be located and loaded before being used to parse and validate tabular data files. The metadata location process is implemented in `csv2rdf.tabular.metadata`.
Tabular data is parsed into a lazy sequence of maps representing a single data record within the input file according to the structure defined in the metadata file.

### Cell parsing

The `csv2rdf.tabular.cell` namespace is responsible for parsing input cell values according to the column datatypes defined in the metadata document. Cell values are
converted to appropriate java types depending on the column datatype. Parsed values are then validated according to any size or range constraints for the datatype.

### CSV Reader

The `csv2rdf.tabular.csv.reader` namespace implements a parser for CSV files according to the tabular specification. The reader returns a lazy sequence of maps
describing the raw CSV data according to the given CSV dialect. For example the input file

| col1 | col2 |
|------|------|
| a    | b    |
| c    | d    |

will return a sequence like

    ({:source-row-number 1, :content "col1,col2", :comment nil, :cells ["col1" "col2"], :type :data}
     {:source-row-number 2, :content "a,b", :comment nil, :cells ["a" "b"], :type :data}
     {:source-row-number 3, :content "c,d", :comment nil, :cells ["c" "d"], :type :data})

### Row parsing

The `csv2rdf.tabular.csv` namespace transforms each row record from the reader according to the structure described by the metadata document. This includes parsing
cell values and deriving cell URIs from property templates.

## CSVW

The [Generating RDF from Tabular Data on the Web](https://www.w3.org/TR/2015/REC-csv2rdf-20151217/) specification defines how to create RDF from a tabular data file
and associated metadata. This is implemented within the `csv2rdf.csvw` namespaces - the minimal and standard modes are implemented by the `csv2rdf.csvw.minimal` and
`csv2rdf.csvw.standard` namespaces respectively.

### Grafter

[Grafter](http://grafter.org/) is a clojure library for processing RDF data. The `CSVW` functions output a lazy sequence of Grafter statement records.

## External specifications

The code contains (not necessarily complete) implementations of various specifications required to implmement the CSVW conversion process. For smaller specifications
these are implemented in a single namespace, while larger specifications are split across multiple sub-namespaces. Functions within these namespaces may make reference
to a particular section of the specification they implement. Such functions are tagged with a metadata item indicating the specification/section they implement e.g.

```clojure
(defn ^{:metadata-spec "5.1.2"} link-property ...)
```

This indicates the `link-property` function implements some part of section 5.1.2 of the "Metadata Vocabulary for Tabular Data" specification.
Below is the list of specifications implemented referenced within the code:

| Specification                                                                                                  | metadata key       | namespace            |
| ---------------------------------------------------------------------------------------------------------------|--------------------|----------------------|
| [Generating RDF from Tabular Data on the Web](https://www.w3.org/TR/2015/REC-csv2rdf-20151217/)                | `:csvw-spec`       | csv2rdf.csvw         |
| [Model for Tabular Data and Metadata on the Web](https://www.w3.org/TR/2015/REC-tabular-data-model-20151217/)  | `:tabular-spec`    | csv2rdf.tabular      |
| [Metadata Vocabulary for Tabular Data](https://www.w3.org/TR/2015/REC-tabular-metadata-20151217/)              | `:metadata-spec`   | csv2rdf.metadata     |
| [W3C XML Schema Definition Language (XSD) 1.1 Part 2: Datatypes](https://www.w3.org/TR/xmlschema11-2/)         | `:xml-schema-spec` | csv2rdf.xml          |
| [JSON LD 1.0](https://www.w3.org/TR/json-ld/)                                                                  | `:json-ld-spec`    | csv2rdf.json-ld      |
| [JSON-LD 1.0 Processing Algorithms and API](https://www.w3.org/TR/json-ld-api/)                                | `:jsonld-api-spec` | csv2rdf.json-ld      |
| [Tags for Identifying Languages](https://tools.ietf.org/html/bcp47)                                            | `:bcp47-spec`      | csv2rdf.bcp47        |
| [Number Format Patterns](http://www.unicode.org/reports/tr35/tr35-31/tr35-numbers.html#Number_Format_Patterns) |                    | csv2rdf.uax35        |
| [URI Template](https://tools.ietf.org/html/rfc6570)                                                            |                    | csv2rdf.uri-template |