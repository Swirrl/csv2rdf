# csv2rdf

[![Build Status](https://travis-ci.org/Swirrl/csv2rdf.svg?branch=master)](https://travis-ci.org/Swirrl/csv2rdf)

Clojure library for converting [CSV to RDF](https://www.w3.org/TR/2015/REC-csv2rdf-20151217/) according to the specifications for [CSV on the web](https://w3c.github.io/csvw/)

## Building

Standalone JARs can be built using [leiningen](https://leiningen.org/). After installing leiningen an uberjar can be built by running the following in the project root directory:

    lein uberjar
    
This will create a `target/csv2rdf-VERSION-standalone.jar` which can be run.

## Running

csv2rdf can be run from the command line given the location of either a tabular data file or metadata file referencing the described tabular file. The location
can be either a path on the local machine or URI for the document on the web.

To run from a tabular file:

    java -jar csv2rdf-standalone.jar -t /path/to/tabular/file.csv

The resulting RDF is written to standard output in [turtle](https://www.w3.org/TR/turtle/) format. The output can instead be written to file with the -o option:

    java -jar csv2rdf-standalone.jar -t /path/to/tabular/file.csv -o output.ttl

The extension of the output file is used to determine the output format. The full list of supported formats is defined by [rdf4j](http://docs.rdf4j.org/programming/#_detecting_the_file_format),
some common formats are listed below:

| Extension | Format                                               |
| --------- | -----------------------------------------------------|
| .ttl      | [turtle](https://www.w3.org/TR/turtle/)              |
| .nt       | [n-triples](https://www.w3.org/TR/n-triples/)        |
| .xml      | [rdf-xml](https://www.w3.org/TR/rdf-syntax-grammar/) |
| .trig     | [trig](https://www.w3.org/TR/trig/)                  |
| .nq       | [n-quads](https://www.w3.org/TR/n-quads/)            |

Note that for quad formats like trig and n-quads the graph will be nil.

The triples are generated according to CSVW standard mode by default. The mode to use can be specified by the -m parameter:

    java -jar csv2rdf-standalone.jar -t /path/to/tabular/file.csv -m minimal

The supported values for the mode are `standard` and `minimal` and `annotated`. `annotated` mode is a non-standard mode which behaves like
`minimal` mode with the addition that any notes or non-standard annotations defined for table groups and tables will be output if the
corresponding metadata element specifies an `@id`.

The recommended way to start processing a tabular file is from a metadata document that describes the structure of a referenced tabular file. The tabular file does not
need to be provided when processing from a metadata file since the metadata should contain a reference to the tabular file(s).

    java -jar csv2rdf-standalone.jar -u /path/to/metadata/file.json -o output.ttl

## Using as a library

csv2rdf also exposes its functionality as a library - please see [the csv2rdf library](doc/library.md) for a description of the library and its interface.

## Overview of the code

See [overview of the code](doc/code.md) for an overview of the codebase.

## License

Copyright © 2018 Swirrl IT Ltd.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
