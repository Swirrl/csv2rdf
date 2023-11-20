# csv2rdf

[![CircleCI](https://circleci.com/gh/Swirrl/csv2rdf/tree/master.svg?style=svg)](https://circleci.com/gh/Swirrl/csv2rdf/tree/master)

Command line application (and clojure library) for converting [CSV to RDF](https://www.w3.org/TR/2015/REC-csv2rdf-20151217/) according to the specifications for [CSV on the web](https://w3c.github.io/csvw/).

## Native Builds

We provide CI generated native builds for Linux (AMD64) and MacOS (AMD64) of the `csv2rdf` command line app attached to [releases](https://github.com/Swirrl/csv2rdf/releases).

## Running

There are two main functions for `csv2rdf`

1. [RDFization](#RDFization)
2. [Validation](#Validation) of CSV data against a `csvw:TableSchema`.

### RDFization

`csv2rdf` can be run from the command line given the location of either a tabular data file or metadata file referencing the described tabular file. The location
can be either a path on the local machine or URI for the document on the web.

Assuming you are using a native build you can run from a tabular file like this:

    csv2rdf -t /path/to/tabular/file.csv

The resulting RDF is written to standard output in [turtle](https://www.w3.org/TR/turtle/) format. The output can instead be written to file with the -o option:

    csv2rdf -t /path/to/tabular/file.csv -o output.ttl

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

    csv2rdf -t /path/to/tabular/file.csv -m minimal

The supported values for the mode are `standard` and `minimal` and `annotated`. `annotated` mode is a non-standard mode which behaves like
`minimal` mode with the addition that any notes or non-standard annotations defined for table groups and tables will be output if the
corresponding metadata element specifies an `@id`.

The recommended way to start processing a tabular file is from a metadata document that describes the structure of a referenced tabular file. The tabular file does not
need to be provided when processing from a metadata file since the metadata should contain a reference to the tabular file(s).

    csv2rdf -u /path/to/metadata/file.json -o output.ttl

The RDFization mode will validate the data, and report any errors to the console.  Currently by default validation errors are ignored, if you want to fail on a validation error then you should run a pass over the data with the `--validate-data` flag set.

### Validation

You can also use `csv2rdf` to validate data against a CSVW TableSchema, by using the `--validate-data` flag.  In this mode `csv2rdf` will only perform validation, reporting errors to stdout.  Additionally it will return a non-zero exit code of `2` if any validation failures are found.

    csv2rdf -t /path/to/data.csv -u /path/to/csv-schema.json --validate-data

### Running with docker

Docker images are published to the public repository `europe-west2-docker.pkg.dev/swirrl-devops-infrastructure-1/public/csv2rdf`.
These can be run by specifying the image version to run, and mapping volumes into the container to make local files available within
the container e.g.

    docker run --rm -v .:/data europe-west2-docker.pkg.dev/swirrl-devops-infrastructure-1/public/csv2rdf:v0.7 -t /data/input.csv -o /data/output.ttl

Note that file paths should be specified relative to the container, not the local system.

## Using as a library

csv2rdf also exposes its functionality as a library - please see [the csv2rdf library](doc/library.md) for a description of the library and its interface.

- See [overview of the code](doc/code.md) for an overview of the codebase.
- See [Developing csv2rdf itself](doc/developing.md) for a quickstart guide on how to work on the library and application itself.

## Deploying new builds

In order to compile and deploy new native image builds for all our supported architectures, just create a release in the Github UI tagged to a commit.

## License

Copyright © 2018 Swirrl IT Ltd.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
