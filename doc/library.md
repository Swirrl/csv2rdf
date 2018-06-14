# The csv2rdf library

The entry point in the csv2rdf library is the `cvs2rdf.csvw` namespace and the public functions defined there. The `csv2rdf.csvw/csv->rdf`
function is the primary interface into the library:

```clojure
(defn csv->rdf
  ([tabular-source metadata-source])
  ([tabular-source metadata-source options] ...))
```

At least one of `tabular-source` and `metadata-source` must be non-nil. If `metadata-source` is specified then processing will start from the
referenced metadata document.

### Metadata

The `metadata-source` argument must support two operations - retrieving a URI which identifies the document, and loading the contained JSON
document as a clojure map. These two operations are defined by protocols in the `csv2rdf.source` namespace.

#### URIable

The `csv2rdf.source\URIable` protocol represents resources with an associated URI. Implementations for `java.net.URI` and `java.io.File`
are provided.

#### JSONSource

The `csv2rdf.source\JSONSource` protocol represents a source a JSON map can be loaded from. Implementations for `java.net.URI`, `java.io.File`
and `java.lang.String` are provided.

Since both `java.net.URI` and `java.io.File` implement both the required protocols, these types can already be used to specify the location
of the metadata document.

### Tabular source

The `tabular-source` argument should be either a `java.net.URI` or `java.io.File` identifying the tabular data.

### Return type

`csv2rdf` uses [grafter](http://grafter.org/) for representing RDF data. The `csv->rdf` function returns a lazy sequence of Grafter statements - see
the [grafter documentation](http://api.grafter.org/docs/master/grafter.rdf.html) for how to access the components of the result statements.

### Options

The options map can be used to specify which CSVW mode to run in - either standard (the default) or minimal. This is configured by specifying
the `:mode` key e.g.

```clojure
  (csv->rdf tabular-source nil {:mode :minimal})  ;; or {:mode :standard}
```

