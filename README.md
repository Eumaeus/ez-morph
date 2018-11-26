# ez-morph

A basic framwork for documenting Greek vocabulary and morphology. Very much a work in progress.

Licensed under the [GPL 3.0](http://www.opensource.org/licenses/gpl-3.0.html)

## Running

Requires [SBT](https://www.scala-sbt.org), and [Pandoc](https://pandoc.org).

- Navigate to the top level of this directory.
- Do `sbt console`
- Then do `:load tools.sc`

Various libraries and definitions will load, ending with a listing of some examples of what you can do.

- `:quit` to get out of SBT.

The data files that constitute a library of morphology are:

- `data/lexicon.cex`
- `data/forms.cex`

The first row of each identifies the fields; the separator is `#`. You can generate Part-of-Speech tags with the [POS Generator](http://folio.furman.edu/pos/).


