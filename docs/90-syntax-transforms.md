# Syntax Transformations

Currently we take the AST and transform it into a semantic tree. To
better support IDE-like features we will need to support more
variations in transformation.

With input trees immutable we could have a syntax transformer which
uses the visitor pattern. Would then have a `visit_` for each kind of
`Expression` and have them produce a new expression. New expressions
would have synthesised `Span` information.

Initial optimisations/transformations would be a desugaring pass over
this AST before lowering to the `sem` tree. Maybe even want to do the
type check pass at this level and pass a fully-formed type checked
tree to `sem`. If we do add a desugaring pass then the output of that
would be nice to have as an `--emit` value.

This would be useful to observe the desugaring of things like `for`
into `while`.

Further use of these would be to implement a language server in the
future and some kind of code formatter.

## Round-tripping POC

Given initial support for syntax transformations we could have a
visitor for the tree which writes the tree to the standard output to
prove out the round-tripping. Add a new set of tests which just
round-trips all of the code from the specs.

API surface could be similar to Minsk's `Compilation::EmitTree`. I'm
thinking we come up with a trait something like `TreeSink` and
implement it for `IO::Write` or similar. Can then have a similar
`emit_tree` which walks the tree and pushes it to the writer.

I'm thinking we might also want to have this tree emission part of a
higher level `--emit=` flag on the compiler front end. Other values
for now would be to emit the `Debug` version of the parsed tree and to
emit the compilation result as IR, an object file or a linked
executable.

## Interaction with `dumpast` and `dumptree`

The current flags for dumping compilation state may be best grouped
under a single `--dump=<type>` flag. This could allow for different
outputs:

 * Plain round-tripped source
 * Formatted source
 * Syntax tree structure
 * Desugared source
 * HTML syntax tree
