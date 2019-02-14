# Thoughts..

This is just a scratchpad for ongoing work on this arboreal branch.

## Types Need Tokens

Currently we aren't keeping track of the tokens in the `TypeRef`
structure. Once we do it should be possible to add `#[must_use]` to
`Parser::advance`.


## Round-tripping PIC

We could have a visitor for the tree which writes the tree to the
standard output to prove out the round-tripping. Add a new set of
tests which just round-trips all of the code from the specs.

API surface could be similar to Minsk's `Compilation::EmitTree`. I'm
thinking we come up with a trait something like `TreeSink` and
implement it for `IO::Write` or similar. Can then have a similar
`emit_tree` which walks the tree and pushes it to the writer.

I'm thinking we might also want to have this tree emission part of a
higher level `--emit=` flag on the compiler front end. Other values
for now would be to emit the `Debug` version of the parsed tree and to
emit the compilation result as IR, an object file or a linked
executable.

## Syntax Transformations

With input trees immutable we could have a syntax transformer which
uses the visitor pattern. Would then have a `visit_` for each kind of
`Expression` and have them produce a new expression. New expressions
would have synthesised `Span` information.

Initial optimisations/transformations would be a desugaring pass over
this AST before lowering to the `sem` tree. Maybe even want to do the
qtype check pass at this level and pass a fully-formed type checked
tree to `sem`. If we do add a desugaring pass then the output of that
would be nice to have as an `--emit` value.

This would be useful to observe the desugaring of things like `for`
into `while`.

Further use of these would be to implement a language server in the
future and some kind of code formatter.

## Mangling

We need to come up with some way of mangling the names of functions to
support modules. Would probably make sense to use the Itanium ABI like
everyone else. Can't find a crate which provides mangling for it
though, just demangling.

