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

## Tree Building and Syntax Factories

The current checkparse tests are getting unwieldy. Writing the tree to
match against is a bit of a pain. It would be nice to introduce a
builder API similar to the [Roslyn "quoter" API][roslyn_quoter]. This
allows building more complex trees part by part and stubs in tokens
when trees are not being directly backed by source. This would be
useful when creating replacement trees in a syntax transformation.

## Defining a Parser API

It would be nice if the different areas of the compiler lived in
separate crates within this workspace. This would hopefully mean we
can have API level integration tests for each unit of the compiler in
isolation.

For this each part (`syntax`, `sem` etc.) we would need to have a
clean API. I see an initial split of the _syntax_ related concerns
(source text, parsing and tokenisation) from the _compilation_
concerns (typechecking, binding, and code lowering).

E.g.

* Syntax concerns
 * `SourceText` - represents the input.
  * `from_string` - for direct input
  * `from_file` - for `mmmaping` files?? 
  * `PPS_to_line` - for extracting line information for a `Span`
 * `SyntaxTree` - Parsed syntax element.
  * `parse_compilation_unit` - Parse a whole 'compilation unit'
  * `parse_expression` - Just parse a single expression.
 * `SyntaxVisitor`/`SyntaxWalker` - Trait for walking trees?
  * `walk_*` - default walks for different node kinds.
* Compilation concerns
 * `Compilation` - represent the binding and diagnostic state of an
   expression.

 [roslyn_quoter]: https://roslynquoter.azurewebsites.net/

