# Parsing and Syntax Trees

The parser takes an input `SourceText` and produces a full-fidelity `SyntaxTree`. In theory each character in the source exists as a `Token` or `TriviaToken` within this tree. This initial tree is intended to provide as rich a possible model of the underlying source text to allow for syntax transformations in the future. It is later transformed into an abstract representation of the semantics of the code by the `sem` module.

[TOC]

## Structure

Source is represented by a `SourceText` type. This exposes characters
as `&str` slices or via the `walk_chars` method for tokenisation. The
source text also contains line information and has the ability to take
a position and convert it into a line, column pair.

Parsing creates a lexer which implements token iteration for a given
`SourceText`. Each token has a `Span` and `TokenKind`. Consumption of
tokens from the iterator by `Parser::expect` stubs out missing tokens
as well as recording errors in a collection of `Diagnostic`s.

Rather than returning a `Result` type from the parser instead a valid
`SyntaxTree` is always be returned. It is the client's responsibility to inspect the tree for a given parse for diagnostics and act accordingly. This allows clients to parse malformed source text and should allow the compiler to produce more error information in each pass.

### Structure

The structure looks something like this:

 * `syntax/parse/` - Parser and tokenisation logic
 * `syntax/tree/` - Tree node types.
 * `syntax/text/` - Source buffer abstraction.
 * `diag.rs` - Diagnostic implementation.

### Diagnostics

At the moment diagnostics are just string values and position
information. More metadata can be added later. It might be useful to
add 'level' information to each diagnostic to allow for warnings.

It would be nice to have some kind of diagnostic pretty printing
rather than relying on the `main` method to walk the diagnostics and
write them to stderr manually.
