# Parsing and Syntax Trees

The current parsing architecture works but doesn't provide enough metadata to the parser in the token stream. The parser then fails to pass on any metadata to the AST.

[TOC]

## Current

A `Tokeniser` is created from a string. This iterates over the `char`s in the string and yields tokens. Word tokens in the stream have their value copied out of the source buffer.

There are a few downsides to this:

 1. We are stuck with only being able to parse strings. Would be nice to abstract over the input so that string buffers and files can be parsed.
 2. The value of each token is only extracted for `Word` tokens. It would be nice to access the raw value for all tokens.
 3. Whitespace is lost in the AST. Richer whitespace and other trivia tokens would allow better error reporting and round-tripping.
 4. The position of each token is lost. It would be nice to keep the tokens around in the AST so that their position information is available.
 5. Parsing is limited to a single source. It would be nice to add more than one syntax tree to a compilation.

## Proposed Replacement

Source will be represented by a `SourceText` type. This will expose the  characters at a given index along with the ability to take a position and convert it into a file, line, column triple.

Parsing will crate a lexer which implements token iteration for a given `SourceText`. Each token should be a pair of a `Position` and `TokenKind`. Consumption of tokens from the iterator by the parser should stub out missing tokens as well as recording errors in a `DiagnosticCollection`.

Rather than returning a `Result` type from the parser instead a valid parse should always be returned. At the end if there are any diagnostics an error type containing those messages and the parsed tree with its substitutions should be returned.

### Structure

The structure could look something like this:

 * `syntax/parse/` - Parser and tokenisation logic
 * `syntax/tree/` - Tree node types and the `SyntaxNode` enum.
 * `syntax/text/` - Source buffer abstraction as well as diagnostic pretty printing.
 * `diagnostics/` - Diagnostic collection implementation.

### Diagnostics

Initially diagnostics could just be string values and position information.  More metadata can be added later. It might be useful to add 'level' information to each diagnostic to allow for warnings.
