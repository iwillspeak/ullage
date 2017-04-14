# Structure of the Repository

This is an outline of the planned structure of the repository. It details the general layout, the Rust module hierarchy,  and some of the significant classes.

## Important Folders

The language parser and compiler are written in Rust. The source lives in the `src/` folder.

Functional tests for the language consist of a set of source files in `spec/`. These files contain specially formatted comments which are interpreted by the Ruby test runner `specs.rb` when run.

Documentation, written in Markdown, is in the `docs/` folder. It can be compiled into a static side with [`d`](https://github.com/sjl/d).

## Rust modules

There are four main rust modules in the project. Three of them (`syntax`, `sem` and `compile`) correspond to the main stages of the compilation pipeline. The final module contains abstractions around LLVM itself.

### `syntax`

This module contains all of the logic required to parse source text into a syntax expression tree (AST).

* `parse/`
	* `Source` - Represents a source of tokens for parsing.
	* `TokenIterator` - A state machine which consumes a source buffer and produces `Token`s.
	* `Parser` - Consumes a `TokenIterator` and produces an `Expression` tree.
* `ast/`
	* `Token` - Represents a single lexeme in the language.
	* `TokenType` - A rust enum representing the different types of tokens.
	* `Expression` - represents a node in the syntax expression tree. Expressions are composed from one or more `Token`s
	* `TypeRef` - Represents a reference to a type. This could be a simple type like `Num`, or a more complex one like `[Num]`.

## `sem`

This module is responsible for semantic analysis. It contains a more detailed semantic expression tree, a syntax expression tree visitor to transform a syntax tree into a semantic tree, and a set of transforms which can be applied to the semantic tree.

The semantic expression tree contains metadata about types and has each identifier resolved.

## `compile`

This module is responsible for lowering and evaluating semantic expression trees.

* `Compiler` - An expression visitor which walks `sem::Expression`s and compiles them into native modules.
* `Lowerer` - The context required when lowering a given expression tree to LLVM IR.

## `low_loader`

This module contains high-level safe wrappers around the LLVM API. It's not intended to expose the whole API. Instead it provides just the modules and abstractions needed to make working with LLVM ergonomic.