# Structure of the Repository

This is an outline of the planned structure of the repository, detailing the Rust module hierarchy and some of the significant classes. The language itself will be split into a few main modules:

## `syntax`

This module contains all of the logic required to parse source text into a syntax expression tree (AST).

* `Expression` - represents a node in the syntax expression tree.
* `TypeRef` - Represents a reference to a type. This could be a simple type like `Num`, or a more complex one like `[Num]`.
* `ExpressionVisitor` - a trait representing an object which will traverse an expression tree and transform each node into a new type.
* `ExpressionTransform` - A trait which simplifies implementing `ExpressionVisitor`s when the visitor’s output type is also `Expression`.

## `sem`

This module is responsible for semantic analysis. It contains a more detailed semantic expression tree, a syntax expression tree visitor to transform a syntax tree into a semantic tree, and a set of transforms which can be applied to the semantic tree.

The semantic expression tree contains metadata about types and has each identifier resolved.

* `Expression` - A node in the semantic expression tree.
* `Type` - A canonical representation of a type.
* `ExpressionVisitor` - A trait which can be used to visit each node in an expression tree.

## `compile`

This module is responsible for lowering and evaluating semantic expression trees.

* `Compiler` - An expression visitor which walks `sem::Expression`s and compiles them into native modules.