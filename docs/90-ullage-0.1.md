# Ullage 0.1

In this fist version of Ullage we will keep things nice and simple. The language just needs a few simple types and bits of built-in functionality to get us off the ground. It's not intended that all of these things will be part of the final language, but right now each brings its own value to the table.

## Types

We're going to start with just three types:

- `Bool` along with the two laterals `true` and `false`
- `Number` 64 bit integer, along with basic decimal literals
- `String` along with `'`-quoted *raw* literals.

These should be enough to express a wide variety of programs and allow us to explore the language.

## Control Flow

When it comes the the actual language itself we will stick to a few basic constructs. These should allow us to begin defining the semantics of be language as well as allowing enough expressibility and flexibility for future development.

- **Control Flow**: we'll start off with just `if`/`unless`/`else` ternaries for conditions and `while` for looping.
- **Function**, or `fn` to its friends. This allows us to group code so we can call it later.

## Printing

A simple addition to the early language, and not intended to stay the course. `print` should allow us to develop an array of functional test cases quickly before getting the whole language runtime up on its feet.

## Variables

To begin with we just need two kinds of variables: `var` and `let`. Both will take a type annotation and an expression of that type. The difference between the two is just mutability. Neither should allow you to define an uninitialised variable at this stage.

## Compilation

The language itself will be compiled with LLVM. The output should be something which is independently runnable. For initial versions the compiler can just produce LLVM IR. A small language *runtime* will be needed to support the print statement and manage calling the entry point. This runtime support can initially be a small Rust stub with a c api. In the future it would be nice to have an interactive REPL.

## Future

### Jit?

The compilation part of the JIT should be usable to perform AOT
compilation. That is we should build the JIT on top of the AOT
compilation support. Maybe look at CraneLift for this?

## Types

We want the following built in types:

 * Integer/Number (arbitrary precision?)
 * String
 * Array
 * Function
 * Tuples, (including the unit tuple?)

As far as further user defined types I'd like a full algabraic data type set with sum and product types. Unsure if this should be through asigning names to unknown types or with an explict syntax as in Rust. E.g.:

```ullage
type Foo = Number | String

# or
enum Foo {
	Number(Number),
	String(String),
}
```

The type system should support generics. Given generics the following can be built from those primitives:

 * Map
 * Linked list
 * Range

It should also be possible to define 'trait' or 'interface' types which can be implemented by concrete types. Not sure if these should be rust-like or more traditional.
