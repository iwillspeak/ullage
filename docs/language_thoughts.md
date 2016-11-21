# Jit Thoughts

The compilation part of the JIT should be usable to perform AOT
compilation. That is we should build the JIT on top of the AOT
compilation support.

We should have a wrapper around the low level (`llvm-sys`) bindings.

All compliation starts with a context object. Modules should be
registered with this context. We then add functions to the module,
basic blocks to the functions.
   
Given a basic block we can build in by creating a builder from the
context. For efficiency we can just have a single builder and share it
amongst all of the builder calls.

# Types

We want the following built in types:

 * Integer/Number (arbitary precision?)
 * String
 * Array
 * Function
 * Tuples, (including the unit tuple?)

There is also the biltin type `Any`, which all types will inherit
from?

Given generics the following can be built from those primitives:

 * Map
 * Linked list
 * Range
