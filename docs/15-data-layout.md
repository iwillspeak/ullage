# Data Layout

For the initial version of the language we just need `Bool`, `Number` and `String` to have defined layout. It is probably worth thinking about the future structure of arrays, tuples, and structs though.

## Value and Reference Semantics

The plan is that `Bool`, `Number`, and `String` will all have value semantics. That is a modification of a `String` value in one place will not affect its appearence in another. Such behaviour is referred to as "value semantics". This is similar to `Copy` types in rust and `struct` types in C#. I feel that tuple types should also have value semantics. Similar to `ValueTuple` in C#.

Array and structure types instead will have reference semantics. This means that passing a structure to a `fn` will allow the function to modify the structure value. This is similar to refernece types in C# and `&mut` reference in Rust.

## Type Layouts

For the primitive types we have the following type layouts from language type to LLVM type:

 * `Bool` -> `i1`
 * `Number` -> `i64`

String types should be represented as a pair of length, data:

 * `String` -> `<{u32,[0 x u8]}>`

This would have the value of the string be encoded directly as part of the pair. Allocation of a string would use a variable length array to contain a sequence of utf-8 characters. There are a few problems with this:

 * It sounds like a lot of copies of the string data might be needed. Especially when values are assigned and returned from multiple levels of call heirachy.
 * The expectation is that strings are rarely modified and we could probably share a single buffer between string instances and use reference counting to control mutable acces.
 * This needs some knowledge of when a value is 'dropped' to free the correct amount of memory.
 
 Given these concernes we could lay a string out as:
 
  * `String` -> `<{u32, u32, [0 x u8]}>*`

In this representation each string has a pointer to a reference counted backing buffer. This should reduce copy-size of each string and means that a string reference would again have a single easily known size. We still need to know when the reference should be deallocated however.

The other thing to think about here is are 32 bit rc and length too big? Can we get away with 16 bit numbers for string length? For refcount we could always detect the overflow on retain and dupe the value as-needed to maintain correctness.

## Garbage Collection

Rather than aiming to control access to data as Rust does the language should provide a garbage collection mechanism to clean up data once no one references it. There are a few alternatives for this:

 * Don't deallocate - Probably useful to get us off the ground
 * Reference counting. E.g. Swift's ARC & Python.
 * Simple mark & sweep GC.
 
 For a full mark and sweep or other collector the code generated needs to insert GC statepoints. For this reason i'm tempted to head towards the second option. I place of statepoints we will need to decide in the lower pass where to insert RC retain and release code to maintain the count. Could this work the same way as Rust's ARC model? In that case the code just needs to know a fixed point in the scope where each value is deallocated.
