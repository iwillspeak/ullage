# Ullage

[![Build Status][build_status_image]][build_status]

A statically-typed compiled language defined by a simple grammar.

## Current Status

It is now possible to write simple programs. The following program
computes 9 factorial:

    fn fact(n: Number): Number
        var acc = 1
        var i = 1
        while i < n
            acc = acc * i
            i = i + 1
        end
        acc
    end
    
    print fact(9) # => 362880

You can also mess around with constant strings:

    # FizzBuzz
    #
    # Implementation of the legendary `FizzBuzz` algorithm.
    fn fizzbuzz(n: Number): String
        (print 'fizzbuzz') if mod(n, 15) == 0 else
        (print 'fizz') if mod(n, 3) == 0 else
        (print 'buzz') if mod(n, 5) == 0 else
        print_num(n)
    end

## Building and Testing

The main build is performed by `cargo`. For running the functional
tests and benchmarks you'll need Python and to `cargo install just`. The suggested process is to
use the `build.sh` script:

 * `$ ./build.sh` will build the compiler `target/release/ullage`.
 * `$ ./build.sh test` will build the compiler and run the test suite
   from `specs/`.
 * `$ ./build.sh bench` will run the benchmarks from `spec/bench/`.

## License

Ullage is open source, under the [MIT License](LICENSE.md).

## Features and Progress

 * [ ] Custom data structures
 * [ ] Pattern matching
 * [ ] First-class functions

### Lexer

 * [x] Recognise words, numbers, comments, operators and white-space
 * [x] Position information on each token
 * [ ] Interpolated strings
 * [x] Expose whitespace to the parser

### Parser

 * [x] Parse base constructs
 * [ ] For loops and iterators
 * [ ] Traditional `if` blocks
 * [x] Keep track of _all_ underlying tokens
 * [x] Expose position & span information on syntax nodes
 * [ ] Round-trippable/pretty-printable trees

### Code Generation / Lowering

 * [x] Create LLVM module and lower basic constructs
 * [ ] Array indexing
 * [x] Arbitrary types for local variables
 * [ ] Heap allocated types
    * [x] Lowering of `String` type
    * [ ] User-defined types
    * [ ] RC garbage collection (#26)
 * [ ] Library output types (LLVM ir, LLVM bc, object, staticlib, dylib, exe)
 * [x] Control of target machine & features
 * [x] Optimisation
 * [ ] Linker support:
   * [x] `clang` - macOS linker default
   * [ ] gold - GNU ld
   * [ ] lld/llvm-link
   * [ ] Microsoft LINK


 [build_status_image]: https://dev.azure.com/iwillspeak/GitHub/_apis/build/status/iwillspeak.ullage?branchName=main
 [build_status]: https://dev.azure.com/iwillspeak/GitHub/_build/latest?definitionId=2&branchName=main
