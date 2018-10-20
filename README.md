# Ullage

Ullage started as an attempt to implement a top-down operator precedence parser in Rust. It has since started developing into a small language. Feel free to have a poke around but don't expect much to work at the moment.

## Current Status

It is now possible to write simple programs. The following program computes 9 factorial:

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

## License

Ullage is open source, under the [MIT License](LICENSE.md).

## Features and Progress

 * [ ] Custom data structures
 * [ ] Pattern matching
 * [ ] First-class functions

### Lexer

 * [x] Recognise words, numbers, comments, operators and whitespace
 * [ ] Position information on each token
 * [ ] Interpolated strings
 * [ ] Expose whitespace to the parser

### Parser

 * [x] Parse base constructs
 * [ ] For loops and iterators
 * [ ] Traditional `if` blocks
 * [ ] Keep track of _all_ underlying tokens
 * [ ] Expose position & span information
 * [ ] Round-trippable/pretty-printable

### Code Generation / Lowering

 * [x] Create LLVM module and lower basic constructs
 * [ ] Array indexing
 * [ ] Arbitrary types for local variables
 * [ ] Heap allocated types
    * [ ] Lowering of `String` type
    * [ ] User-defined heap allocated types
    * [ ] Garbage collection ??
 * [ ] Library output types
 * [ ] Control of target machine & features
 * [ ] Stop shelling out for linking on supported platforms (lld or similar)