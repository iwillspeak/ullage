# Getting Started

This is only known to work on my machine at the moment. I'm running macOS and stable Rust If you'd still like to give it a go then make sure you have:

 * Rust - to compile the compiler
 * Python - to run the functional tests
 * Clang - Used to link the output to create the final executables.

The compiler can be built with *Cargo*. Functional tests are defined by a collection of source in `spec/` and run by `specs.py`. It's easier to run them with Python's invoke however. Unit and functional test can be run with `invoke test`.

## Source

The source is all available [at GitHub](https://github.com/iwillspeak/ullage). Pull requests, comments and issues are welcome. Any thoughts I find interesting during development might be posted to [my blog](http://willspeak.me/).

## Building & Running the Compiler

The compiler can be built with `cargo build --release` from the root directory. This should create a `target/release/ullage` executable. With this executable in your path you can compile an example program with `$ ullage -o hello specs/hello.ulg`. This will produce an execuable called `hello` in the currrent directory based on the source file `specs/hello.ulg`. Running `$ ./hello` should then print 1337 to the terminal. For full usage instructions on the compiler run `$ ullage --help`.
