# Getting Started

This is only known to work on my machine at the moment. I'm running macOS and stable Rust If you'd still like to give it a go then make sure you have:

 * Rust - to compile the compiler
 * Just - to run help commands
 * Python - to run the functional tests
 * Clang - Used to link the output to create the final executables.

The compiler is written in Rust and built with *Cargo*. Functional tests are defined by a collection of source in `spec/` and run by `specs.py`. It's easier to run the tests with a virtual environment. Rather than running `cargo` or python directly use the `build.sh` script. This will set up the required python environment as required.

## Source

The source is all available [at GitHub](https://github.com/iwillspeak/ullage). Pull requests, comments and issues are welcome. Any thoughts I find interesting during development might be posted to [my blog](http://willspeak.me/).

## Building & Running the Compiler

The compiler can be built with `./build.sh build --release` from the root directory. This should create a `target/release/ullage` executable. With this executable in your path you can compile an example program with `$ ullage -o hello specs/hello.ulg`. This will produce an executable called `hello` in the current directory based on the source file `specs/hello.ulg`. Running `$ ./hello` should then print 1337 to the terminal. For full usage instructions on the compiler run `$ ullage --help`.
