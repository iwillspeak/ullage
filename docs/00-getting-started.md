# Getting Started

This is only known to work on my machine at the moment. I'm running macOS and stable Rust If you'd still like to give it a go then make sure you have:

 * Rust - to compile the compiler
 * Python - to run the functional tests

The compiler can be built with *Cargo*. Functional tests are defined by a collection of source in `spec/` and run by `specs.py`. It's easier to run them with Python's invoke however. Unit and functional test can be run with `invoke test`.

## Source

The source is all available [at GitHub](https://github.com/iwillspeak/ullage). Pull requests, comments and issues are welcome. Any thoughts I find interesting during development might be posted to [my blog](http://willspeak.me/).
