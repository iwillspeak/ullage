# Benchmarking

This folder contains files which can be used to benchmark the
performance of the generated code. Each file should have a `#!!skip`
attribute so that they aren't run when the spec tests are executed.

The benchkarks can be run with `./build.sh bench` from the project
root.

## `fib.ulg`

This benchmark performs the same Fibonacci computation from
<https://github.com/drujensen/fib> to allow some form of comparision
with popular lanugages.
