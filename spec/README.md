# Spec Tests

This directory and it's subdirectories contain a set of specifications
for the language structured as a selection of source files. Each
source file contains comments which describe assertions about the
compilation.

## Assertions

Assertions are encoded in comments. Comments start with a given sigil
sequence and the rest of the line is the parameter to the assertion.

 * ` # => <output>` - Verify output. All standard output from the
   program must be matched by one of these assertions.
 * ` # !> <error>` - Verify error. Checks that the compilation fails
   and that the given error is printed to stderr.
 * ` # !!skip` - Skips running the output. Just checks that the code
   is parsed and compiles.

## Structure

Tests can be arranged into subdirectories to group similar
tests. Subdirectories of interest are:

 * `bench/` - Code benchmarks.
 * `malformed/` - Broken input that shouldn't parse correctly.
 * `fail/` - Syntactically valid code that is semantically invalid and
   should fail compilation.
