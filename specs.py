#! /usr/bin/env python3

import os
import sys
import fnmatch
import subprocess
import re
import collections
import itertools
import threading

Expectations = collections.namedtuple('Expectations', ['expects', 'failure_expects', 'skip_run'])

EXPECT_PATTERN = re.compile(r'#\s?=>\s?(.+)')
EXPECT_ERR_PATTERN = re.compile(r'#\s?!>\s?(.+)')
SKIP_PATTERN = re.compile(r'#\s?!!skip')

class Error(Exception):
    def __init__(self, error):
        self.error = error

class ExitCodeMismatchError(Error):
    def __init__(self, message, code, output):
        message = "{} exit={}, out='{}', err='\n{}'".format(
            message, code, output[0].decode('utf-8'), output[1].decode('utf-8'))
        super(ExitCodeMismatchError, self).__init__(message)

class OutputMissingError(Error):
    def __init__(self, output, expected):
        output = "\n       >> ".join(output.strip().split('\n'))
        expected = '", "'.join(expected)
        msg = 'Expected "{}" in output. \n found >> {}'.format(expected, output)
        super(OutputMissingError, self).__init__(msg)

class OutputMismatchError(Error):
    def __init__(self, expected, actual):
        msg = 'Expected "{}", found "{}"'.format(expected, actual)
        super(OutputMismatchError, self).__init__(msg)

def add_matches(pattern, line, expects):
    match = pattern.search(line)
    if match:
        expects.append(match.group(1))

def parse_spec(path):
    expects = []
    failure_expects = []
    skip_run = False
    with open(path, encoding='utf-8') as f:
        for line in f.readlines():
            add_matches(EXPECT_PATTERN, line, expects)
            add_matches(EXPECT_ERR_PATTERN, line, failure_expects)
            if SKIP_PATTERN.search(line):
                skip_run = True
    return Expectations(expects, failure_expects, skip_run)

def check_output(lines, expects):
    """Check that Output Matches Expectations

    Given a string representing the output of a command and
    a list of expected lines check that the output of the
    command matches.
    """

    lines = lines.strip().split('\n')
    for actual, expected in itertools.zip_longest(lines, expects):
        if not actual or not expected:
            raise OutputMismatchError(expected, actual)
        if actual != expected:
            raise OutputMismatchError(expected, actual)

def check_expected_exit(exit_code, output, expectations):
    """Check the Exit of a Program

    Given the output and exit status of a program make sure we got a
    successful exit, or a non-zero exit code with the expected
    failures in the program's standard error output.
    """

    # if we got killed by a signal we don't want to check for errors
    if exit_code < 0:
        raise ExitCodeMismatchError(
            "Compilation was killed by signal '{}'".format(-exit_code),
            exit_code, output)

    # If we were expecting a compilation failure make sure we got it
    if expectations.failure_expects:
        if exit_code == 0:
            raise ExitCodeMismatchError(
                "Expected failure but compilation succeeded",
                exit_code, output)
        check_compilation_failure(
            output[1].decode('utf-8'), expectations.failure_expects)
    # No compilation failure but we got one.
    elif exit_code != 0:
        raise ExitCodeMismatchError(
            "Expected successfull exit", exit_code, output)

def check_compilation_failure(output, failure_expects):
    """Check Failure Output

    Given the output of a failed compilation command check that any
    failure expectations are met.
    """

    fails = list(failure_expects)
    for line in output.strip().split('\n'):
        # Check we haven't found the source printed out again...
        if EXPECT_ERR_PATTERN.search(line):
            continue
        if fails and fails[0] in line:
            fails.pop(0)
    if fails:
        raise OutputMissingError(output, fails)

def run_spec(path):
    """Compile and Run the Given Spec

    Compiles the spec, examining any compilation errors. If none are
    found then the resulting executable is run and expectations
    from the input file are matched against the output.
    """

    expectations = parse_spec(path)
    out = "specbin/{}".format(os.path.basename(path).split('.')[0])
    compile_cmd = subprocess.Popen(["target/release/ullage", path, "-o", out], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    # Give the compiler 5 seconds to run, and return an error on timeout
    timer = threading.Timer(5, compile_cmd.kill)
    try:
        timer.start()
        output = compile_cmd.communicate()
        exit_code = compile_cmd.returncode
        check_expected_exit(exit_code, output, expectations)
    finally:
        timer.cancel()

    if expectations.skip_run or compile_cmd.returncode != 0:
        return
    run_cmd = subprocess.Popen(out, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output = run_cmd.communicate()
    if run_cmd.returncode != 0:
        raise ExitCodeMismatchError("Expected successfull exit code")
    check_output(output[0].decode('utf-8'), expectations.expects)


def glob_for(path, extension):
    """Glob For Files

    Recursively walks a directory tree and finds files matching a
    given extension. Used to find the files to test.
    """

    ext_glob = '*.{}'.format(extension)
    for root, dirnames, filenames in os.walk(path):
        for filename in fnmatch.filter(filenames, ext_glob):
            yield os.path.join(root, filename)

def main(argv):
    try:
        os.mkdir("specbin/")
    except OSError:
        pass

    failures = 0
    for spec in glob_for('spec/', 'ulg'):
        try:
            run_spec(spec)
            sys.stdout.write('.')
            sys.stdout.flush()
        except Error as e:
            err = '\n{}: {}: {}'.format(spec, type(e).__name__, e.error)
            print(err, file=sys.stderr)
            failures += 1

    # newline follwing all those .s
    print('\n')
    print('-' * 40)
    if failures:
        print('{} tests failed'.format(failures))
    else:
        print('All tests passed')
    return failures

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
