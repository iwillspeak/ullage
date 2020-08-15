#! /usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function check_for()
{
	command -v $1 >/dev/null 2>&1 || { echo >&2 "$1 is required but it's not installed.  Aborting."; exit 1; }
}

check_for python3

echo "Clang version:"
clang --version

echo "Rust version"
rustc --version

just "$@"
