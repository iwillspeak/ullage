#! /usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function check_for()
{
	command -v $1 >/dev/null 2>&1 || { echo >&2 "$1 is required but it's not installed.  Aborting."; exit 1; }
}

check_for python3
check_for pip3
check_for virtualenv

if [ ! -d ${DIR}/venv ]
then
	# make sure to use a Python3 interpreter
	virtualenv -p python3 venv
fi

source ${DIR}/venv/bin/activate
pip3 install -r ${DIR}/requirements.txt

invoke "$@"
