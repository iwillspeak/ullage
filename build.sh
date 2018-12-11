#! /usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ ! -d ${DIR}/venv ]
then
	virtualenv venv
	pip install -r ${DIR}/requirements.txt
fi

source ${DIR}/venv/bin/activate

invoke "$@"
