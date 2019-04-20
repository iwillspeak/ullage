#! /usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ ! -d ${DIR}/venv ]
then
	virtualenv venv
fi

source ${DIR}/venv/bin/activate
pip3 install -r ${DIR}/requirements.txt

invoke "$@"
