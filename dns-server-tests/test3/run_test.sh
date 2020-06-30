#! /bin/bash

set -eo pipefail

if [ "$1" == "" ] || [ "$2" == "" ]; then
    echo "Usage: run_test.sh <hostname> <port>"
    exit 1
fi

nc -u $1 $2 < packet > response
