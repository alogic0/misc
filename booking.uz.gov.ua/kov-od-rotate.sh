#!/bin/bash

## Leave only last $1 lines of file $2

function error { echo "No such file: $1"; exit 1
} 

[ -f "$2" ] || error "$2"
nl=$(($1 + 0))

tmpf1=$(mktemp -q /tmp/tmpf1-XXXX)

tail -n $nl "$2" > $tmpf1
mv $tmpf1 "$2"

