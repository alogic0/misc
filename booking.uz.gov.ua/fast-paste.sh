#!/bin/bash

# Usage: $0 name.txt
## Paste by middle mouse lines from name.txt, press Enter for the next line

for i in $(cat $1); do echo $i | xsel -i; echo $i; read a; done
