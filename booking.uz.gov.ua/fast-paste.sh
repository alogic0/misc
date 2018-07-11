#!/bin/bash

# Usage: $0 name.txt
## Paste by middle mouse lines from name.txt, press Enter for the next line

#for i in $(cat $1); do xsel -d; echo $i | xsel -i -b -s -p; echo $i; read a; done

NL=$(wc -l < "$1")
for i in $(seq 1 $NL); do
  l=$(sed -n ${i}p "$1");
  xsel -d
  echo $l | cut -d ' ' -f 1 | xsel -i -b -s -p
  echo $l
  read -s a
done
