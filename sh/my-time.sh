#!/bin/bash

echo "Computer time: "
 date -u
echo "Time from time.nist.gov: " 
 cat </dev/tcp/time.nist.gov/13
 
# nc time.nist.gov 13
