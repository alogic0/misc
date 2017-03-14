#!/bin/bash

## Use: $0 16041960
# FL=$(date +"%s")
FL="$1"
# wget -O ${FL}.html --post-data="s=$1&o=searchdigits&n=2&c=pi" http://www.subidiom.com/pi/pi.asp
wget -O ${FL}.html --post-data="searchstring=$1&searchoption=search&numdigits=1&searchconstant=pi&search=submit" http://www.subidiom.com/pi/piday.asp
echo See file ${FL}.html

# grep -o -E "string <b>[0-9]*</b> appears at the [0-9,]*"