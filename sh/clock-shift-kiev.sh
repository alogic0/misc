#! /bin/bash

# Use: $0 [/usr/share/zoneinfo/Europe/Kiev] [2018]

ZONE_FILE=${1:-/usr/share/zoneinfo/Europe/Kiev}
Y=${2:-$(date +"%Y")}

echo "Clock shift in $Y"
zdump -v ${ZONE_FILE} | grep "$Y"
