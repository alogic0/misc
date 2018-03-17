#! /bin/bash

# Use: $0 [2018]

Y=${1:-$(date +"%Y")}
echo "Clock shift in $Y"
zdump -v /usr/share/zoneinfo/Europe/Kiev | grep "$Y"
