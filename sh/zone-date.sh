#!/bin/bash

## Usage: $0 /usr/share/zoneinfo/America/Caracas

if [[ ( -e $1 ) && ( $1 =~ /usr/share/zoneinfo ) ]]
  then
    TZ=${1#/usr/share/zoneinfo/}
    export TZ
    shift
fi
echo TZ=$TZ
if [[ ! -z $1 ]]
  then
    date $*
  else
    date -R
fi
