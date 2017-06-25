#!/bin/bash

## Usage: $0 OD KOV 19.08

YEAR=2017
declare -A names
names=([2218020]=Ковель
       [2208001]=Одесса
       [2100001]=Минск) 

declare -A codes
codes=([KOV]=2218020
       [OD]=2208001
       [MINSK]=2100001)

if [ ! -z $3 ]
  then
    FROM=${codes[$1]}
    TO=${codes[$2]}
    DATE=${3:-19.08}.$YEAR
fi
if [ x$(basename $0) == x"booking-uz-kov-od.sh" ]
  then FROM=${codes[KOV]}
       TO=${codes[OD]}
       DATE=${1:-19.08}.$YEAR
  else if [ x$(basename $0) == x"booking-uz-od-kov.sh" ]
        then FROM=${codes[OD]}
             TO=${codes[KOV]}
             DATE=${1:-19.08}.$YEAR
       fi
  
fi
if [[ (-n $FROM) && (-n $TO) ]]
  then
    echo "On date $DATE ${names[$FROM]} - ${names[$TO]}"
    wget -q -O - --post-data='station_id_from='$FROM'&station_id_till='$TO'&station_from=&station_till=&date_dep='$DATE'&time_dep=00%3A00&time_dep_till=&another_ec=0&search=' http://booking.uz.gov.ua/ru/purchase/search/
    echo
fi
