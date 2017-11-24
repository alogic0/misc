#!/bin/bash

## Usage: $0 OD KOV 19.08
##    or: $0 OD KOV 19.08.2018
##    or: $0 OD KOV

YEAR=$(date +'%Y')
DT=${3:-$(date +'%d.%m')}

declare -A names
names=([2218020]=Ковель
       [2208001]=Одесса
       [2100001]=Минск) 

declare -A codes
codes=([KOV]=2218020
       [OD]=2208001
       [MIN]=2100001)

if [[ -n $2 ]]
  then
    FROM=${codes[$1]}
    TO=${codes[$2]}
fi

if [[ $(echo $DT | sed 's/\./ /g' | wc -w) -eq 3 ]]
  then
    DATE=$DT
  else
    DATE=${DT}.$YEAR
fi

if [[ (-n $FROM) && (-n $TO) ]]
  then
    TMP=$(mktemp /tmp/$(basename $0)-$(date +%Y-%m-%d-%H%M%S)-XXX)
    echo "On date $DATE ${names[$FROM]} - ${names[$TO]}"
    wget -q -O - --post-data='station_id_from='$FROM'&station_id_till='$TO'&station_from=&station_till=&date_dep='$DATE'&time_dep=00%3A00&time_dep_till=&another_ec=0&search=' https://booking.uz.gov.ua/ru/purchase/search/ | tee $TMP
    if grep -q 'places' $TMP
      then
          d=$(cut -d '.' -f 1 <<<$DATE)
          m=$(cut -d '.' -f 2 <<<$DATE)
          Y=$(cut -d '.' -f 3 <<<$DATE)
          echo
          echo 'https://booking.uz.gov.ua/ru/?date='${Y}-${m}-${d}'&from='$FROM'&time=00%3A00&to='$TO'&url=train-list'
    fi
    rm $TMP
    echo
fi
