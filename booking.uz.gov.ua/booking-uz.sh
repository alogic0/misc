#!/bin/bash

## Usage: $0 OD KOV 08-19
##    or: $0 OD KOV 2018-08-19
##    or: $0 OD KOV

YEAR=$(date +'%Y')
DT=${3:-$(date +'%m-%d')}

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

if [[ $(echo $DT | sed 's/\-/ /g' | wc -w) -eq 3 ]]
  then
    DATE=$DT
  else
    DATE=$YEAR-${DT}
fi

PNM="${1}-${2}"
PZD='from='$FROM'&to='$TO'&time=00%3A00&train=084%D0%A8&get_tpl=1'

if [[ (-n $FROM) && (-n $TO) ]]
  then

    TMP=$(mktemp /tmp/$(basename $0)-$(date +%Y%m%d%H%M%S)-XXX)
    TMP2=$(mktemp /tmp/$(basename $0)-$(date +%Y%m%d%H%M%S)-2-XXX)
    wget -q -O - --post-data=${PZD}'&date='$DATE https://booking.uz.gov.ua/ru/mobile/train_wagons/ > $TMP
    cat $TMP | grep -Eo '"title":"[[:alpha:]]+","letter":".","free":[[:digit:]]+,"cost":[[:digit:]]+' \
              | cut -d ',' -f 1,3,4 | grep -v 'Люкс' | tee $TMP2
    P_NOW=$(cat $TMP2 | grep 'Плацкарт' | egrep -o '"free":[[:digit:]]+' | egrep -o '[[:digit:]]+')
    P_NOW=${P_NOW:-0}
    K_NOW=$(cat $TMP2 | grep 'Купе' | egrep -o '"free":[[:digit:]]+' | egrep -o '[[:digit:]]+')
    K_NOW=${K_NOW:-0}
    MSG="$MSG"" Купе: $K_NOW"
    MSG="$MSG"" Плацкарт: $P_NOW"
    echo "$DT $PNM $MSG"
    rm $TMP $TMP2
fi

## wget -O - --post-data='from='$FROM'&to='$TO'&date='$DATE'&time=00%3A00&get_tpl=1' https://booking.uz.gov.ua/ru/train_search/

##  wget -O - --post-data='from=2208001&to=2200001&date=2018-08-22&train=106%D0%A8&wagon_type_id=%D0%9A&get_tpl=1' https://booking.uz.gov.ua/ru/train_wagon/

## wget -O - --post-data='from=2208001&to=2200001&train=106%D0%A8&date=2018-08-22&wagon_num=1&wagon_type=%D0%9A&wagon_class=%D0%91' https://booking.uz.gov.ua/ru/train_wagons/
