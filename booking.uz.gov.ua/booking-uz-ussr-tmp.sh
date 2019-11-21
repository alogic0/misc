#!/bin/bash

## Usage: $0 Одесса Ковель 2019-06-28
##    or: $0 барановичи минск 08-19

## requires parsing JSON utility 'jl'
## https://github.com/chrisdone/jl

YEAR=$(date +'%Y')
DT=${3:-$(date +'%m-%d')}

TRAIN_SEARCH=https://booking.uz.gov.ua/ru/train_search/;
TRAIN_SEARCH_MOBILE=https://booking.uz.gov.ua/ru/mobile/train_search/;

read STATION1 FROM <<< $(grep -Pi "^$1\s+\d+$" express-asu.txt);
read STATION2 TO <<< $(grep -Pi "^$2\s+\d+$" express-asu.txt);

if [[ $(echo $DT | sed 's/\-/ /g' | wc -w) -eq 3 ]]
  then
    DATE=$DT
  else
    DATE=$YEAR-${DT}
fi

ROUTE="${1}-${2}"
# PZD='from='$FROM'&to='$TO'&time=00%3A00&train=084%D0%A8&get_tpl=1'
PZQ="from=${FROM}&to=$TO"

# echo "PZQ: $PZQ, ROUTE: $ROUTE"; exit

if [[ (-n $FROM) && (-n $TO) ]]
  then

    TMP=$(mktemp /tmp/$(basename $0)-$(date +%Y%m%d%H%M%S)-XXX)
    TMP2=$(mktemp /tmp/$(basename $0)-$(date +%Y%m%d%H%M%S)-2-XXX)
## DEBUG
     echo wget --post-data="${PZQ}&date=${DATE}&time=00%3A00&get_tpl=1" $TRAIN_SEARCH;
     #wget -q -O - --post-data="${PZQ}&date=${DATE}" https://booking.uz.gov.ua/ru/train_search/ > $TMP
     wget -q -O $TMP \
      --post-data="${PZQ}&date=${DATE}&time=00%3A00&get_tpl=1" $TRAIN_SEARCH;
     cat $TMP
     exit
## DEBUG END
    wget -q -O $TMP \
      --post-data="${PZQ}&date=${DATE}&time=00%3A00&get_tpl=1" $TRAIN_SEARCH;
    MSG=$(jl '\o -> if ( elem "warning" $ keys o.data) then o.data.warning else (map (\l -> [l.num, l.from.stationTrain, l.to.stationTrain]) o.data.list)' $TMP)
    [[ -n $MSG ]] && echo $MSG | jl id --lines | while read TR; do
      if [[ "$TR" =~ ^\[\" ]];
        then read PZNAME FROMST TOST <<< $(cut -d '"' -f2,4,6 --output-delimiter=' ' <<< "$TR");
             read STATION1 FROM <<< $(grep -iP "^${FROMST}\s+\d+$" express-asu.txt);
             read STATION2 TO <<< $(grep -iP "^${TOST}\s+\d+$" express-asu.txt);
             PZQ="from=${FROM}&to=$TO"
## DEBUG
             # echo $PZQ
             # echo wget -q -O $TMP2 --post-data=\"${PZQ}\&date=${DATE}\&train=${PZNAME}\" https://booking.uz.gov.ua/ru/mobile/train_wagons/;
             wget -q -O $TMP2 --post-data="${PZQ}&date=${DATE}&train=${PZNAME}" \
                  https://booking.uz.gov.ua/ru/mobile/train_wagons/;
  #           MSG2=" Cost: $(jl '_.data.wagonTypes | map (\x -> [x.letter, x.cost / 100.0])' $TMP2)"
             MSG2="$(jl '\o -> if ( elem "error" $ keys o)
                                      then "error"
                                      else  map (\x -> [x.title, x.free, x.cost / 100.0]) o.data.wagonTypes' $TMP2)"

        else MSG2=''
      fi
      echo "$DATE ${TR} ${MSG2}"
    # break  
    done
## DEBUG
    # echo $TMP; cat $TMP; echo; echo $TMP2; cat $TMP2; echo
    rm $TMP $TMP2
fi
