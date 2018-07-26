#!/bin/bash

export PATH=$PATH:$HOME/bin

YR=2018
DT=08-22

PNM='OD - KOV'
PZD='to=2218020&from=2208001&time=00%3A00&train=084%D0%A8&get_tpl=1'

#PNM='KOV - OD'
#PZD='from=2218020&to=2208001&time=00%3A00&train=084%D0%9B&get_tpl=1'

[ -d $HOME/tmp/kov-od ] || mkdir -p $HOME/tmp/kov-od 2>/dev/null

LOG=$HOME/tmp/kov-od/${DT}.txt
LOGNZ=$HOME/tmp/kov-od/${DT}-nozero.txt

[ -f $LOG ] || touch $LOG

P_PRE=$(tac $LOG | egrep -m 1 'Плацкарт' | egrep -o '[[:digit:]]+')
P_PRE=${P_PRE:-0}
K_PRE=$(tac $LOG | egrep -m 1 'Купе' | egrep -o '[[:digit:]]+')
K_PRE=${K_PRE:-0}

function show-last () {
    echo "Купе: $K_PRE";
    echo "Плацкарт: $P_PRE";
    echo;
}

date
show-last;
TMP=$(mktemp /tmp/$(basename $0)-$(date +%Y%m%d%H%M%S)-XXX)
TMP2=$(mktemp /tmp/$(basename $0)-$(date +%Y%m%d%H%M%S)-2-XXX)
wget -q -O - --post-data=${PZD}'&date='${YR}-${DT} https://booking.uz.gov.ua/ru/mobile/train_wagons/ > $TMP
cat $TMP | grep -Eo '"title":"[[:alpha:]]+","letter":".","free":[[:digit:]]+,"cost":[[:digit:]]+' \
          | cut -d ',' -f 1,3,4 | grep -v 'Люкс' | tee $TMP2
P_NOW=$(cat $TMP2 | grep 'Плацкарт' | egrep -o '"free":[[:digit:]]+' | egrep -o '[[:digit:]]+')
P_NOW=${P_NOW:-0}
K_NOW=$(cat $TMP2 | grep 'Купе' | egrep -o '"free":[[:digit:]]+' | egrep -o '[[:digit:]]+')
K_NOW=${K_NOW:-0}
if [[ $K_NOW -gt $K_PRE ]]; then MSG="$MSG""Купе: $K_NOW +$(($K_NOW - $K_PRE)) "; fi
if [[ $P_NOW -gt $P_PRE ]]; then MSG="$MSG""Плацкарт: $P_NOW +$(($P_NOW - $P_PRE)) "; fi

if [[ "$MSG" != "" ]]; then snd-sms.sh "$DT $PNM $MSG"; fi
#if [[ "$MSG" != "" ]]; then echo "$DT $PNM $MSG"; fi

K_PRE=$K_NOW;
P_PRE=$P_NOW;
{ date;
  show-last;
} >> $LOG

rm $TMP $TMP2

dt1=$(date +'%Y-%m-%d %a %H:%M:%S %Z')
if [ $K_NOW -gt 0 -o $P_NOW -gt 0 ];
  then
    echo -e "${dt1}\tKupe: ${K_NOW}\tPlackart: ${P_NOW}" | tee -a $LOGNZ
fi
echo
