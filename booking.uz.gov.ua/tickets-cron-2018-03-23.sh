#!/bin/bash

export PATH=$PATH:$HOME/bin

LOG=$HOME/tmp/kov-od/03-23.txt
P_PRE=$(tac $LOG | egrep -m 1 'Плацкарт' | egrep -o '[[:digit:]]+')
K_PRE=$(tac $LOG | egrep -m 1 'Купе' | egrep -o '[[:digit:]]+')

function show-last () {
    echo "Купе: $K_PRE";
    echo "Плацкарт: $P_PRE";
    echo;
}

show-last;
date
TMP=$(mktemp $HOME/tmp/kov-od/$(basename $0)-$(date +%Y%m%d%H%M%S)-XXX)
TMP2=$(mktemp $HOME/tmp/kov-od/$(basename $0)-$(date +%Y%m%d%H%M%S)-2-XXX)
wget -q -O - --post-data='from=2218020&to=2208001&date=2018-03-23&time=00%3A00&train=084%D0%9B&get_tpl=1' https://booking.uz.gov.ua/ru/mobile/train_wagons/ > $TMP
echo
cat $TMP | grep -Eo '"title":"[[:alpha:]]+","letter":".","free":[[:digit:]]+,"cost":[[:digit:]]+' \
          | cut -d ',' -f 1,3,4 | grep -v 'Люкс' | tee $TMP2
P_NOW=$(cat $TMP2 | grep 'Плацкарт' | egrep -o '"free":[[:digit:]]+' | egrep -o '[[:digit:]]+')
P_NOW=${P_NOW:-0}
K_NOW=$(cat $TMP2 | grep 'Купе' | egrep -o '"free":[[:digit:]]+' | egrep -o '[[:digit:]]+')
K_NOW=${K_NOW:-0}
if [[ $K_NOW -gt $K_PRE ]]; then MSG="$MSG""Купе: $K_NOW +$(($K_NOW - $K_PRE)) "; fi
if [[ $P_NOW -gt $P_PRE ]]; then MSG="$MSG""Плацкарт: $P_NOW +$(($P_NOW - $P_PRE)) "; fi
if [[ "$MSG" != "" ]]; then snd-sms.sh "03-23 KOV OD ""$MSG"; fi
K_PRE=$K_NOW;
P_PRE=$P_NOW;
{ date;
  show-last;
} >> $LOG

rm $TMP $TMP2
echo
