#! /bin/bash

# Usage: $0 03-23

DT=2018-${1:-03-23}

echo "Date: ${DT}"
echo
wget -q -O - --post-data='from=2218020&to=2208001&date='${DT}'&time=00%3A00&train=084%D0%9B&get_tpl=1' https://booking.uz.gov.ua/ru/mobile/train_wagons/ | grep -Eo '"title":"[[:alpha:]]+","letter":".","free":[[:digit:]]+,"cost":[[:digit:]]+' | cut -d ',' -f 1,3,4 | grep -v 'Люкс'
echo
