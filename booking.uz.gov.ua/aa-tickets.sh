#!/bin/bash

TMP=$(mktemp /tmp/$(basename $0)-$(date +%Y-%m-%d-%H%M%S)-XXX)
while :; do
  for i in 28.07 29.07; do
    booking-uz.sh KOV OD ${i} | tee $TMP
    if grep -q 'places' $TMP
      then 
        lnk=$(grep 'http:' $TMP)
        xdg-open "$lnk"
        alert-tickets.sh
        exit
      fi
    rm $TMP
    sleep 5
  done
sleep 15m
done
