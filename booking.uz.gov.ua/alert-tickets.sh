#!/bin/bash

TMP=$(mktemp /tmp/xmXXXXX)
xmessage -print -center "Ticket!" > $TMP &
##/usr/share/sounds/ubuntu/stereo/
while sleep 1; do 
  if grep -q okay $TMP
  then 
    rm $TMP
    exit
  else canberra-gtk-play -i phone-incoming-call
  fi
done
