#! /bin/bash

cat -n /sys/class/power_supply/BATA/uevent | sed -n 's/=/ /;2p;8p;9p;10p;11p;13p' \
  | while read i; do printf '%2s  %-31s  %15s\n' $i; done
