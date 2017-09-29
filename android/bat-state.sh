FN=/sys/class/power_supply/bq27441_battery/uevent
echo $FN
cat -n $FN |\
  sed -n 's/=/ /;1,2p;10,11p;14,15p;16p' \
  | while read i; do printf '%2s  %-31s  %15s\n' $i; done
