FN=/sys/class/power_supply/bq27441_battery/uevent
echo $FN
cat $FN |\
  sed -ne '1,2p;10,11p;14,15p;16p'
