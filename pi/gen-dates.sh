for i in $(seq 0 $((365*2018))); do date -d "1-1-1 +$i day" +"%Y%m%d%n%d%m%Y%n%m%d%Y"; done >dates-ac.txt
# grep -bo -m 1 -E "16041960" pi-billion.txt