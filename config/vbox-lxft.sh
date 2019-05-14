## Creating a swapfile
## under root

dd if=/dev/zero of=swapfile bs=1M count=4K
chmod 0600 swapfile
mkswap swapfile
echo '/swapfile    none   swap   defaults   0    0' >> /etc/fstab
swapon -a
