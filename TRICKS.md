On Android to make visible mounts use:
```bash
su -mm -c 'mount -t ext4 -o rw /dev/block/mmcblk1p2 /config/p2'
```

Problem on Lenovo K80M
```
root@localhost:~# ghc-8.0.1-bin/bin/ghc -V
ghc: failed to create OS thread: Cannot allocate memory
root@localhost:~# ulimit -a
core file size          (blocks, -c) 0
data seg size           (kbytes, -d) unlimited
scheduling priority             (-e) 40
file size               (blocks, -f) unlimited
pending signals                 (-i) 31107
max locked memory       (kbytes, -l) 64
max memory size         (kbytes, -m) unlimited
open files                      (-n) 1024
pipe size            (512 bytes, -p) 8
POSIX message queues     (bytes, -q) 819200
real-time priority              (-r) 0
stack size              (kbytes, -s) 2359296
cpu time               (seconds, -t) unlimited
max user processes              (-u) 31107
virtual memory          (kbytes, -v) unlimited
file locks                      (-x) unlimited
root@localhost:~# ulimit -s 1000000
root@localhost:~# ghc-8.0.1-bin/bin/ghc -V
ghc: failed to create OS thread: Cannot allocate memory
```

The solution:
```
root@localhost:~# ulimit -s 899999
root@localhost:~# ghc-8.0.1-bin/bin/ghc -V
The Glorious Glasgow Haskell Compilation System, version 8.0.1
```
