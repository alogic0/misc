Do you have an Windows 7 machine and no ASCII user name there? Then you'll have a problem
with building the `cabal-install`. The solution is to install GHC and all packages
into a directory with a simple path. For example `d:\mstmp\`. We'll do the following steps:

1. Install [MSYS2](http://repo.msys2.org/distrib/)  
I use `tar.gz` file and just unpack it to my `d:\mstmp` directory.  
Run after unpacking `mingw64.exe` file so it will make the initial configuration of the MSYS2. 
It's optional also to change 'Options' of the window to have better font and window size if you will.
2. Create inside `/home` a directory with ASCII user name and make it a new home.  
I've created a directory named `oleg` and copied there `.bashrc` and `.profile` from the current home.
Then old `.bashrc` and `.profile` were replaced with the following content:
```bash
export HOME=/home/oleg
cd $HOME
. .profile
. .bashrc
```
After re-running `mingw64` you'll be in the new home. The full path in my case is `D:\mstmp\msys64\home\oleg`  
3. Follow the steps from [Windows preparation](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows#II.UpgradingpackagesinMSYS2).


```bash
 PREFIX=$HOME/cabal  ./bootstrap.sh --user --no-doc
 ```
