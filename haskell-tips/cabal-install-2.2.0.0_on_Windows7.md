Do you have an Windows 7 machine and an user name with some no ASCII symbols? Then you'll have a problem
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

Now we'll do the steps from the [Windows preparation](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows#II.UpgradingpackagesinMSYS2).

Run and complete, following its advices
```
pacman -Syuu
```
Install dependencies:
```
pacman -S --needed git tar bsdtar binutils autoconf make xz \
    curl libtool automake python python2 p7zip patch ca-certificates \
    mingw-w64-$(uname -m)-gcc mingw-w64-$(uname -m)-python3-sphinx \
    mingw-w64-$(uname -m)-tools-git
```
 Download binary windows [release](https://www.haskell.org/ghc/) of GHC, unpack it to `$HOME` and add its `bin` and `lib/bin` dirs to your `$PATH`.
```
export PATH=$HOME/ghc-8.4.3/bin:$HOME/ghc-8.4.3/lib/bin:$PATH
```
Now we can install `cabal-install` via modified script `bootstrap.sh` which downloads and compiles dependencies.
```bash
wget https://hackage.haskell.org/package/cabal-install-2.2.0.0/cabal-install-2.2.0.0.tar.gz
wget https://hackage.haskell.org/package/cabal-install-2.2.0.0/cabal-install.cabal
tar xvf cabal-install-2.2.0.0.tar.gz
cp cabal-install.cabal cabal-install-2.2.0.0/
cd cabal-install-2.2.0.0/
mv bootstrap.sh bootstrap-old.sh
wget https://raw.githubusercontent.com/alogic0/misc/master/haskell-tips/bootstrap.sh
PREFIX=$HOME/cabal  ./bootstrap.sh --user --no-doc
export PATH=$HOME/cabal/bin:$PATH
cabal update
```
If it successfully finished don't forget to add string with  `export` to your `.profile` file.

And final step, put `config` file into `/c/Users/<yourname>/AppData/Roaming/cabal/`, because `cabal` on Windows looks for its config in that directory. Change all path-records there to your analog of my `d:\mstmp\msys64\home\oleg\cabal`. See, for example, [my version](./config-windows).
