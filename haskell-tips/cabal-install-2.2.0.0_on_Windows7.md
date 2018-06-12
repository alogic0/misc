Do you have an Windows 7 machine and a user name with some no ASCII symbols? Then you'll have a problem
with building the `cabal-install`. The solution is to install GHC and all packages
into a directory with a simple path. For example `d:\mstmp\`. We'll do the following steps:

1.  Install [MSYS2](http://repo.msys2.org/distrib/)  
I download a latest `tar.gz` file from there and unpack it to my, let's say, `d:\mstmp` directory.  
You can choose any path, but avoid spaces and non ASCII symbols in it.
Run, after unpacking, `mingw64.exe`. It will make the initial configuration of MSYS2. 
It's useful also to go into the 'Options' menu it the opened terminal window and change for better looking font 
and size of the window if you will.
2.  Create a directory inside `/home` with ASCII user name. Now we'll make it a new home.  
I create a directory named `oleg` and copy `.bashrc` and `.profile` there from the current home.
Then old `.bashrc` and `.profile` are replaced with the following content:
     ```bash
     export HOME=/home/oleg
     cd $HOME
     . .profile
     . .bashrc
     ```
3.  After re-running `mingw64` you'll be in the new home. The full Windows path in my case is `D:\mstmp\msys64\home\oleg`  

Now we'll do the steps taken from the [Windows preparation](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows#II.UpgradingpackagesinMSYS2) instruction.

Run and wait for complete the next command, follow its screen advices
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
 Download the binary [release](https://www.haskell.org/ghc/) of GHC for Windows, unpack it to `$HOME` and add its `bin` and `lib/bin` dirs to your `$PATH`.
```
export PATH=$HOME/ghc-8.4.3/bin:$HOME/ghc-8.4.3/lib/bin:$PATH
```
Now we can install `cabal-install` via modified script `bootstrap.sh` which downloads and compiles all needed dependencies.
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
If it successfully finished don't forget to add the string from, starting with `export` to your `.profile` file.

And final step, put `config` file into the hidden from Windows users directory `/c/Users/<yourname>/AppData/Roaming/cabal/`, because `cabal` on Windows looks for its config in there. The `<yourname>` here is your user name in Windows. Change all the path-records there to your version of my `d:\mstmp\msys64\home\oleg\cabal`. See, for example, [my variant](./config-windows).
