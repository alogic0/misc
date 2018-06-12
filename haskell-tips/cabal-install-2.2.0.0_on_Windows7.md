Do you have an Windows 7 machine and a user name containing some no ASCII symbols? If 'No' then skip steps 2-4, if 'Yes', then you'll have a problem with building the `cabal-install`. The solution is to install GHC and all its packages into a directory with 
a simple path name. For example, into `d:\shonfinkel\` or `d:\haskell\`. We'll do the following steps for this:

1.  Install [MSYS2](http://repo.msys2.org/distrib/)  
Download a latest `tar.gz` file from there and unpack it to `d:\shonfinkel` directory. 
You can choose any path, but avoid spaces and non ASCII symbols in it. Now we have a directory `d:\shonfinkel\msys64`.
Run `mingw64.exe` from there it will make the initial configuration of MSYS2. 
It's useful also to go into the 'Options' menu in the opened window and change font 
and window sizes there for better looking, if you will. Now, run `msys64` terminal again and you are in the Linux environment.
2.  Our current home directory is named as `<your Windows name>`. We won't change it, but we'll change the `$HOME` variable and will work inside a new directory with the only ASCII symbols in its name. This is for saving us from the problems during a compilation.  
3.  Create a directory inside `/home` with ASCII user name.  
I've created a directory named `oleg` and copied `.bashrc` and `.profile` there from the current `$HOME`.
The old `.bashrc` and `.profile` were replaced with the following content:
     ```bash
     export HOME=/home/oleg
     cd $HOME
     . .profile
     . .bashrc
     ```
4.  After re-running `mingw64` you'll be in your new home. The path inside MSYS2 is `/home/oleg` and the full Windows path is `D:\shonfinkel\msys64\home\oleg`  

Now we'll do the steps taken from the [Windows preparation](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows#II.UpgradingpackagesinMSYS2) instruction.

Run and wait for complete the next command, follow its screen advices
```
pacman -Syuu
```
Run it again.

Install dependencies:
```
pacman -S --needed git tar bsdtar binutils autoconf make xz \
    curl libtool automake python python2 p7zip patch ca-certificates \
    mingw-w64-$(uname -m)-gcc mingw-w64-$(uname -m)-tools-git \
    mingw-w64-$(uname -m)-python3-sphinx
```
 Download the binary [release](https://www.haskell.org/ghc/) of GHC for Windows, unpack it to `$HOME` and add its `bin` and `lib/bin` dirs to your `$PATH`. Now it's 8.4.3, so the numbers here.
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
```
If it successfully finished don't forget to add the string from, starting with `export` to your `.profile` file.

And the final step, the `config` file for `cabal-install` is in the hidden for Windows users directory `/c/Users/<yourname>/AppData/Roaming/cabal/`. Change all the path-records there to your version of `d:\shonfinkel\msys64\home\oleg\cabal`. See, for example, [my variant](./config-windows).

Now, you can run
```
cabal update
```
and install anything from the [Hackage](https://hackage.haskell.org/).
