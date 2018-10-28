#!/bin/bash

hlink1=https://downloads.haskell.org/~ghc/8.4.2/ghc-8.4.2-x86_64-deb8-linux.tar.xz
hlink2=https://hackage.haskell.org/package/cabal-install-2.2.0.0/cabal-install-2.2.0.0.tar.gz

sudo apt-get install git autoconf automake libtool make gcc g++ \
   libgmp-dev ncurses-dev libtinfo-dev python3 xz-utils zlib1g-dev

##tmpd=$(mktemp -d -p $HOME src-haskell-XXX)
tmpd=$HOME/src-haskell
mkdir $tmpd

cd $tmpd
wget -c $hlink1
ghc_file=$(basename $hlink1)
tar -xvf ${ghc_file}
ghc_src=$(tar -tf ${ghc_file} | head -n 1 | cut -d '/' -f 1)
cd ${ghc_src}
./configure --prefix=$HOME/${ghc_src}
make install
export PATH=$HOME/${ghc_src}/bin:$PATH

echo ghc_src: ${ghc_src}

cd $tmpd
wget -c $hlink2 
cabal_file=$(basename $hlink2)
tar -xvf ${cabal_file}
cabal_src=$(tar -tf ${cabal_file} | head -n 1 | cut -d '/' -f 1)
cd ${cabal_src}
./bootstrap.sh --user --no-doc
export PATH=$HOME/.cabal/bin:$PATH

echo ghc_src: ${ghc_src}
echo cabal_src: ${cabal_src}

cd
echo "You can now delete $tmpd"
