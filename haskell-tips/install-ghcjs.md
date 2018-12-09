```bash
mkdir src; cd src
sudo apt-get install nodejs npm
sudo apt-get install ghc-8.4.4
## poing `ghc` to ghc-8.4.4
sudo update-alternatives --config opt-ghc
cabal install alex happy hsc2hs
git clone --branch ghc-8.4 https://github.com/ghcjs/ghcjs.git ghcjs-8.4
cd /ghcjs-8.4/
git submodule update --init --recursive
./utils/makePackages.sh
cabal new-configure
cabal new-build
mkdir .bin; cd .bin
```

`mk-links.sh`:
```bash
ln -s ../utils/dist-newstyle-wrapper.sh ghcjs
ln -s ../utils/dist-newstyle-wrapper.sh ghcjs-pkg
ln -s ../utils/dist-newstyle-wrapper.sh ghcjs-run
ln -s ../utils/dist-newstyle-wrapper.sh ghcjs-dumparchive
ln -s ../utils/dist-newstyle-wrapper.sh haddock-ghcjs
ln -s ../utils/dist-newstyle-wrapper.sh hsc2hs-ghcjs
ln -s ../utils/dist-newstyle-wrapper.sh ghcjs-boot
```
put `mk-links.sh` inside `.bin`, then
```bash
sh mk-links.sh
export PATH="${HOME}/src/ghcjs-8.4/.bin:$PATH"
cd $HOME/src/ghcjs-8.4
## assuming you have 4-core processor
ghcjs-boot -j 4 --no-haddock -s lib/boot/
```
If you want to install `ghcjs-dom`
```bash
wget -O ghcjs-base-master.zip https://codeload.github.com/ghcjs/ghcjs-base/zip/master
unzip ghcjs-base-master.zip
cd ghcjs-base-master
cabal install  --ghcjs
cabal install  --ghcjs ghcjs-dom
```
