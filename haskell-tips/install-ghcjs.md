```bash
mkdir src; cd src
sudo apt-get install nodejs npm
sudo apt-get install ghc-8.4.4
## link manually `ghc` to ghc-8.4.4
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
Now the best way to have

 * `ghcjs-base`
 * `reflex`
 * `reflex-dom` and `reflex-dom-core`.


is to add to your `~/.cabal/config`:

```
repository ghcjs-overlay
  url: http://hackage-ghcjs-overlay.nomeata.de/
  secure: True
  root-keys:
  key-threshold: 0
```

If you use new-style cabal commands, you can also add it to your
`cabal.project`, but you will have to use `cabal new-update` instead of `cabal
update`.

See about this and about automatic deployment your build to `gh-pages` [here](https://github.com/nomeata/hackage-ghcjs-overlay/blob/master/README.md).

Obsolete instruction
--------------------

The way to install `ghcjs-dom`:
```bash
wget -O ghcjs-base-master.zip https://codeload.github.com/ghcjs/ghcjs-base/zip/master
unzip ghcjs-base-master.zip
cd ghcjs-base-master
cabal install  --ghcjs
cabal install  --ghcjs ghcjs-dom
```
