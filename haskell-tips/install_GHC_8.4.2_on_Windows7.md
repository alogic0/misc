Do you have an Windows 7 machine with no ASCII user name? Then you'll have a problem
with building `cabal-install` there. The solution is to install GHC and all packages
to a simple-path directory. For example into `d:\oleg\`.

```bash
 PREFIX=$HOME/cabal ./bootstrap.sh --user --no-doc
 ```