```bash
pacman -Sy mingw-w64-x86_64-sqlite3
cp /mingw64/include/sqlite3.h /mingw64/lib/include/
cabal install ideas
```
or
```bash
cabal install ideas -f -logging
```
and you won't have to install `sqlite3`  
see [Ideas tutorial](http://ideas.cs.uu.nl/tutorial/)
