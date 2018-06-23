I've been installing on Windows 7 from the `mingw64` shell and this worked for me.
```bash
pacman -Sy mingw-w64-x86_64-openssl
cabal get HsOpenSSL-0.11.4.14
cd HsOpenSSL-0.11.4.14
cabal install --user --extra-include-dirs=/mingw64/include --extra-lib-dirs=/mingw64/lib --extra-lib-dirs=/mingw64/bin
```
