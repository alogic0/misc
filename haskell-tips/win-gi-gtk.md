https://github.com/haskell-gi/haskell-gi

Read this

https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows

Put these files together

https://github.com/haskell-gi/haskell-gi/blob/master/examples/advanced/Cairo.hs

###  haskell-gi-examples.cabal
```
name:                haskell-gi-examples
version:             0.1
synopsis:            Some examples of the use of haskell-gi generated libraries
description:         Some examples of the use of haskell-gi generated libraries
homepage:            https://github.com/haskell-gi/haskell-gi
license:             LGPL-3
author:              Iñaki García Etxebarria
maintainer:          Iñaki García Etxebarria (garetxe@gmail.com)
stability:           Experimental
category:            Development
build-type:          Simple
cabal-version:       >=1.10

source-repository head
  type: git
  location: git://github.com/haskell-gi/haskell-gi.git


executable Cairo
  main-is:              Cairo.hs
  build-depends:        base >= 4.7 && < 5,
                        haskell-gi-base == 0.21.*,
                        gi-glib == 2.0.*,
                        gi-cairo == 1.0.*,
                        gi-gdk == 3.0.*,
                        gi-gdkpixbuf == 2.0.*,
                        gi-gtk == 3.0.*,
                        cairo == 0.13.*,
                        old-time == 1.1.*,
                        transformers == 0.5.*
  default-language:     Haskell2010
  other-extensions:     OverloadedStrings, OverloadedLabels
  ghc-options:         -Wall -fwarn-incomplete-patterns -fno-warn-unused-do-bind -fno-warn-name-shadowing -fno-warn-missing-signatures -threaded -with-rtsopts=-N
```

### build-examples.sh

```bash
#!/bin/bash

set -e
set -x

chcp.com 65001
export HASKELL_GI_GIR_SEARCH_PATH=/mingw64/share/gir-1.0

cabal install --only-dep
cabal clean
haskell-gi -l Gtk WebKit Gst GstBase GstAudio GstVideo xlib
haskell-gi -c Gtk WebKit Gst GstBase GstAudio GstVideo xlib
cabal configure
cabal build

```

Run
```bash
./build-examples.sh
./dist/build/Cairo/Cairo.exe
```
