#!/bin/bash

## Usage: launch ./path/to/shortcut.desktop
## https://stackoverflow.com/questions/6020106/hashbang-for-gnome-desktop-files
launch(){
    (
    # where you want to install the launcher to
    appdir=$HOME/.local/share/applications

    # the template used to install the launcher
    template=launcher-XXXXXX.desktop

    # ensure $1 has a .desktop extension, exists, is a normal file, is readable, has nonzero size
    # optionally use desktop-file-validate for stricter checking
    # if ! desktop-file-validate "$1" 2>/dev/null; then
    if [[ ! ( $1 = *.desktop && -f $1 && -r $1 && -s $1 ) ]]; then
        echo "ERROR: you have not supplied valid .desktop file" >&2
        exit 1
    fi

    # ensure the temporary launcher is deleted upon exit
    trap 'rm "$launcherfile" 2>/dev/null' EXIT

    launcherfile=$(mktemp -p "$appdir" "$template")
    launchername=${launcherfile##*/}

    if cp "$1" "$launcherfile" 2>/dev/null; then
        gtk-launch "$launchername" "${@:2}"
    else
        echo "ERROR: failed to copy launcher to applications directory" >&2
        exit 1
    fi

    exit 0
    )
}

launch "$1"
