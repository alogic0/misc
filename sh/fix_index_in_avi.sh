## This command fixes broken index in .avi files. VLC will not complain on them
## and try to restore index before playing.
## First, create fixed copy.

for i in [012]*.avi; do ffmpeg -i "$i" -vcodec copy -acodec copy copy_"$i"; done

## Second, replace broken original with fixed copy. Also remove empty space in
## the end of file names.

shopt -s extglob
for i in [012]*.avi; do rm "$i";  mv -v copy_"$i" "${i/%*([ ]).avi/.avi}"; done
