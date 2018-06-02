for i in $(cat names.txt); do echo $i | xsel -i; echo $i; read a; done
