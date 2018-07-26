#!/bin/bash

for i in ~/tmp/kov-od/*08-22*; do
  kov-od-rotate.sh $((4 * 30)) "$i"
done
