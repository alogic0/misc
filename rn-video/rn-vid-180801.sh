#!/bin/bash

find . -name "*.avi" | while read i; do mv "$i" $(rn-vid "$(basename "$i")"); done
