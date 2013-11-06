#!/bin/sh

dir="$(dirname "$0")"
cd $dir
racket "./python-main.rkt" --python-path "$(which python3.2)" --get-lexical-syntax | sed s/^' '*$//g | uniq | sed '/^$/d'
