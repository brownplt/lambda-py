#!/bin/sh

dir="$(dirname "$0")"

racket "$dir/python-main.rkt" --python-path "$(which python3.2)" --get-lexical-syntax | sed s/^' '*$//g | uniq | sed '/^$/d'
