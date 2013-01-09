#!/bin/sh

py=`which python3`
racket python-main.rkt --python-path $py --get-core-syntax
