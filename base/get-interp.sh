#!/bin/sh

PY=`which python3`
racket python-main.rkt --python-path $PY --interp
