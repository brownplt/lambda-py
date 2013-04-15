#!/bin/sh

# Test (by visual inspection) options of python-main.rkt using both native lexer/parser and python parser

function on_sigint ()
{
    echo "Interrupted."
    exit 0
}
 
trap on_sigint SIGINT

TESTPY="assert True"
TESTDIR=../tests/python-reference/types
PPATH="--python-path `which python3.2`"

function run_options {
PARSEROPT=$1
echo "--interp"
racket python-main.rkt $PARSEROPT --interp <<<$TESTPY
echo "--get-syntax"
racket python-main.rkt $PARSEROPT --get-syntax <<<$TESTPY
# Ignore some of the inner phases...
echo "--get-lex-tokens"
racket python-main.rkt $PARSEROPT --get-lex-tokens <<<$TESTPY
echo "--test"
racket python-main.rkt $PARSEROPT --test $TESTDIR
echo "--progress-report"
racket python-main.rkt $PARSEROPT --progress-report $TESTDIR
}

function run_pyonly_options {
echo "--interp-py"
racket python-main.rkt $PPATH --interp-py <<<$TESTPY
echo "--test-py"
racket python-main.rkt $PPATH --test-py $TESTDIR
}

function run_parser_test_options {
echo "--test-native-parser"
racket python-main.rkt $PPATH --test-native-parser $TESTDIR
}

echo "===== Parser switch options ====="
echo "==== No option - default (native) ===="
run_options ""
echo "==== Python parser ===="
run_options "$PPATH --python-parser"
echo "==== Native parser ===="
run_options "--native-parser"
echo "===== Python only options ====="
run_pyonly_options
echo "===== Two parser options ====="
run_parser_test_options

