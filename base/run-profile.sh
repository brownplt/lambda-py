#!/bin/bash

BASE_COMMAND='racket python-main.rkt '
TEST_DIR='../tests/python-reference'
TEST_FILES=`find $TEST_DIR -name '*.py'`

if [ -z $1 ]
then
    echo "please provide flag"
    exit 1
fi
FLAG=$1

# clean $FLAG.current file
touch $FLAG.current && rm $FLAG.current && touch $FLAG.current

#make && /usr/bin/time -f '%e' racket python-main.rkt --set-flag-false $1 --test ../tests/python-reference > "$1.output" 2>&1

#---------------util function------------
assertNotEmpty ()
{
    [ -z "$1" ] && echo "${FUNCNAME[1]} lacks arguments" && exit 1
}

# args: test file name
getInterpretationTime ()
{
    assertNotEmpty $1
    local startTime=`date +%s`
    local output=`racket python-main.rkt --interp < $1`
    local endTime=`date +%s`
    echo "output:$1:$output"
    echo "time:$1:$((endTime-startTime))"
}

getInterpretationWithFlagTime ()
{
    assertNotEmpty $1

    local startTime=`date +%s`
    local output=`racket python-main.rkt --set-flag-false $FLAG --interp < $1`
    local endTime=`date +%s`
    echo "flagoutput:$1:$output"
    echo "flagtime:$1:$((endTime-startTime))"
}

# args: test file name
getCoreLines ()
{
    assertNotEmpty $1
    local lines=`racket python-main.rkt --get-core-syntax < $1 | wc -l`
    echo "line:$1:$lines"
}

getCoreLinesWithLibs ()
{
    assertNotEmpty $1
    local lines=`racket python-main.rkt --get-core-syntax-with-libs < $1 | wc -l`   # really slow!
    echo "libline:$1:$lines"

}

# args: test file
getCoreLinesWithFlag()
{
    assertNotEmpty $1
    local lines=`racket python-main.rkt --set-flag-false $FLAG --get-core-syntax < $1 | wc -l`
    echo "flagline:$1:$lines"
}

# args: test file
getCoreLinesWithFlagAndLibs()
{
    assertNotEmpty $1
    local lines=`racket python-main.rkt --set-flag-false $FLAG --get-core-syntax-with-libs < $1 | wc -l` # really slow!
    echo "libflagline:$1:$lines"
}

function profileInfo {
s='The profile shows: 

line
  the length of the core language desugared by original desugaring process

lineFlag
  the length of the core language desugared by new desugaring process

Diff
  the shrinked size

time
  the time spent on interpreting the core language desugared by original desugaring process

timeFlag
  the time spent on interpreting the core langauage desugared by new desugaring process

Diff
  the time difference

pass?
  pass the test or not

'

    echo "$s"

}

# args: for loop limits
function getCoreSyntaxLines
{
    local lines
    local output
    local limits
    local x
    if [ -z $1 ]; then limits=-1; else limits=$1; fi

    # flag is true: 
    make
    echo "lib info" >> $FLAG.current
    local randomFile=`echo "$TEST_FILES" | head -n1`
    local coreLineLibs=`getCoreLinesWithLibs $randomFile`
    
    echo "$coreLineLibs"

    x=1
    for fname in $TEST_FILES
    do
        echo $fname >> $FLAG.current

        lines=`getCoreLines $fname`
        output=`getInterpretationTime $fname`
        echo "$lines"
        echo "$output"
        [ $x -eq $limits ] && break
        x=`$(( x + 1 ))`
    done

    # second pass, record the number of lexical-py with flag is false
    make
    echo "lib info" >> $FLAG.current
    local coreLineLibsFlag=`getCoreLinesWithFlagAndLibs $randomFile`
    echo "$coreLineLibsFlag"
    
    x=1
    for fname in $TEST_FILES
    do
        echo $fname >> $FLAG.current

        lines=`getCoreLinesWithFlag $fname`
        output=`getInterpretationWithFlagTime $fname`
        echo "$lines"
        echo "$output"
        [ $x -eq $limits ] && break
        x=`$(( x + 1 ))`
    done
}

# args: contents
makeTable()
{
    # echo profile and libs shrink
    profileInfo

    liblines=`echo "$1" | grep '^libline' | cut -d':' -f3`
    libflaglines=`echo "$1" | grep '^libflagline' | cut -d':' -f3`
    echo "shrink size: $liblines - $libflaglines = $(( liblines - libflaglines ))"
    echo ""

    # echo table head
    local c1=40 c2=10 c3=10 c4=10 c5=8 c6=8 c7=8 c8=5
    echo `printf "%${c1}s | %${c2}s | %${c3}s | %${c4}s | %${c5}s | %${c6}s | %${c7}s | %${c8}s" | sed 's/ /=/g' | sed 's/|/ /g'`
    printf "%${c1}s   %${c2}s   %${c3}s   %${c4}s   %${c5}s   %${c6}s   %${c7}s   %${c8}s\n" file line lineFlag Diff 'time' timeFlag Diff pass?
    echo `printf "%${c1}s | %${c2}s | %${c3}s | %${c4}s | %${c5}s | %${c6}s | %${c7}s | %${c8}s" | sed 's/ /=/g' | sed 's/|/ /g'`
    
    # start
    for fname in $TEST_FILES
    do
        info=`echo "$1" | grep $fname`
        name=`basename $fname`
        line=`echo "$info" | grep '^line' | cut -d':' -f3`
        lineWithFlag=`echo "$info" | grep '^flagline' | cut -d':' -f3`
        time=`echo "$info" | grep '^time' | cut -d':' -f3`
        timeWithFlag=`echo "$info" | grep '^flagtime' | cut -d':' -f3`
        #output=`echo "$info" | grep '^output' | cut -d':' -f3 | tr -d ' '` # the interp result(no use)
        outputWithFlag=`echo "$info" | grep '^flagoutput' | cut -d':' -f3 | tr -d ' '` # the interp result
        succWithFlag='false'
        if [ -z $outputWithFlag ]
        then
            succWithFlag='true'
        fi
               # file, line, lineFlag, lineDiff, time, timeFlag, timeDiff
        printf "%${c1}s   %${c2}s   %${c3}s   %${c4}s   %${c5}s   %${c6}s   %${c7}s   %${c8}s\n" $name $line $lineWithFlag $((line - lineWithFlag)) $time $timeWithFlag $(( time - timeWithFlag)) $succWithFlag
    done

    # echo table's bottom
    echo `printf "%${c1}s | %${c2}s | %${c3}s | %${c4}s | %${c5}s | %${c6}s | %${c7}s | %${c8}s" | sed 's/ /=/g' | sed 's/|/ /g'`
}

contents=`getCoreSyntaxLines` # could be `getCoreSyntaxLines 1`
echo "$contents" > $FLAG.output.test
table=`makeTable "$contents"`
echo "$table" > $FLAG.table.test
