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
getCoreNodes ()
{
    assertNotEmpty $1
    local nodes=`racket python-main.rkt --count-ast-node < $1`
    echo "node:$1:$nodes"
}

getCoreNodesWithLibs ()
{
    assertNotEmpty $1
    local nodes=`racket python-main.rkt --count-ast-node-with-libs < $1`   # slow
    echo "libnode:$1:$nodes"

}

# args: test file
getCoreNodesWithFlag()
{
    assertNotEmpty $1
    local nodes=`racket python-main.rkt --set-flag-false $FLAG --count-ast-node < $1`
    echo "flagnode:$1:$nodes"
}

# args: test file
getCoreNodesWithFlagAndLibs()
{
    assertNotEmpty $1
    local nodes=`racket python-main.rkt --set-flag-false $FLAG --count-ast-node-with-libs < $1` # slow
    echo "libflagnode:$1:$nodes"
}

# args: for loop limits
function getCoreSyntaxNodes
{
    local nodes
    local output
    local limits
    local x
    if [ -z $1 ]; then limits=-1; else limits=$1; fi

    # flag is true: 
    make
    echo "lib info" >> $FLAG.current
    local randomFile=`echo "$TEST_FILES" | head -n1`
    local coreNodeLibs=`getCoreNodesWithLibs $randomFile`
    
    echo "$coreNodeLibs"

    x=1
    for fname in $TEST_FILES
    do
        echo $fname >> $FLAG.current

        nodes=`getCoreNodes $fname`
        output=`getInterpretationTime $fname`
        echo "$nodes"
        echo "$output"
        [ $x -eq $limits ] && break
        x=`$(( x + 1 ))`
    done

    # second pass, record the number of lexical-py with flag is false
    make
    echo "lib info" >> $FLAG.current
    local coreNodeLibsFlag=`getCoreNodesWithFlagAndLibs $randomFile`
    echo "$coreNodeLibsFlag"
    
    x=1
    for fname in $TEST_FILES
    do
        echo $fname >> $FLAG.current

        nodes=`getCoreNodesWithFlag $fname`
        output=`getInterpretationWithFlagTime $fname`
        echo "$nodes"
        echo "$output"
        [ $x -eq $limits ] && break
        x=`$(( x + 1 ))`
    done
}

getRatio()
{
    assertNotEmpty $1
    assertNotEmpty $2
    local num=`python -c "print round(($1-$2)/$1.0,3)"`
    echo $num
}

printTitle()
{
    # echo table head
    local title1='test file'
    local title234='Comparison of the number of AST nodes'
    local title2='old'
    local title3='new'
    local title4='shrink'
    local title567='Comparison of interpretation time'
    local title5='old'
    local title6='new'
    local title7='shrink'
    local title8='pass?'

    #  column of file name
    local c1=${#title1}
    pythonscript="import os;files=\"\"\"$TEST_FILES\"\"\"; files = files + \"\n$title1\"; print max(map(lambda x: len(os.path.basename(x)), files.split()))"

    c1=`python -c "$pythonscript"`

    # column of ast nodes
    # TODO. this is bad, we have to make sure c2+c3+c4>length of title234
    local c2=15  #initial
    local c3=$c2
    local c4=${#title4}
    c4=`python -c "print max([$c4, $c2])"`
    local c234=`python -c "print $c2+$c3+$c4"`

    # column of time
    # TODO. this is bad, we have to make sure c5+c6+c7>length of title567
    local c5=15
    local c6=$c5
    local c7=${#title7}
    c7=`python -c "print max([$c7, $c5])"`
    local c567=`python -c "print $c5+$c6+$c7"`
    
    local c8=6
    
    # echo c1=$c1
    # echo c2=$c2
    # echo c3=$c3
    # echo c4=$c4
    # echo c234=$c234
    # echo c5=$c5
    # echo c6=$c6
    # echo c7=$c7
    # echo c8=$c8
    echo `printf "%${c1}s|%${c2}s|%${c3}s|%${c4}s|%${c5}s|%${c6}s|%${c7}s|%${c8}s" | sed 's/ /=/g' | sed 's/|/ /g'`

    printf "%${c1}s %${c234}s %${c567}s %${c8}s\n" "" "$title234" "$title567" "" 

    echo `printf "%${c1}s| %${c234}s | %${c567}s |%${c8}s" | sed 's/ /-/g' | sed 's/|/ /g'`
    printf "%${c1}s %${c2}s %${c3}s %${c4}s %${c5}s %${c6}s %${c7}s %${c8}s\n" "$title1" "$title2" "$title3" "$title4" "$title5" "$title6" "$title7" "$title8"

    echo `printf "%${c1}s|%${c2}s|%${c3}s|%${c4}s|%${c5}s|%${c6}s|%${c7}s|%${c8}s" | sed 's/ /=/g' | sed 's/|/ /g'`

    
}

# args: contents
makeTable()
{
    # echo profile and libs shrink

    libnodes=`echo "$1" | grep '^libnode' | cut -d':' -f3`
    libflagnodes=`echo "$1" | grep '^libflagnode' | cut -d':' -f3`
    local ratio=`getRatio $libnodes $libflagnodes`
    echo "libs shrink: $ratio"
    echo ""

    # echo table head
    local title1='File Name'
    local title234='Comparison of the number of AST nodes'
    local title2='Old'
    local title3='New'
    local title4='Shrink'
    local title567='Comparison of interpretation time'
    local title5='Old'
    local title6='New'
    local title7='Shrink'
    local title8='Pass?'

    #  column of file name
    local c1=${#title1}
    pythonscript="import os;files=\"\"\"$TEST_FILES\"\"\"; files = files + \"\n$title1\"; print max(map(lambda x: len(os.path.basename(x)), files.split()))"

    c1=`python -c "$pythonscript"`

    # column of ast nodes
    # TODO. this is bad, we have to make sure c2+c3+c4>length of title234
    local c2=15  #initial
    local c3=$c2
    local c4=${#title4}
    c4=`python -c "print max([$c4, $c2])"`
    local c234=`python -c "print $c2+$c3+$c4"`

    # column of time
    # TODO. this is bad, we have to make sure c5+c6+c7>length of title567
    local c5=15
    local c6=$c5
    local c7=${#title7}
    c7=`python -c "print max([$c7, $c5])"`
    local c567=`python -c "print $c5+$c6+$c7"`
    
    local c8=6
    
    # echo c1=$c1
    # echo c2=$c2
    # echo c3=$c3
    # echo c4=$c4
    # echo c234=$c234
    # echo c5=$c5
    # echo c6=$c6
    # echo c7=$c7
    # echo c8=$c8
    echo `printf "%${c1}s|%${c2}s|%${c3}s|%${c4}s|%${c5}s|%${c6}s|%${c7}s|%${c8}s" | sed 's/ /=/g' | sed 's/|/ /g'`

    printf "%${c1}s %${c234}s %${c567}s %${c8}s\n" "." "$title234" "$title567" "" 

    echo `printf "%${c1}s| %${c234}s | %${c567}s |%${c8}s" | sed 's/ /-/g' | sed 's/|/ /g'`
    printf "%${c1}s %${c2}s %${c3}s %${c4}s %${c5}s %${c6}s %${c7}s %${c8}s\n" "$title1" "$title2" "$title3" "$title4" "$title5" "$title6" "$title7" "$title8"

    echo `printf "%${c1}s|%${c2}s|%${c3}s|%${c4}s|%${c5}s|%${c6}s|%${c7}s|%${c8}s" | sed 's/ /=/g' | sed 's/|/ /g'`

    
    # start
    for fname in $TEST_FILES
    do
        info=`echo "$1" | grep $fname`
        name=`basename $fname`
        node=`echo "$info" | grep '^node' | cut -d':' -f3`
        nodeWithFlag=`echo "$info" | grep '^flagnode' | cut -d':' -f3`
        time=`echo "$info" | grep '^time' | cut -d':' -f3`
        timeWithFlag=`echo "$info" | grep '^flagtime' | cut -d':' -f3`
        #output=`echo "$info" | grep '^output' | cut -d':' -f3 | tr -d ' '` # the interp result(no use)
        outputWithFlag=`echo "$info" | grep '^flagoutput' | cut -d':' -f3 | tr -d ' '` # the interp result
        succWithFlag='false'
        if [ -z $outputWithFlag ]
        then
            succWithFlag='true'
        fi
               # file, node, nodeFlag, nodeDiff, time, timeFlag, timeDiff
        local ratio1=`getRatio $node $nodeWithFlag`
        local ratio2=`getRatio $time $timeWithFlag`
        printf "%${c1}s %${c2}s %${c3}s %${c4}s %${c5}s %${c6}s %${c7}s %${c8}s\n" $name $node $nodeWithFlag $ratio1 $time $timeWithFlag $ratio2 $succWithFlag
    done

    # echo table's bottom
    echo `printf "%${c1}s|%${c2}s|%${c3}s|%${c4}s|%${c5}s|%${c6}s|%${c7}s|%${c8}s" | sed 's/ /=/g' | sed 's/|/ /g'`
}

contents=`getCoreSyntaxNodes` # could be `getCoreSyntaxNodes 1`
echo "$contents" > $FLAG.output.test
#contents=`cat $FLAG.output.test`
table=`makeTable "$contents"`
echo "$table" > $FLAG.table.test
