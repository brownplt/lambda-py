#!/usr/bin/python

import re
import subprocess
import time

filename = 'python-desugar-flags.rkt'
command = 'racket python-main.rkt --python-path `which python3.2` --test ../tests/python-reference > %s 2>1'
easycommand = 'racket python-main.rkt --python-path `which python3.2` --test ../tests/python-reference/bool > %s 2>1'
progressFile = "currentProgress.lambdapy"  # for recording which flags have been processed.

def make():
    return subprocess.call(['raco', 'make', 'python-main.rkt'])

def runTest(outputFileName):
    start = time.time()
    subprocess.call(command % outputFileName, shell=True)
    elapsed = (time.time() - start)
    appendFile(outputFileName, '\nElapsed time: %s' % elapsed)

def runTestDebug(outputFileName):
    start = time.time()
    subprocess.call(easycommand % outputFileName, shell=True)
    elapsed = (time.time() - start)
    appendFile(outputFileName, '\nElapsed time: %s' % elapsed)

def saveFile(filename, content):
    f = open(filename, 'w')
    f.write(content)
    f.close()

def clearFile(filename):
    open(filename, 'w').close()

def appendFile(filename, content):
    f = open(filename, 'a')
    f.write(content)
    f.close()

def addProgress(flag):
    appendFile(progressFile, 'start %s\n' % flag)

def switchFlag():

    contents = open(filename).read()
    flagsAndValue = re.findall("(dsg[-\w]*)\ +(.*)\)", contents) # get flags and values
    clearFile(progressFile) # clear

    for flag, v in flagsAndValue:
        addProgress(flag)

        if v == 'true':
            v = 'false'
        else:
            v = 'true'

        outputFileName = 'output_%s_%s' % (flag, v)

        # save the new flag value
        newContents = re.sub("(define\ +%s)(\ +)(.*\))" % flag, r'\1\2%s)' % v, contents)
        saveFile(filename, newContents)

        # start compile and run
        if make() == 0:
            runTest(outputFileName)
        else:
            saveFile(outputFileName, 'make error')

    # restore file
    saveFile(filename, contents)
    addProgress('done\n')


if __name__ == '__main__':
    switchFlag()

