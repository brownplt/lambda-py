#!/usr/bin/env python

import sys
import argparse
import re
import os


def build_cmd():
    parser = argparse.ArgumentParser()

    parser.add_argument("file_name", help="file needs to analyze")
    parser.add_argument("--showsource",
                        help="show the contents of python files",
                        action="store_true")
    args = parser.parse_args()
    return args

def extract_filename_and_error(source_file):
    with open(source_file) as f:
        contents = f.read()
    if contents == "":
        print "nothing in %s" % source_file
        exit(1)
    res = re.findall("([\w_-]+.py|=== Actual stderr ===.*?======|=== Actual stderr ===.*$)", contents, re.MULTILINE|re.DOTALL)
    files = {}
    for i in range(0, len(res), 2):
        files[res[i]] = res[i+1]

    return files

def add_space(s):
    return '   %s' % s.replace("\n", "\n   ")

def show_source(file_name_and_error):
    files_with_path = locate_files(file_name_and_error.keys())
    for fname, fname_path in files_with_path.items():
        with open(fname_path) as f:
            print fname_path
            print '~' * len(fname_path)
            print '.. error::\n'
            print add_space(file_name_and_error[fname])
            #print '\n.. code:: python\n'
            #print add_space(f.read())
            print '\n.. include:: %s\n' % fname_path
            #print '\n^^^^^^^^'

def locate_files(file_names):
    res = {}
    for root, dirs, files in os.walk("../tests/python-reference"):
        for f in file_names:
            if f in files:
                res[f] = "%s/%s" % (root, f)

    return res

if __name__ == '__main__':

    args = build_cmd()
    if args.file_name == "":
        print("file name is empty")
        exit(1)

    if args.showsource:
        files = extract_filename_and_error(args.file_name)
        show_source(files)


