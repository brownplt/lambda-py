#!/usr/bin/env python

from run_profile import *
import os
from subprocess import Popen, PIPE

def get_files(pattern, directory="../tests/python-reference"):
    "given directory, return the filename and filename with paths"
    p = Popen(['find', directory, '-name', pattern], stdout=PIPE, stderr=PIPE)
    result = p.communicate()
    assert result[1] == '', 'errors occur when find files in `%s`' % directory
    path_files = result[0].split()
    names = map(lambda f: os.path.basename(f), path_files)
    return (names, path_files)

def write_file(filename, contents):
    print "writing %s..." % filename
    with open(filename, 'w') as f:
        f.write(contents)

def read_file(filename):
    with open(filename, 'r') as f:
        return f.read()

# the difference between job and task is that one job could have many taskes.
def get_job_id(filename):
    names = filename.strip().split('.')
    assert names[-2].startswith('e') or names[-2].startswith('o')
    return names[-2][1:]

def get_task_id(filename):
    "return (e, id) or (o, id)"
    names = filename.strip().split('.')
    return (names[-2][0], names[-1])

def get_job_id_from_pattern(file_starts):
    files = get_files("{0}*.*".format(file_starts), ".")[0]
    assert len(files)!=0, 'could not find file starting with: %s' % file_starts
    return get_job_id(files[0])

flag="cloud_specified_lambdapy_flag.txt"
object_cached_file="cloud_cached_info.obj"
filename_list="cloud_all_test.txt"
lib_node_output="cloud_lib_node_info.txt"
lib_node_flag_output="cloud_lib_node_flag_info.txt"
libnode_script="cloud_1_libnode_and_cached_file.sh"
node_script="cloud_2_get_node.sh"
interp_script="cloud_3_get_interp.sh"
flag_libnode_script="cloud_4_flag_libnode_and_cached_file.sh"
flag_node_script="cloud_5_flag_get_node.sh"
flag_interp_script="cloud_6_flag_get_interp.sh"
clean_script="cloud_clean.sh"

all_tests = get_files("*.py")
all_tests_path = all_tests[1]
tests_number = len(all_tests_path)

script_template = """#!/bin/sh
#$ -cwd

if [ -z $SGE_TASK_ID ]
then
    echo "NO ID assigned. Abort"
    exit 1
fi
#get file name through numbers
file_path=`sed -n "$SGE_TASK_ID p" {0}`
racket=/local/bin/racket

$racket python-main.rkt {1} < $file_path
"""

clean_template = """#!/bin/sh

rm cloud_*
"""

lib_info_template = """#!/bin/sh

file_path=`sed -n "1 p" {0}`
make && racket python-main.rkt --count-ast-node-with-libs < $file_path > {1}
"""

flag_lib_info_template = """#!/bin/sh

file_path=`sed -n "1p" {0}`
make && racket python-main.rkt --set-flag-false {1} --count-ast-node-with-libs < $file_path > {2}
"""

script_usage_info = """How to run the script generated(in order):
1. To get the lib node and generate cached file
   {0}
2. To get node information
   qsub -t 1-{6} {1}
3. To get the time of interpretation
   qsub -t 1-{6} {2}

   then wait till everything is done.

1. To get the lib node info with flag and generated cached file
   {3}
2. To get the node info with flag
   qsub -t 1-{6} {4}
3. To get the time of interpretation with simplified desugaring
   qsub -t 1-{6} {5}

clean:
   {7}

Note:
1. use qstat to check the running state
2. use qacct -j jobid to check the running information
2. Interp any file once to generate the cached lib files before running on the grid.
""".format(libnode_script, node_script, interp_script,
           flag_libnode_script, flag_node_script, flag_interp_script,
           str(tests_number), clean_script)


class Info(TestFiles):
    """ members of Info class are compatible with the class TestFiles in run_profile"""
    def __init__(self, test_directory="../tests/python-reference"):
        global all_tests
        super(Info, self).__init__('foo', test_directory)
        self.filenames, self.path_files = all_tests

    def init_all(self):
        for fname in self.filenames:
            self.file_time_map[fname] = None
            self.file_interp_result_map[fname] = ["",""]
            self.file_node_map[fname] = None
            self.flag_file_time_map[fname] = None
            self.flag_file_interp_result_map[fname] = ["",""]
            self.flag_file_node_map[fname] = None

    def get_id_time_map(self, job_id):
        # get the running time info:
        p = Popen(['qacct', '-j', str(job_id)], stdout=PIPE, stderr=PIPE)
        result = p.communicate()
        assert result[1] == '', '[error qacct -j %d] %s' % (job_id, result[1])
        # get task id->time
        lines = [line.strip() for line in result[0].split('\n')
                 if line.startswith('taskid') or line.startswith('ru_wallclock')]
        lines = map(lambda x: x.split()[1], lines)
        # lines now contain [task_id, time, task_id, time ...]
        id_time_map = dict(zip(lines[0::2], lines[1::2]))
        return id_time_map

    def set_time_map(self, job_id, with_flag=False):
        print 'collecting time...'
        id_time_map = self.get_id_time_map(job_id)
        for k, v in id_time_map.items():
            f = self.filenames[int(k)-1]
            if not with_flag:
                self.file_time_map[f] = float(v)
            else:
                self.flag_file_time_map[f] = float(v)

    def set_result_map(self, job_id, with_flag=False):
        """pass in the interp result file names generated from cloud,
        read file and assign the result map"""
        print 'collecting result...'

        global flag_interp_script, interp_script, tests_number
        if with_flag:
            main_name = flag_interp_script
            result_map = self.flag_file_interp_result_map
        else:
            main_name = interp_script
            result_map = self.file_interp_result_map

        # grid file index starts from 1
        for task_id in range(1, tests_number+1):
            filename = self.filenames[task_id-1]
            e_file = "{0}.e{1}.{2}".format(main_name,job_id,task_id)
            o_file = "{0}.o{1}.{2}".format(main_name,job_id,task_id)
            result_map[filename] = [read_file(o_file), read_file(e_file)]

    def set_node_map(self, job_id, with_flag=False):
        print 'collecting node info...'
        global node_script, flag_node_script, tests_number
        if with_flag:
            main_name = flag_node_script
            node_map = self.flag_file_node_map
        else:
            main_name = node_script
            node_map = self.file_node_map

        for task_id in range(1, tests_number+1):
            filename = self.filenames[task_id-1]
            e_file = "{0}.e{1}.{2}".format(main_name,job_id,task_id)
            o_file = "{0}.o{1}.{2}".format(main_name,job_id,task_id)
            output = read_file(o_file).strip()
            error  = read_file(e_file).strip()
            if error != '':
                output = '-1'
            node_map[filename] = output


if __name__ == '__main__':
    available_flags = get_current_flags()


    parser = argparse.ArgumentParser(description="""generate script for Brown grid""")

    parser.add_argument('--flag', 
                        help='given flag, the script will try to generate script for running test on Brown grid')

    parser.add_argument('--how-to', action='store_true',
                        help='tell you how to run the generated script')

    parser.add_argument('--interp-origin-result', action='store_true',
                        help='get interp success/fail information(useful for regression test')

    parser.add_argument('--interp-flag-result', action='store_true',
                        help='get interp success/fail information(useful for regression test')

    parser.add_argument('--collect-info', action='store_true',
                        help='given desugar flag, collect node and running time information, and cache the info in a cache file')

    parser.add_argument('--print-cached-table', action='store_true',
                        help='load cached file, print the table in rst form. use --collect-info to generate cached file once before printing the cached table')

    parser.add_argument('--print-cached-failed-testcase', action='store_true',
                        help='load cached file, print failed test in rst form. use --collect-info to generate cached file once before printing the cached table')


    args = parser.parse_args()

    #================start=================
    usage = script_usage_info


    if args.how_to == True:
        print usage
        exit(0)

    if args.print_cached_table:
        info = loadObject(object_cached_file)
        if info == None:
            print "could not find object file, use --collect-info instead"
            exit(1)
        p = PrintTable(info)
        
        print p.draw_table()
        exit(0)
    
    if args.print_cached_failed_testcase:
        info = loadObject(object_cached_file)
        if info == None:
            print "could not find object file, use --collect-info instead"
            exit(1)
        p = PrintTable(info)
        print p.get_failed_test()
        exit(0)

    if args.interp_origin_result:
        info = Info() #random dsg flag
        interp_time_jobid = get_job_id_from_pattern(interp_script)
        info.set_result_map(interp_time_jobid, with_flag=False)
        p = PrintTable(info)
        print p.get_failed_test(with_flag=False)
        exit(0)
        
    if args.interp_flag_result == True:
        info = Info()
        flag_interp_time_jobid = get_job_id_from_pattern(flag_interp_script)
        info.set_result_map(flag_interp_time_jobid, with_flag=True)
        p = PrintTable(info)
        print p.get_failed_test(with_flag=True)
        exit(0)

    if args.collect_info == True:
        info = Info()

        info.libs_node_info = read_file(lib_node_output).strip()
        info.flag_libs_node_info = read_file(lib_node_flag_output).strip()

        # get related files
        node_job_id = get_job_id_from_pattern(node_script)
        info.set_node_map(node_job_id)

        flag_node_job_id = get_job_id_from_pattern(flag_node_script)
        info.set_node_map(flag_node_job_id, with_flag=True)

        interp_time_jobid = get_job_id_from_pattern(interp_script)
        flag_interp_time_jobid = get_job_id_from_pattern(flag_interp_script)

        info.set_time_map(interp_time_jobid, with_flag=False)
        info.set_time_map(flag_interp_time_jobid, with_flag=True)

        info.set_result_map(interp_time_jobid, with_flag=False)
        info.set_result_map(flag_interp_time_jobid, with_flag=True)

        writeObject(object_cached_file, info)
        print 'now, maybe you want to print cached table or check if you break the regression test?\n "-h" would tell you more'
    
        exit(0)

    if args.flag != None:
        p = Popen(["which", "racket"], stdout=PIPE, stderr=PIPE)
        [racket_path, error] = p.communicate()
        assert error == '', 'could not find racket'
        # generate the file that lists all the test files
        write_file(filename_list, "\n".join(all_tests[1]))
        # generate the file that gets lib node info
        write_file(libnode_script, lib_info_template.format(filename_list, lib_node_output))
        # generate the file that gets node info
        write_file(node_script, script_template.format(filename_list, "--count-ast-node"))
        # generate the file that runs interpreter
        write_file(interp_script, script_template.format(filename_list, "--interp"))

        # generate the file that get lib node info with flag
        write_file(flag_libnode_script, flag_lib_info_template.format(filename_list, args.flag, lib_node_flag_output))
        # generate the file that gets node info with flag
        write_file(flag_node_script, script_template.format(filename_list,
                                                 "--set-flag-false {0} --count-ast-node".format(args.flag)))
        # generate the file that runs interpreter with flag
        write_file(flag_interp_script, script_template.format(filename_list, "--set-flag-false {0} --interp".format(args.flag)))
        write_file(clean_script, clean_template)

        print usage

        exit(0)

    parser.print_help()    
    exit(0)
