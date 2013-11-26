from subprocess import Popen, PIPE
import sys
import os
import time
import pickle
import argparse
import re

track_stdout = False

# command line tools
# python run_profile.py --run-with-flag dsg-%
# python run_profile.py --continue-if-possible --run-with-flag dsg-%

class RunRacket:
    def __init__(self, flag):
        self.flag = flag
        self.init_cmd = ['racket', 'python-main.rkt']

    def cmd_add_flag(self, current_cmd, flag):
        return current_cmd + ['--set-flag-false', flag]

    def cmd_add_count(self, current_cmd):
        return current_cmd + ['--count-ast-node']

    def cmd_add_countlibs(self, current_cmd):
        return current_cmd + ['--count-ast-node-with-libs']

    def cmd_add_interp(self, current_cmd):
        return current_cmd + ['--interp']

    def run_cmd(self, cmd, filename):
        """run command, opened filename as stdin
        return [stdout, stderr]"""
        with open(filename) as f:
            p = Popen(cmd, stdin=f, stdout=PIPE, stderr=PIPE)
            return p.communicate()

    def build_cmd(self, func, flag):
        cmd = self.init_cmd
        if flag != None:
            cmd = self.cmd_add_flag(cmd, flag)
        cmd = func(cmd)
        return cmd

    def count_ast_node(self, filename, flag=None):
        "given filename, return node number as int"
        cmd = self.build_cmd(self.cmd_add_count, flag)
        out, err = self.run_cmd(cmd, filename)
        return int(out)

    def count_ast_node_with_libs(self, filename, flag=None):
        "given filename, return node number as int"
        cmd = self.build_cmd(self.cmd_add_countlibs, flag)
        out, err = self.run_cmd(cmd, filename)
        return int(out)

    def interp(self, filename, flag=None):
        "given filename, return the interp [stdin, stderr]"
        cmd = self.build_cmd(self.cmd_add_interp, flag)
        result = self.run_cmd(cmd, filename)
        return result


class TestFiles:
    def __init__(self, flag, test_directory='../tests/python-reference',
                 ignoreLibs=False, ignoreNodes=False, ignoreInterp=False):
        self.ignoreLibs = ignoreLibs
        self.ignoreNodes = ignoreNodes
        self.ignoreInterp = ignoreInterp

        self.racket = RunRacket(flag)
        self.flag = flag
        self.filenames, self.path_files = self._get_files(test_directory)
        assert self.filenames != [] and self.path_files != []
        self.file_path_map = dict(zip(self.filenames, self.path_files))
        self.tracking_file = '%s.current' % self.flag
        # record
        self.file_time_map = {}
        self.file_interp_result_map = {}
        self.file_node_map = {}
        self.libs_node_info = 0

        self.flag_file_time_map = {}
        self.flag_file_interp_result_map = {}
        self.flag_file_node_map = {}
        self.flag_libs_node_info = 0

        self.init_all()
        # start
        self.start(self.flag)

    def init_all(self):
        for fname in self.filenames:
            self.file_time_map[fname] = 0
            self.file_interp_result_map[fname] = ["",""]
            self.file_node_map[fname] = 0
            self.flag_file_time_map[fname] = 0
            self.flag_file_interp_result_map[fname] = ["",""]
            self.flag_file_node_map[fname] = 0

    def _get_path(self, name):
        p = self.file_path_map[name]
        assert p != ""
        return p

    def _get_files(self, directory):
        "given directory, return the filename and filename with paths"
        p = Popen(['find', directory, '-name', '*.py'], stdout=PIPE, stderr=PIPE)
        result = p.communicate()
        assert result[1] == '', 'errors occur when find files in `%s`' % directory
        path_files = result[0].split()
        names = map(lambda f: os.path.basename(f), path_files)
        return (names, path_files)

    def get_libs_node_info(self, flag=None):
        "return lib node and its shrink ratio"
        self._track('[flag=%s]get libs' % flag)
        random_file = self.path_files[0]
        n1 = self.racket.count_ast_node_with_libs(random_file, flag)
        n2 = self.racket.count_ast_node(random_file, flag)
        return n1-n2

    def get_files_node_numbers(self, flag=None):
        # getting node numbers of all testing files
        result = {}
        for f in self.filenames:
            path = self._get_path(f)
            self._track('[flag=%s]on node: %s' % (flag, f))
            result[f] = self.racket.count_ast_node(path, flag)
        return result

    def get_interp(self, flag=None):
        # start test on all testing files
        result_map = {}
        time_map = {}
        for f in self.filenames:
            path = self._get_path(f)
            self._track('[flag=%s]on interp: %s' %(flag,f))
            start = time.time()
            result_map[f] = self.racket.interp(path, flag) #[stdout, stderr]
            time_map[f] = time.time() - start

        return (result_map, time_map)

    def start(self, flag):
        self._clean_track()
        startTime = time.time()


        # test two time, one without flag, the other with flag
        ### first: no flag
        self.make()
        if not self.ignoreLibs:
            self.libs_node_info = self.get_libs_node_info()
        if not self.ignoreNodes:
            self.file_node_map = self.get_files_node_numbers()
        if not self.ignoreInterp:
            self.file_interp_result_map, self.file_time_map = self.get_interp()

        ### second with flag
        self.make()
        if not self.ignoreLibs:
            self.flag_libs_node_info = self.get_libs_node_info(self.flag)
        if not self.ignoreNodes:
            self.flag_file_node_map = self.get_files_node_numbers(self.flag)
        if not self.ignoreInterp:
            self.flag_file_interp_result_map, self.flag_file_time_map = self.get_interp(self.flag)

        endTime = time.time()
        self._track('\nrunning time: %s' % round(endTime - startTime,2))



    def _track(self, msg):
        global track_stdout
        if track_stdout:
            print msg
            return
        with open(self.tracking_file, 'a') as f:
            f.write("%s\n" % msg)
        return
    def _clean_track(self):
        open(self.tracking_file, 'w').close()

    def make(self):
        self._track("make")
        p = Popen('make', stdout=PIPE, stderr=PIPE)
        result = p.communicate()
        assert result[1] == ''

class PrintTable:
    def __init__(self, testFiles):
        self.info  = testFiles
        self.table = []
        self.width = []

        # init
        self._set_title()
        self._fill_table()
        self.calculateColumnWidth()


    def _set_title(self):
        self.title234 = 'Comparison of the number of AST node'
        self.title567 = 'Comparison of interpretation time'

        title = ['test file', 'Previous', 'Current', 'Change',
                'Previous',
                'Current',
                'Change',
                'Passed Test?']
        self.table.append(title)

    def limitWidth(self, flt_number, digit):
        t = str(round(flt_number, digit))
        t = t.split('.')
        assert len(t) == 2
        if len(t[1]) < 3:
            t[1] = t[1] + "0" * (3 - len(t[1]))
        return ".".join(t)


    def getRatioStr(self, n1, n2, digit):
        if float(n1) == 0:
            return "0."+"0"*digit
        t = self.limitWidth((float(n1)-float(n2))/float(n1), digit) # n1, n2 might be string
        return t

    def calculateColumnWidth(self):
        self.width = [0,0,0,0,0,0,0,0]
        for row in self.table:
            for col, item in enumerate(row):
                if len(str(item)) > self.width[col]:
                    self.width[col] = len(str(item))

        # title234 should cover the 2,3,4 width
        if sum(self.width[1:4]) < len(self.title234):
            sum_width = len(self.title234)
            while sum_width % 3 != 0:
                sum_width += 1
            width = sum_width / 3;

            self.width[1:4] = [width, width, width]

        # title567 should cover the 5,6,7 width
        if sum(self.width[4:7]) < len(self.title567):
            sum_width = len(self.title567)
            while sum_width % 3 != 0:
                sum_width += 1
            width = sum_width / 3;
            self.width[4:7] = [width, width, width]

        if self.width[7] < len(self.table[0][-1]):
            self.width[7] = len(self.table[0][-1])



    def get_bottom_border(self, title=False):
        char = '-'
        if title:
            char = '='
        border='+%s+%s+%s+%s+%s+%s+%s+%s+' % tuple(map(lambda n: char*n, self.width))
        return border


    def draw_title(self):
        # draw
        s = self.get_bottom_border()
        # +2 is to fill two '+' symbol
        subtitle="|%*s|%*s|%*s|%*s|" % (self.width[0], "", sum(self.width[1:4])+2, self.title234,
                                        sum(self.width[4:7])+2, self.title567, self.width[7], "")
        s1=self.get_bottom_border(True)
        s2=self.draw_cell(self.table[0])
        s3=self.get_bottom_border()

        return s+'\n'+subtitle+'\n'+s1+'\n'+s2+'\n'+s3

    def draw_cell(self, cell):
        "cell is [filename, xx, xx, xx, ...]"
        # build the list (width0, cell0, width1, cell1...)
        s="|%*s|%*s|%*s|%*s|%*s|%*s|%*s|%*s|" % reduce(lambda x,y: x+y, zip(self.width, cell))
        return s

    def draw_cells(self):
        # draw cells excluding title
        s=[]
        for row in self.table[1:]:
            s.append(self.draw_cell(row))
        return '\n'.join(s)

    def draw_table(self):
        libs = "Libs Shrink: %s" % self.getRatioStr(self.info.libs_node_info, self.info.flag_libs_node_info, 3)

        title=self.draw_title()
        cells=self.draw_cells()
        bottom = self.get_bottom_border(True)
        return libs+'\n\n'+title+'\n'+cells+'\n'+bottom

    def save_table(self):
        table = self.draw_table()
        with open("%s.table"%self.info.flag, 'w') as f:
            f.write(table)

    def _fill_table(self):
        digit = 3

        for f in self.info.filenames:
            cell = []
            # name
            cell.append(f)
            # oldnode
            cell.append(self.info.file_node_map[f])
            # newnode
            cell.append(self.info.flag_file_node_map[f])
            # change
            cell.append(self.getRatioStr(cell[1], cell[2], digit))
            # previous time
            cell.append(self.limitWidth(self.info.file_time_map[f], digit))
            # current time
            cell.append(self.limitWidth(self.info.flag_file_time_map[f], digit))
            # time change
            cell.append(self.getRatioStr(cell[4],cell[5], digit))
            # pass?
            err = self.info.flag_file_interp_result_map[f][1]
            cell.append(str(err == ''))
            self.table.append(cell)

def get_current_flags():
    with open('python-desugar-flags.rkt', 'r') as f:
        contents = f.read()
        flags = re.findall("\(define-flag\ +(dsg[-\w]*)\)", contents)
        return flags

if __name__ == '__main__':
    avaiable_flags = get_current_flags()

    parser = argparse.ArgumentParser(description="""get testing profile of lambda-py.""")

    parser.add_argument('--generate-report', action='store_true',
                        help='given flag, the script will try to generate and print the report from existing data')
    parser.add_argument('--ignore-libs', action='store_true', default=False, help="don't collect libs info")
    parser.add_argument('--ignore-nodes', action='store_true', default=False, help="don't collect nodes info")
    parser.add_argument('--ignore-interp', action='store_true', default=False, help="don't collect interp info")
    parser.add_argument('flag', nargs='?' , help='specify a flag')
    parser.add_argument('--show-all-flags', action='store_true', help='show available flags')
    parser.add_argument('--stdout', action='store_true', default=False, help='print debug info through stdout instead of in a file')
    parser.add_argument('--test-directory', default='../tests/python-reference', help='set test-directory, default is %(default)s')

    args = parser.parse_args()
    track_stdout = args.stdout == True

    if args.show_all_flags:
        for flag in avaiable_flags:
            print flag
        exit(0)


    if args.flag == None:
        parser.print_help()
        exit(1)

    if args.flag not in avaiable_flags:
        print "flag %s not found" % args.flag
        exit(1)

    output_file = "%s.output" % args.flag

    if args.generate_report:
        if os.path.exists(output_file):
            with open(output_file, 'rb') as f:
                test = pickle.load(f)
                p = PrintTable(test)
                print p.draw_table()
        else:
            print 'cannot find %s, Abort' % output_file
        exit(1)

    test = TestFiles(args.flag, test_directory=args.test_directory,
                     ignoreLibs=args.ignore_libs,
                     ignoreNodes=args.ignore_nodes,
                     ignoreInterp=args.ignore_interp)

    with open(output_file, 'wb') as f:
        pickle.dump(test, f)

    p = PrintTable(test)
    if track_stdout:
        print p.draw_table()
    else:
        p.save_table()

