#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
PML2CSV - convert PML output to comma-separated values (CSV),
          T4 Format-2 compliant (headers)

Input parameters:

 p1: PML output
 p2: classname (i.e. 'SMH')
 p3: MOD_ELEMCLASS object (i.e. 'SMTFZ,SMH00,SMH01,SMH02,SMH03')
 p4: timestamp (format: 1-JAN-1970 00:00:00.00)
 p5: CSV output
 p6: Type of counter list [0102100110....]
 ------------------------------------------------------------------------------

Requires:  python >= 2.5

'diffline' will have 0 value when no-cumulative and last value when it is.
This way we can do the difference.

- q points to the parameter position
- p points to the parameter in cumulative list
-  cumvalue = 0 means normal counter
         = 1 means cumulative -> do the difference with the previous run
         = 2 means IW_INFO table (cumulative) => go through list and
                                       make the differencence

v2.4 - Performance improvements, fixed retrieval of old data
v2.3 - Sample-time column name set according to other T4-CSV files
v2.2 - Added Argparse, refactored
v2.1 - Refactored
v2.0 - A little bit less anti-pythonic
v1.8 - Quick'n'dirty initial version

"""
# Running on python 2.5.1, with statement introduced in 2.6
from __future__ import with_statement
from sys import argv as sys_argv
from re import search as re_search, IGNORECASE as nocase
from os.path import splitext as os_splitext, exists as os_exists


__version_info__ = (2, 4, 2)
__version__ = '.'.join(str(i) for i in __version_info__)
__author__ = 'fernandezjm'

__all__ = ('Data')
SEPARATOR = ','



def get_input_arguments():
    """
    Parse input arguments, argparse is only used when available
    (not in OpenVMS' python 2.5.1)
    """
    try:
        import argparse
        parser = argparse.ArgumentParser(description=\
                'Convert PML output to T4-compliant (Format 2) CSV format.',\
                formatter_class=argparse.RawTextHelpFormatter)
        parser.add_argument('input_file', metavar='input-file', type=str,
                            help='PML output file')
        parser.add_argument('classname', metavar='Class name', type=str,
                            help='Entity class name (i.e. "SMH"')
        parser.add_argument('elemclass', metavar='Class elements', type=str,
                            help='Elemclass object (i.e. "SMTFZ,GIW_11,GIW_12"')
        parser.add_argument('timestamp', metavar='Timestamp', type=str,
                            help='Time stamp for current sample')
        parser.add_argument('output_file', metavar='output-file', type=str, \
                            help='Resulting CSV file')
        parser.add_argument('type_of_counter', metavar='counter-list', type=str,
                            help=("Type of counter list [0102100110...], where:"
                                  "\n 0 means normal counter"
                                  "\n 1 means cumulative: "
                                  "do the difference with the previous run\n"
                                  " 2 means IW_INFO table (cumulative): go "
                                  "through list and make the difference"))
        return parser.parse_args()
    except ImportError:
        class ARGS(object):
            """
            For python 2.5 don't use argparse and set ARGS directly from sysarg
            """
            if len(sys_argv) < 7:
                print "Missing input arguments (p1..p6)"
                quit()

            def __init__(self):
                self.input_file = sys_argv[1]
                self.classname = sys_argv[2]
                self.elemclass = sys_argv[3]
                self.timestamp = sys_argv[4]
                self.output_file = sys_argv[5]
                self.type_of_counter = sys_argv[6]
        return ARGS()


class Data(object):
    """
    A simple class containing miscellaneous data
    """

    def read_input(self):
        """
        Called from pml2csv()
        Read input file
        """
        try:
            with open(self.args.input_file, 'r') as data_in:
                self.lines = data_in.readlines()
 
        except EnvironmentError:
        # parent of IOError, OSError *and* WindowsError where available
            print "Error while reading input file: " + self.args.input_file
            quit()

    def check_if_new_file(self):
        """
        Check if temporary file does already exist
        """
        if os_exists(self.args.output_file):
            try:
                with open(self.args.output_file, 'r') as _outputcsv:
                    if _outputcsv.readline().startswith(self.args.elemclass[0]):
                        self.is_new_file = False
                    else:
                        self.is_new_file = True
            except EnvironmentError:
                self.is_new_file = True
        else:
            self.is_new_file = True
  
        if os_exists(self.tmpfile):
            try:
                with open(self.tmpfile, 'r') as currentf:
                    self.current = currentf.readline()
                    self.current = self.current.split(SEPARATOR)
                    self.old_tmp_found = True
            except EnvironmentError:
                print "Error opening temporary file: " + self.tmpfile
                self.old_tmp_found = False
        else:
            self.old_tmp_found = False


    def get_res_for_cum2(self, nextline, paramlist):
        """
        Called from process_search()
        Processes the results when the type of counter is 2=TAB (IW_INFO table)
        """
        count_op = paramlist[nextline].count('_OP')
        value_res = ''
        value_diff = ''
        # Doing the loop through all operations in the IW_INFO table
        for tablerow in range(0, count_op):
            [opname, value] = self.lines[self.index + tablerow + 1].split(None,
                                                                          1)
            value = value.split()
            if tablerow == 0:
                value_diff = SEPARATOR.join(value)
            else:
                value_diff = SEPARATOR.join([value_diff]+value)
            if self.is_new_file:
                self.header += '%c%s_%s_TOTAL' % (SEPARATOR,
                                                  self.entname,
                                                  opname)
                self.header += '%c%s_%s_OK' % (SEPARATOR,
                                               self.entname,
                                               opname)
                self.header += '%c%s_%s_REJECTED' % (SEPARATOR,
                                                     self.entname,
                                                     opname)
                self.header += '%c%s_%s_TIMEOUT' % (SEPARATOR,
                                                    self.entname,
                                                    opname)
            if not self.old_tmp_found:
                if tablerow == 0:
                    value_res = SEPARATOR.join(['0'] * 4)
                else:
                    value_res = SEPARATOR.join([value_res] + ['0']*4)
            else:
                value = map(int, value)
                vlres = str(value[0] - int(self.current[self.q_index])) + \
                        SEPARATOR + \
                        str(value[1] - int(self.current[self.q_index+1])) + \
                        SEPARATOR + \
                        str(value[2] - int(self.current[self.q_index+2])) + \
                        SEPARATOR + \
                        str(value[3] - int(self.current[self.q_index+3]))
                if tablerow == 0:
                    value_res = vlres
                else:
                    value_res += SEPARATOR + vlres
       
        return value_res, value_diff


    def get_res_for_cumb(self, nextline, paramlist):
        """
        Called from process_search()
        Processes the results when the type of counter is 0=STA or 1=CUM
        """
        value = self.lines[self.index].split(':')[1].split()[0]
        cumvalue = int(self.args.type_of_counter[self.p_index])
        if cumvalue == 0:
            value_diff = '0'
        if cumvalue == 1:
            value_diff = value
        if self.is_new_file:
            self.header += SEPARATOR + self.entname + '_' + paramlist[nextline]
  
        if not self.old_tmp_found:
            value_res = str(float(value) - float(value_diff))
        else:
            value_res = str(float(value) - float(self.current[self.q_index]))
        
        return value_res, value_diff


    def process_search(self, result_text_and_index):
        """
        Called from process_entity()
        Processed search string
        """
        if result_text_and_index[0] == None:
            return()
        paramlist = result_text_and_index[0].group(1)
        paramlist = paramlist.split(' /CONTENT=')[0]
        paramlist = paramlist.split(' /INTERVAL=')[0]
        paramlist = paramlist.split('/')
        for nextline in range(0, len(paramlist)):
            self.index = result_text_and_index[1] + nextline + 1
            cumvalue = int(self.args.type_of_counter[self.p_index])
            if cumvalue == 2:
                (value_res, value_diff) = self.get_res_for_cum2(nextline,
                                                                paramlist)
                self.q_index += 4
            else:
                (value_res, value_diff) = self.get_res_for_cumb(nextline, 
                                                                paramlist)
                self.q_index += 1
            self.output_data += SEPARATOR + value_res
            if self.diffline == '':
                self.diffline = value_diff
            else:
                self.diffline += SEPARATOR + value_diff
            self.p_index += 1


    def process_entity(self, entname):
        """
        Called from pml2csv()
        Processes the whole PML file for a specific entity class
        """
        self.entname = entname
        self.p_index = 0
        map(self.process_search,
            [(re_search('SHOW CLASS %s ENTITY %s /(.*)' % (self.args.classname,
                                                           entname),
                        self.lines[i], nocase),
              i) for i in range(0, len(self.lines))])


    def pml2csv(self):
        """
        Called from init()
        Main method
        """
        self.args.elemclass = self.args.elemclass.split(SEPARATOR)
        self.read_input()
       
        try:
            with open(self.args.output_file, 'a') as output:
                # process everything, but still do not write anything, all the
                # results are saved in self.output_data
                map(self.process_entity, self.args.elemclass[1:])
                # write the headers if needed
                if self.is_new_file:
                    output.write(self.args.elemclass[0] + SEPARATOR + \
                    '\n$$$ START COLUMN HEADERS $$$\n')
                    output.write(self.header + '\n$$$ END COLUMN HEADERS $$$\n')
                # write the data
                output.write(self.output_data+'\n')
        except IOError:
            print "Error: can't open output file for writting: " + \
                  self.args.output_file
            quit()
        try:
            with open(self.tmpfile, 'w') as temp:
                temp.write(self.diffline)
        except IOError:
            print "Error: can't write to temporary file: " + self.tmpfile



    def __init__(self):
        """
        Defines class 'Data'
        """
        self.q_index = self.p_index = self.index = 0
        self.diffline = self.lines = self.entname = self.current = ''
        self.header = '[MON]Sample Time' #initial value for header
        self.args = get_input_arguments()
        self.output_data = self.args.timestamp   #initial content of each row
        #tmpfile will have a single line with the latest diffline
        self.tmpfile = os_splitext(self.args.output_file)[0] + '.tmp'
        self.check_if_new_file()
      
if __name__ == "__main__":
    DATA = Data()
    DATA.pml2csv()
