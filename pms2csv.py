#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
PMS2CSV - PMS statistics file to CSV parser
# p1: PMS output
# p2: classname (i.e. 'SMH')
# p3: node name (i.e. 'SMXA31')
# p4: CSV output
# p5: timestamp (format: 1-JAN-1970 00:00:00.000000)
"""

from __future__ import with_statement
import sys
import re
import os
import datetime as dt

__version_info__ = (0, 8)
__version__ = '.'.join(str(i) for i in __version_info__)
__author__ = 'fernandezjm'


SEPARATOR = ','
HEADER = '[MON]Sample Time'  # First column name


def main(pms_file=None,
         classname=None,
         nodename=None,
         csv_output=None,
         timestamp=None):
    header = HEADER
    paramvalue = timestamp or dt.datetime.today().strftime(
        '%Y-%m-%d %H:%M:%S.%f'
    )
    with open(pms_file, 'r') as input_file:
        lines = input_file.readlines()

    newFile = not os.path.exists(csv_output)

    with open(csv_output, 'a') as output:
        for i in range(0, len(lines)):
            result = re.search('SHOW CLASS ' + classname + ' /(.*)', lines[i])
            if result:
                paramlist = re.findall(r'\w+', result.group(1))
                num = len(paramlist)
            else:
                result = re.search('Response from', lines[i])
                if result:
                    entname = re.split('Response from ', lines[i])[1].rstrip()
                    if newFile:
                        for p in range(num):
                            header += SEPARATOR + entname + "_" + paramlist[p]
                    for nextline in range(num):
                        i += 1
                        paramvalue += SEPARATOR + re.split('\s',
                                                           re.split(':\W+',
                                                                    lines[i])[1]
                                                           )[0]
        if newFile:
            output.write(nodename + ',\n$$$ START COLUMN HEADERS $$$\n')
            output.write(header + '\n$$$ END COLUMN HEADERS $$$\n')
        output.write(paramvalue + '\n')


if __name__ == "__main__":
    main(*sys.argv[1:])
