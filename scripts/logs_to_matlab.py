#!/usr/bin/env python

'''
this script is a tool to clean and process data from logs from stdout
in order to import it to a matlab variable
and save the data to separate files (death.txt, fitness.txt, etc.)

finds the lowest number of columns within run attributes (death, fitness, population)
and clips other attributes columns to this one
'''

import sys
import os
import log_to_files
from collections import defaultdict

attrs1 = ['fitness', 'population']
attrs2 = ['fight', 'reproduction', 'death', 'migration']

def parse_dir(directory, old_format=False):
    logfiles = [os.path.join(directory,name) for name in os.listdir(directory) if name.startswith(proj) and not name.endswith('_run')]
    stats = defaultdict(list)
    for logfile in logfiles:
        lines = log_to_files.read_zeus_lines(logfile)
        old_format = log_to_files.is_format_old(lines)
        # print logfile, len(lines)
        if old_format:
            attrs = attrs1 + attrs2
            for attr in attrs:
                data = [line.split(':')[1].strip() for line in lines if line.startswith(attr)]
                if data:
                    # print attr, len(data)
                    stats[attr].append(data)
        else:
            for attr in attrs1:
                isl_dict = defaultdict(list)
            data = [line.split(' ') for line in lines if line.startswith(attr)]
            for line in data:
                isl_dict[line[1]].append(line[2])
            vals = zip(*isl_dict.values())
            if attr == 'fitness':
                data2 = [max(tup) for tup in vals]
            elif attr == 'population':
                data2 = [sum(tup) for tup in vals]
            if data2:
                # write_stats_to_file(out_dir, attr, data2)
                stats[attr].append(data2)
        for attr in attrs2:
            # data = [float(line.split(' ')[-1].strip()) for line in lines if line.startswith(attr)]
            data = [line.split(' ')[-1].strip() for line in lines if line.startswith(attr)]
            # print attr, data
            if data:
                # write_stats_to_file(out_dir, attr, data)
                stats[attr].append(data)
    # find minimal number of columns in a run
    mincols = min(min([len(elem) for elem in columns]) for columns in stats.values())
    print mincols
    for key, lists in stats.items():
        print [len(elem) for elem in lists]
        minlists = [elem[:mincols] for elem in lists]
        # keylists2 = 
        print key, 'raw:', [len(elem) for elem in lists]
        # print 'mincols:', mincols
        print key, 'cleaned:', [len(elem) for elem in minlists]
        stats[key] = [' '.join(elem) for elem in minlists]
        log_to_files.write_stats_to_file(directory, key, stats[key])
    print 'merged', len(logfiles), 'logfiles (rows) - ', mincols, 'columns'


if __name__ == '__main__':
    proj = 'emas'
    if len(sys.argv) < 2:
        print 'Usage:'
        print '\tpython log_to_matlab.py <directory_with_logfiles> ...'
    else:
        directories = sys.argv[1:]
        for directory in directories:
            print 'merging logs from:', directory #,' <- ', os.listdir(directory)
            parse_dir(directory)
