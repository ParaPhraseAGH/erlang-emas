#!/usr/bin/env python

import sys
import os
import log_to_files
from collections import defaultdict

attrs = ['fitness', 'population', 'fight', 'reproduction', 'death', 'migration']

def parse_dir(directory):
    logfiles = [os.path.join(directory,name) for name in os.listdir(directory) if name.startswith(proj) and not name.endswith('_run')]
    stats = defaultdict(list)
    for logfile in logfiles:
        lines = log_to_files.read_zeus_lines(logfile)
        # print logfile, len(lines)
        for attr in attrs:
            # data = [float(line.split(':')[1].strip()) for line in lines if line.startswith(attr)]
            data = [line.split(':')[1].strip() for line in lines if line.startswith(attr)]
            if data:
                # print attr, len(data)
                stats[attr].append(data)
    for key, lists in stats.items():
        # print key, len(lists)
        stats[key] = [' '.join(elem) for elem in lists]
        log_to_files.write_stats_to_file(directory, key, stats[key])
    print 'merged', len(logfiles), 'logfiles'


if __name__ == '__main__':
    # sys.argv[1] = os.path.join("output","sequential_lists","1")
    proj = 'emas'
    if len(sys.argv) < 2:
        print 'Usage:'
        print '\tpython log_to_matlab.py <directory_with_logfiles> ...'
    else:
        directories = sys.argv[1:]
        for directory in directories:
            print 'merging logs from:', directory #,' <- ', os.listdir(directory)
            parse_dir(directory)
