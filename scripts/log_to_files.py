#!/usr/bin/env python

import os
import sys
from collections import defaultdict


def read_zeus_lines(filepath):
    with open(filepath) as f:
        return [line.strip() for line in f]


def write_stats_to_file(directory, attr, data):
    if not os.path.exists(directory):
        os.makedirs(directory)
    filepath = os.path.join(directory, attr + '.txt')
    with open(filepath, 'w') as f:
        for line in data:
            f.write(str(line) + '\n')

def parse(logfile, out_dir):
    
    lines = read_zeus_lines(logfile)
    attrs1 = ['fitness', 'population']
    attrs2 = ['fight', 'reproduction', 'death', 'migration']
    
    for attr in attrs1:
        isl_dict = defaultdict(list)
        data = [line.split(' ') for line in lines if line.startswith(attr)]
        for line in data:
            isl_dict[line[1]].append(float(line[2]))
        vals = zip(*isl_dict.values())
        if attr == 'fitness':
            data2 = [max(tup) for tup in vals]
        else:
            data2 = [sum(tup) for tup in vals]
        if data2:
            write_stats_to_file(out_dir, attr, data2)

    for attr in attrs2:
        data = [float(line.split(' ')[-1].strip()) for line in lines if line.startswith(attr)]
        # print attr, data
        if data:
            write_stats_to_file(out_dir, attr, data)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print 'Usage:'
        print '> python log_to_files.py <logfile_to_parse> [<output_directory>]'
    else:
        logfile = sys.argv[1]
        if len(sys.argv) == 2:
            out_dir = logfile + '_run'
        else:
            out_dir = sys.argv[2]
        parse(logfile, out_dir)
