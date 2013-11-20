#!/usr/bin/env python

import os
import sys


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

def main(logfile, out_dir):
    
    lines = read_zeus_lines(logfile)
    attrs = ['fitness', 'population', 'fight', 'reproduction', 'death', 'migration']
    
    for attr in attrs:
        data = [float(line.split(':')[1].strip()) for line in lines if line.startswith(attr)]
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
        main(logfile, out_dir)
