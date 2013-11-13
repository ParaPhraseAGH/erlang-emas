#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from matplotlib.pylab import *
from collections import defaultdict


def readlines(filepath):
    with open(filepath) as f:
        return [float(line.strip()) for line in f]


def read_run_stats(instance_name):
    statslist = ["death", "fight", "migration", "reproduction"]
    island_statslist = ["fitness", "population"]
    common_data = {}
    island_data = []
    islands = [name for name in os.listdir(instance_name) if name.startswith('island')]
    if len(islands) == 0:
        # if skel stats, remove migration
        statslist.remove("migration")
        data = {}
        for filename in island_statslist:
            filepath = os.path.join(instance_name, filename  + ".txt")
            # print filepath, readlines(filepath)
            data[filename] = readlines(filepath)
        island_data.append(data)
    for filename in statslist:
        filepath = os.path.join(instance_name, filename + ".txt")
        # print filepath, readlines(filepath)
        common_data[filename] = readlines(filepath)
    for island in islands:
        data = {}
        for filename in island_statslist:
            filepath = os.path.join(instance_name, island, filename  + ".txt")
            # print filepath, readlines(filepath)
            data[filename] = readlines(filepath)
        island_data.append(data)
    return common_data, island_data


def read_all_runs_stats(instance_names):
    '''
    returns statistics from all run instances
    from current directory
    '''
    return [read_run_stats(instance_name) for instance_name in instance_names]


def plot_common_data(common, figure_name):
    rows = 2
    cols = 2
    fig, ax = plt.subplots(rows, cols)
    data_tuples = common.items()
    for i in range(rows):
        for j in range(cols):
            if len(data_tuples) > i*rows+j:
                label, data = data_tuples[i*rows+j]
                ax[i][j].plot(range(len(data)), data)
                ax[i][j].set_title(label)
    fig.suptitle(figure_name, fontsize=18)
    fig.tight_layout()


def plot_island_data(island, figure_name):
    rows = 1
    cols = 2
    fig, ax = plt.subplots(rows, cols)
    data_tuples = island.items()
    for j in range(cols):
        label, data = data_tuples[j]
        # print label, data
        ax[j].plot(range(len(data)), data)
        ax[j].set_title(label)
    fig.suptitle(figure_name, fontsize=18)
    fig.tight_layout()



def merge_islands_stats(islands, func):
    '''
    func can be one of: avg, max, min, sum, median 
    or func can be a function that gets a list of values and returns a 
    gets a list of islands stats and returns averages of all islands per second
    '''
    func_map = dict([('avg', np.mean), ('max', max), 
                     ('sum', sum), ('min', min),
                     ('median', np.median)])
    if type(func) == str: func = func_map[func]
    groups = defaultdict(list)
    for island in islands:
        for attr in ['fitness', 'population']:
            groups[attr].append(island[attr])
    for key in groups:
        groups[key] = [func(e) for e in zip(*groups[key])]
    return dict(groups)


def main():
    instance_name = "tmp3"

    common, islands = read_run_stats(instance_name)
    func = 'avg'
    island = merge_islands_stats(islands, func)
    plot_common_data(common, instance_name)
    plot_island_data(island, instance_name + " - " + func)
    # print merge_islands_stats(islands, 'avg')
    # print merge_islands_stats(islands, 'max')
    # print merge_islands_stats(islands, 'min')
    # print merge_islands_stats(islands, 'sum')
    # print merge_islands_stats(islands, 'median')
    # print merge_islands_stats(islands, lambda elem_list: len(elem_list))
    show()


if __name__ == '__main__':
    main()