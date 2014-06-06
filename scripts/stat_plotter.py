#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
import matplotlib.pylab as plt
import numpy as np
from collections import defaultdict
import log_to_files


def readlines(filepath):
    with open(filepath) as f:
        return [float(line.strip()) for line in f]


def read_run_stats(instance_name):
    statslist = ["death", "fight", "migration", "reproduction"]
    island_statslist = ["fitness"] #, "population"] #, "stddevsum", "stddevmin"]
    common_data = {}
    island_data = []
    islands = [name for name in os.listdir(instance_name) if name.startswith('island')]
    if len(islands) == 0:
        # if skel stats, remove migration
        # statslist.remove("migration")
        data = {}
        for filename in island_statslist:
            filepath = os.path.join(instance_name, filename  + ".txt")
            # print filepath, readlines(filepath)
            try:
                data[filename] = readlines(filepath)
            except:
                print 'exception:', filename, ' stats not present'
        island_data.append(data)
    for filename in statslist:
        filepath = os.path.join(instance_name, filename + ".txt")
        # print filepath, readlines(filepath)
        try:
            common_data[filename] = readlines(filepath)
        except:
            print 'exception:', filename, ' stats not present'
    for island in islands:
        data = {}
        for filename in island_statslist:
            filepath = os.path.join(instance_name, island, filename  + ".txt")
            # print filepath, readlines(filepath)
            try:
                data[filename] = readlines(filepath)
            except:
                print 'exception:', filename, ' stats not present'
        island_data.append(data)
    return common_data, island_data


def read_all_runs_stats(instance_names):
    '''
    returns statistics from all run instances
    from current directory
    '''
    return [read_run_stats(instance_name) for instance_name in instance_names]


def plot_data(data_tuples, figure_name):
    rows = 2
    cols = 2
    fig, ax = plt.subplots(rows, cols)
    for i in range(rows):
        for j in range(cols):
            if len(data_tuples) > i*rows+j:
                label, data = data_tuples[i*rows+j]
                ax[i][j].plot(range(len(data)), data)
                ax[i][j].set_title(label)
    fig.suptitle(figure_name, fontsize=18)
    fig.tight_layout()


func_map = dict([('mean', np.mean), ('max', max), 
                 ('sum', sum), ('min', min),
                 ('median', np.median),
                 ('spread', lambda elem: max(elem) - min(elem))
                 ])


def get_func(func):
    return func_map[func] if type(func) == str else func 


def merge_stats(islands, func, stat):
    '''
    func can be one of: mean, max, min, sum, median 
    or func can be a function that gets a list of values and returns a 
    list of islands stats and returns averages of all islands per second
    islands is a list of dicts
    e.g. [{'fitness': [1,2,3], 'population': [5,5,5]}, {'fitness': [4,5,6], 'population': [7,7,7]}]
    '''
    func = get_func(func)
    groups = [island[stat] for island in islands]
    result = [func(e) for e in zip(*groups)]
    return result


class DataToPlot(object):
    """docstring for DataToPlot"""
    def __init__(self, data, attr, scale='linear', func=None, label=None):
        self.data = data
        self.func = func
        self.attr = attr
        self.scale = scale
        fs = str(func) + ' ' if func is not None else ''
        self.label = fs + attr if label is None else label

    @classmethod
    def from_list(cls, data, func, attr, scale='linear', label=None):
        data = merge_stats(data, func, attr)
        return cls(data, attr, scale=scale, func=func, label=label)

    def __str__(self): return self.pprint(10)

    def __repr__(self): return self.pprint(1)

    def pprint(self, limit):
        s = self.label + ": " + str(self.data[:limit])
        s = s if len(self.data) < limit else s + ' ... ' + str(len(self.data) - limit) + ' more'
        return s

def plot_data_objs(data_to_plot_list, figure_name, same=False):
    rows = 2
    cols = 2
    topo = figure_name.split(':')[0]
    # print "topo:",topo
    # print "figname:",figure_name
    if same:
        fig_id = figure_name.split(':')[-1]
        fig, ax = plt.subplots(rows, cols, num=fig_id)
    else:
        fig, ax = plt.subplots(rows, cols)
    for i in range(rows):
        for j in range(cols):
            if len(data_to_plot_list) > i*rows+j:
                obj = data_to_plot_list[i*rows+j]
                if obj.scale == 'log' and max(obj.data) <= 0:
                    ydata = map(lambda x: -x, obj.data)
                else:
                    ydata = obj.data
                ax[i][j].plot(range(len(obj.data)), ydata, label=topo, linewidth=2.0)
                ax[i][j].set_yscale(obj.scale)
                ax[i][j].set_title(obj.label)
                if same:
                    ax[i][j].legend(loc='lower left', shadow=True)
                    ax[i][j].legend().set_visible(False)
                    # fig.legend(loc='lower left', shadow=True)
    if fig._suptitle is None:
        suptitle = ':'.join(figure_name.split(':')[1:])
        fig.suptitle(suptitle, fontsize=18)
    # fig.tight_layout()


def fetch_instance(instance_name):
    common, islands = read_run_stats(instance_name)

    best_fitness = DataToPlot.from_list(islands, 'max', 'fitness', scale='log')
    # sum_population = DataToPlot.from_list(islands, 'sum', 'population')
    # spread_population = DataToPlot.from_list(islands, 'spread', 'population')
    # stddevmin = DataToPlot.from_list(islands, 'mean', 'stddevmin') #, scale='log')
    # stddevsum = DataToPlot.from_list(islands, 'mean', 'stddevsum') #, scale='log')

    # subplots_data = [best_fitness, sum_population, spread_population]
    subplots_data = [best_fitness] #, sum_population] #, stddevmin, stddevsum]
    common_data = [DataToPlot(data, attr) for attr, data in common.items()]
    instance_data = [subplots_data[0]] + common_data + subplots_data[1:]

    return instance_data


def merge_instances(instances, func):
    func = get_func(func)
    result = []
    for tup in zip(*instances):
        example = tup[0]
        datas = [elem.data for elem in tup]
        data = [func(data) for data in zip(*datas)]
        new_label = func.func_name + ' ' + example.label
        dtp = DataToPlot(data, example.attr, scale=example.scale, func=example.func, label=new_label)
        result.append(dtp)
    return result


def plot_stats(instance_names, func, same, plot_label=None):
    instances = [fetch_instance(instance_name) for instance_name in instance_names]
    result_to_plot = merge_instances(instances, func)
    out_dir = instance_names[0].split(os.path.sep)[0]
    if plot_label is None:
        plot_label = out_dir
    # print "instance_names",instance_names
    # print "out_dir", out_dir
    step = 4 # 4 subplots on a figure
    for i in range(0,len(result_to_plot), step):
        label = ':'.join([plot_label, func, str(len(instance_names)) + "runs", str(i/step+1)])
        plot_data_objs(result_to_plot[i:i+step], label, same)
    # show()


def main(instance_names=[], func='mean', same=False, plot_label=None):
    # print instance_names
    plot_stats(instance_names, func, same, plot_label)

def zeus(directory='.', proj="emas", func='mean'):
    for name in os.listdir(directory):
        if name.startswith(proj) and not name.endswith('_run'):
            logfile = os.path.join(directory, name)
            out_dir = logfile + '_run'
            log_to_files.parse(logfile, out_dir)
    instance_names = [os.path.join(directory,name) for name in os.listdir(directory) if name.endswith('_run')]
    return instance_names


def trim_plot_labels(first_instances):
    # print "first_instances", first_instances
    paths_elems = [instance.split(os.path.sep)[:-1] for instance in first_instances]
    result = []
    for path in zip(*paths_elems):
        if not all([path[0]==p for p in path]):
            # print "zostaw", path[0]
            result.append(path)
        #else:
        #     print "wywal", path[0]
    #
    new_names = zip(*result)
    return map(lambda x: os.path.sep.join(list(x)), new_names)


def run():
    proj = 'emas'
    func = 'mean'
    if len(sys.argv) < 2:
        print 'Usage:'
        print '\tpython stat_plotter.py {<directory_with_logfiles>|<directory_with_subdirectories} ...'
    else:
        directories = sys.argv[1:]
        instance_names_in_directories = []
        for directory in directories:
            logfiles = [os.path.join(directory,name) for name in os.listdir(directory) if name.startswith(proj) and not name.endswith('_run')]
            instance_names = [os.path.join(directory,name) for name in os.listdir(directory) if name.endswith('_run')]
            if len(logfiles) != len(instance_names):
                instance_names = zeus(directory, proj, func)
            instance_names_in_directories.append(instance_names)
        # print instance_names_in_directories
        first_instances = map(lambda x: x[0], instance_names_in_directories)
        plot_labels = trim_plot_labels(first_instances)
        for instance_names, plot_label in zip(instance_names_in_directories, plot_labels):
            if len(plot_label) == 0:
                plot_label = None
            main(instance_names, func, True, plot_label)
        fig, axes = plt.subplots(2, 2, num=1)
        ax = axes[1][1]
        l = ax.legend()#.set_visible(False)
        # position = 'lower right'
        if l.legendHandles is not None:
            position = 'center left'
            fig.legend(l.legendHandles, [x._text for x in l.texts], position)
            l.set_visible(False)
        # fig.tight_layout()
        # i=1


if __name__ == '__main__':
    run()
    # plt.legend()
    plt.show()
