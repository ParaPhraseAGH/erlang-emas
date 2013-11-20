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


def plot_data_objs(data_to_plot_list, figure_name):
    rows = 2
    cols = 2
    fig, ax = plt.subplots(rows, cols)
    for i in range(rows):
        for j in range(cols):
            if len(data_to_plot_list) > i*rows+j:
                obj = data_to_plot_list[i*rows+j]
                if obj.scale == 'log' and max(obj.data) <= 0:
                    ydata = map(lambda x: -x, obj.data)
                else:
                    ydata = obj.data
                ax[i][j].plot(range(len(obj.data)), ydata)
                ax[i][j].set_yscale(obj.scale)
                ax[i][j].set_title(obj.label)
    fig.suptitle(figure_name, fontsize=18)
    fig.tight_layout()


def fetch_instance(instance_name):
    common, islands = read_run_stats(instance_name)

    best_fitness = DataToPlot.from_list(islands, 'max', 'fitness', scale='log')
    mean_population = DataToPlot.from_list(islands, 'mean', 'population')
    spread_population = DataToPlot.from_list(islands, 'spread', 'population')

    subplots_data = [best_fitness, mean_population, spread_population]
    common_data = [DataToPlot(data, attr) for attr, data in common.items()]
    instance_data = subplots_data + common_data

    # print instance_name, ':', instance_data

    # plot_data_objs(subplots_data, instance_name)
    # plot_data_objs(common_data, instance_name)
    # show()
    
    # print merge_stats(islands, lambda elem_list: len(elem_list))
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


def plot_stats(instance_names, func):
    instances = [fetch_instance(instance_name) for instance_name in instance_names]
    result_to_plot = merge_instances(instances, func)
    step = 4 # 4 subplots on a figure
    for i in range(0,len(result_to_plot), step):
        plot_data_objs(result_to_plot[i:i+step], func + ' of ' + str(len(instance_names)))
    show()


def main():
    func = 'mean'
    instance_names = ['tmpskel3']
    
    # func = 'mean'
    # run_count = 4
    # instance_names = ['tmp'+str(i+1) for i in range(run_count)]

    print instance_names
    plot_stats(instance_names, func)

if __name__ == '__main__':
    main()
