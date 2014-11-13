erlang-emas
======
[![Build Status](https://secure.travis-ci.org/ParaPhraseAGH/erlang-emas.svg?branch=master "Build Status")](http://travis-ci.org/ParaPhraseAGH/erlang-emas)

The ParaPhrase project aims to produce a new structured design and implementation process for heterogeneous parallel architectures, where developers exploit a variety of parallel patterns to develop component based applications that can be mapped to the available hardware resources, and which may then be dynamically re-mapped to meet application needs and hardware availability.

By using the massive and efficient parallelism enabled by ParaPhrase technologies, this work allows to achieve significant speedups of individual agents as well as build much bigger multi-agent systems.

## EMAS

Evolutionary Multi-Agent System (EMAS) is a computational paradigm proposed by prof. Krzysztof Cetnarowicz and developed at AGH University of Science and Technology in Kraków, Poland. EMAS is an even mix of an evolutionary algorithm and multi-agent system. It is built around the concept that the evolution process takes place in a multi-agent environment, where individuals mate, die, reproduce etc.

The algorithm is designed to work without keeping any global knowledge. Agents are autonomous and capable of making their own decisions concerning their future activities. This feature makes the algorithm more scalable and enables easy parallelisation. It also makes the program often asynchronous, because each agent operates and evolves in his own pace.

This EMAS implementation uses the [MAS engine](https://github.com/ParaPhraseAGH/erlang-mas).

## Dependencies

To run the project on your machine you need:

* [Erlang (R17 or later)](http://www.erlang.org/)
* [Git](http://git-scm.com/)

## How to build the project

First you need to clone the repository:

    > git clone https://github.com/ParaPhraseAGH/erlang-emas.git
    > cd erlang-emas/

To build the project you should use the Makefile command:

    > make deps
    
Which will download and compile all necessary dependencies and the project itself.

## How to run the project

To run the project you need to implement problem-specific operators, which define the function that EMAS will optimise. Repository also includes an `emas_test_ops.erl` module which provides test operators that enable to run and test EMAS itself.


You can run `emas` run script which is located in the project root. The script has two obligatory parameters:
    
* `--time <time>` - expected running time of the simulation in milliseconds
* `--model <model>` - desired computation model (one of: `mas_sequential`, `mas_hybrid`, `mas_concurrent`, `mas_skel`; particular models are described in the [erlang-mas project documentation](https://github.com/ParaPhraseAGH/erlang-mas/wiki/MAS-Engines))

By default, the program will write all its results to stdout, so you can see if everything is configured correctly.

E.g. To run a simulation for 30 seconds with `mas_hybrid` model with default genetic operators (`emas_test_ops`) one should run:
    
    > ./emas --time 30000 --model mas_hybrid

In order to list another possible parameters and their description you can run:

    > ./emas -h


There is also a possibility of running the project from erlang shell.
To start a VM where you can run the application, first make sure that you are in the main project's folder:

    > cd erlang-emas/
    
Then you can run:

    > make shell
    
which will compile the sources and start the Erlang VM with appropriate flags.

To run the application you can type:

    1> emas:start(10000, [{model, mas_concurrent}, {genetic_ops, my_own_ops}, {problem_size, 100}]).
  
which will start the algorithm. The word `emas` is the name of the main module of our usecase. You can choose to implement a new one, which should be started in the same way as shown above.

The first parameter is the expected time of the execution in miliseconds.
The atom `mas_concurrent` defines the version of the program which will be used to execute the program. Currently you can choose from `mas_concurrent`, `mas_hybrid`, `mas_skel` and `mas_sequential` versions.

The third argument is a list of simulation properties that can be redefined from the command line. The default values are stored in `erlang-emas/src/emas_config.erl` file and can be freely edited.

Another set of parameters can be also appended in starting arguments, e.g.:

    3> emas:start(10000, [model, mas_concurrent}, {islands, 8}, {migration_probability, 0}]).
    
which overwrites the parameters of the MAS framework. The list of properties and their default values can be found in the `erlang-emas/deps/mas/src/mas_config.erl` file and can be freely edited as well. 
