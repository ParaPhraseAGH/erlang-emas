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
    > cd erlang/

To build the project you should use the Makefile command:

    > make deps
    
Which will download and compile all necessary dependencies and the project itself.

## How to run the project

To run the project you need to implement problem-specific operators, which define the function that EMAS will optimise. Repository also includes an `emas_test_ops.erl` module which provides test operators that enable to run and test EMAS itself.

To start a VM where you can run the application, first make sure that you are in the main project's folder:

    > cd emas/
    
Then you can run:

    > make shell
    
which will compile the sources and start the Erlang VM with appropriate flags.

To run the application you can type:

    1> emas:start(mas_concurrent, 10000).
  
which will start the algorithm. The word _emas_ is the name of the main module of our usecase. You can choose to implement a new one, which should be started in the same way as shown above.

The atom _mas_concurrent_ defines the version of the program which will be used to execute the program. Currently you can choose from _mas_concurrent_, _mas_hybrid_, _mas_skel_ and _mas_sequential_ versions.
The second parameter is the expected time of the execution in miliseconds. By default, the program will write all its results to stdout, so you can see if everything is configured correctly.

There are also two other starting functions provided in the `emas` module:

    2> emas:start(mas_concurrent, 10000, [{genetic_ops, my_own_ops}, {problem_size, 100}]).
    
Here the third argument is a list of simulation properties that can be redefined from the command line. The default values are stored in `~/deps/mas/etc/emas.config` file and can be freely edited.

Another list can be also provided as a starting argument:

    3> emas:start(mas_concurrent, 10000, [{problem_size, 100}], [{islands, 8}, {migration_probability, 0}]).
    
which overwrites the parameters of the MAS framework. The list of properties and their default values can be found in the `~/deps/mas/etc/mas.config` file and can be freely edited as well. 
