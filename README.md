erlang
======
[![Build Status](https://secure.travis-ci.org/ParaPhraseAGH/erlang-emas.svg?branch=master "Build Status")](http://travis-ci.org/ParaPhraseAGH/erlang-emas)

The ParaPhrase project aims to produce a new structured design and implementation process for heterogeneous parallel architectures, where developers exploit a variety of parallel patterns to develop component based applications that can be mapped to the available hardware resources, and which may then be dynamically re-mapped to meet application needs and hardware availability.

By using the massive and efficient parallelism enabled by ParaPhrase technologies, this work allows to achieve significant speedups of individual agents as well as build much bigger multi-agent systems.

## Dependencies

To run the project on your machine you need:

* [Erlang (R17 or later)](http://www.erlang.org/)
* [Git](http://git-scm.com/)

## How to build the project

First you need to clone the repository:

    > git clone https://github.com/ParaPhraseAGH/erlang.git
    > cd erlang/

    
To build the project you can use rebar. On Linux:

    > ./rebar get-deps
    > ./rebar compile
    
On Windows:

    > rebar.cmd get-deps
    > rebar.cmd compile
    
or alternatively (both Windows and Linux, be sure to get-deps first):

    > erl -make
    
which uses the local Emakefile.

## How to run the project

To start a VM where you can run the application, first make sure that you are in the main project's folder:

    > cd emas/
    
Then you can run:

    > erl -pa ebin/ -pa deps/skel/ebin/
    
which will start the Erlang VM. Flags -pa declare the classpath where the VM will look for all the necessary .beam files, which in our case are directories `~/ebin/` and `~/deps/skel/ebin/`.

To run the application you can type:

    1> emas:start(concurrent, 10000, []).
  
which will start the algorithm. The word _emas_ is the name of the main module of your usecase (we provide the implemenentation of the EMAS algorithm as an example).
You can choose to implement a new one, which should be started in the same way as shown above.

The word _concurrent_ defines the version of the program which will be used to execute the program. Currently you can choose from _concurrent_, _hybrid_, _skel_main_ and _sequential_ versions.
The second parameter is the expected time of the execution in miliseconds and the third one is the option list. By default, the program will write all its results to stdout, so you can see if everything is configured correctly.
