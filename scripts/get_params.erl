#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -smp enable -sname factorial -mnesia debug verbose

main(_)->
	io:format("output/size~p_isl~p_pop~p~n",[
		emas_config:problemSize(),
		config:islands(),
		config:populationSize()
		]), 
	ok.
