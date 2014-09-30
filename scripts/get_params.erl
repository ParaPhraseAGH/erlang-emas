#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

main([]) ->
	main(["output"]);

main([Root])->
	io:format(Root ++ "/size~p_isl~p_pop~p~n",
		[emas_config:problemSize(),
		 config:islands(),
		 config:populationSize()
		]), 
	ok.
