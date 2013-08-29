%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(hybrid).
-export([run/0, run/1, run/4]).

%% ====================================================================
%% API functions
%% ====================================================================

run(ProblemSize,Time,Islands,Path) ->
  init(Time),
  {_Time,{_Result,Pids}} = timer:tc(fun spawner/4, [ProblemSize,Time,Islands,Path]),
  cleanup(Pids),
  ok.
  %io:format("Total time:   ~p s~nFitness:     ~p~n",[_Time/1000000,_Result]).

run([A,B,C,D]) ->
  run(list_to_integer(A),
    list_to_integer(B),
    list_to_integer(C),D).

run() ->
  file:make_dir("tmp"),
  run(40,5000,2,"tmp").

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec init() -> ok
%% @doc Funkcja wykonujaca wszelkie operacje potrzebne przed uruchomieniem
%% algorytmu.
init(Time) ->
  register(supervisor,self()),
  timer:send_after(Time,theEnd).

%% @spec cleanup(List) -> ok
%% @doc Funkcja sprzatajaca po zakonczonym algorytmie, dostajaca jako
%% argument liste uruchomionych ciagle procesow.
cleanup(Pids) ->
  ok = misc_util:rambo(Pids),
  misc_util:checkIfDead(Pids),
  misc_util:clearInbox(),
  unregister(supervisor).

%% @spec spawner(int()) -> {float(),List}
%% @doc Funkcja spawnujaca wyspy, ktorych ilosc jest okreslona w argumencie.
%% Zwracany jest wynik obliczen i lista Pid.
spawner(ProblemSize,_Time,Islands,Path) ->
  %Path = io_util:genPath("Hybrid",ProblemSize,Time,Islands),
  PidsRefs = [spawn_monitor(hybrid_island,proces,[Path,X,ProblemSize]) || X <- lists:seq(1,Islands)],
  {Pids,_} = lists:unzip(PidsRefs),
  receiver(Pids,-999999).

%% @spec receiver(List1) -> {float() | timeout,List2}
%% @doc Funkcja odbierajaca wiadomosci od wysp (z wynikami) i obslugujaca je.
%% Zwracany jest koncowy wynik wraz z lista uruchomionych procesow.
receiver(Pids,BestRes) ->
  receive
    {result,Result} ->
      if Result =< BestRes ->
        receiver(Pids,BestRes);
      Result > BestRes ->
        receiver(Pids,Result)
      end;
    {agent,_From,Agent} ->
      Index = random:uniform(length(Pids)),
      lists:nth(Index, Pids) ! {agent,self(),Agent},
      receiver(Pids,BestRes);
    theEnd ->
      {BestRes,Pids};
    {'DOWN',_Ref,process,Pid,Reason} ->
      io:format("Proces ~p zakonczyl sie z powodu ~p~n",[Pid,Reason]),
      {NewPid,_Ref} = spawn_monitor(hybrid_island,proces,[]),
      io:format("Stawiam kolejna wyspe o Pid ~p~n",[NewPid]),
      receiver([NewPid|lists:delete(Pid,Pids)],BestRes)
  after config:supervisorTimeout() ->
    {timeout,Pids}
  end.

