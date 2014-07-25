%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc The module contains abstract genetic operators

-module(genetic).
-export([solution/1, evaluation/1, reproduction/1, reproduction/2, generatePopulation/1, generateAgent/1]).

-include ("emas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Generates a random solution.
-spec solution(integer()) -> solution().
solution(ProblemSize) ->
    Ops = emas_config:genetic_ops(),
    Ops:solution(ProblemSize).
    % [-50 + random:uniform() * 100 || _ <- lists:seq(1, ProblemSize)].

%% @doc Evaluates a given solution. Higher is better.
-spec evaluation(solution()) -> float().
evaluation(S) ->
    Ops = emas_config:genetic_ops(),
    Ops:evaluation(S).

%% @doc Generates an agent with a random solution.
-spec generateAgent(pos_integer()) -> agent().
generateAgent(ProblemSize) ->
    S = solution(ProblemSize),
    {S, evaluation(S), emas_config:initialEnergy()}.

%% @doc Generates a population of agents with random solutions.
-spec generatePopulation(pos_integer()) -> [agent()].
generatePopulation(ProblemSize) ->
    [generateAgent(ProblemSize) || _ <- lists:seq(1, config:populationSize())].

%% @doc Reproduction function for a single agent (mutation only).
-spec reproduction(solution()) -> solution().
reproduction(S) ->
    Ops = emas_config:genetic_ops(),
    case random:uniform() < emas_config:mutationChance() of
        true -> Ops:mutation(S);
        false -> S
    end.
%% @doc Reproduction function for a pair of agents (crossover and mutation).
-spec reproduction(solution(),solution()) -> [solution()].
reproduction(S1, S2) ->
    Ops = emas_config:genetic_ops(),
    {R1, R2} = case random:uniform() < emas_config:recombinationChance() of
                   true -> Ops:recombination(S1, S2);
                   false -> {S1, S2}
               end,
    M1 = case random:uniform() < emas_config:mutationChance() of
             true -> Ops:mutation(R1);
             false -> R1
         end,
    M2 = case random:uniform() < emas_config:mutationChance() of
             true -> Ops:mutation(R2);
             false -> R2
         end,
    [M1, M2].

