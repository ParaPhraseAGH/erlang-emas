%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc The module contains abstract genetic operators

-module(genetic).
-export([solution/1, evaluation/2, reproduction/2, reproduction/3, generate_agent/1]).

-include ("emas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Generates a random solution.
-spec solution(sim_params()) -> solution().
solution(SimParams) ->
    Ops = SimParams#sim_params.genetic_ops,
    Ops:solution(SimParams#sim_params.problem_size).

%% @doc Evaluates a given solution. Higher is better.
-spec evaluation(solution(), sim_params()) -> float().
evaluation(S, SimParams) ->
    Ops = SimParams#sim_params.genetic_ops,
    Ops:evaluation(S).

%% @doc Generates an agent with a random solution.
-spec generate_agent(sim_params()) -> agent().
generate_agent(SimParams) ->
    S = solution(SimParams),
    {S, evaluation(S, SimParams), SimParams#sim_params.initial_energy}.

%% @doc Reproduction function for a single agent (mutation only).
-spec reproduction(solution(), sim_params()) -> solution().
reproduction(S, SimParams) ->
    Ops = SimParams#sim_params.genetic_ops,
    case random:uniform() < SimParams#sim_params.mutation_chance of
        true -> Ops:mutation(S, SimParams);
        false -> S
    end.

%% @doc Reproduction function for a pair of agents (crossover and mutation).
-spec reproduction(solution(), solution(), sim_params()) -> [solution()].
reproduction(S1, S2, SimParams) ->
    Ops = SimParams#sim_params.genetic_ops,
    {R1, R2} = case random:uniform() < SimParams#sim_params.recombination_chance of
                   true -> Ops:recombination(S1, S2);
                   false -> {S1, S2}
               end,
    M1 = case random:uniform() < SimParams#sim_params.mutation_chance of
             true -> Ops:mutation(R1, SimParams);
             false -> R1
         end,
    M2 = case random:uniform() < SimParams#sim_params.mutation_chance of
             true -> Ops:mutation(R2, SimParams);
             false -> R2
         end,
    [M1, M2].

