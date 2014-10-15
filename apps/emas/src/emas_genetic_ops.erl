%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc The module contains the definition of genetic operator callbacks to be implemented.
-module (emas_genetic_ops).

-include ("emas.hrl").

-callback solution(sim_params()) -> solution().

-callback evaluation(solution(), sim_params()) -> float().

-callback mutation(solution(), sim_params()) -> solution().

-callback recombination(solution(), solution(), sim_params()) -> {solution(), solution()}.