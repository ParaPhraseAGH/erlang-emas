%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc The module contains the definition of genetic operator callbacks to be implemented.
-module (genetic_ops).

-include ("emas.hrl").

-callback solution(ProblemSize :: integer()) -> solution().

-callback evaluation(solution()) -> float(). 

-callback mutation(solution(), sim_params()) -> solution().

-callback recombination(solution(), solution()) -> {solution(), solution()}.