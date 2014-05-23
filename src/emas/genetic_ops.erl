-module (genetic_ops).

-include ("emas.hrl").

-callback solution(ProblemSize :: integer()) -> solution().

-callback evaluation(solution()) -> float(). 

-callback mutation(solution()) -> solution().

-callback recombination(solution(), solution()) -> {solution(), solution()}.