%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc The module containing the algorithm parameters

-module(config).
-compile(export_all).

%% ====================================================================
%% PARAMAS PARAMETERS
%% ====================================================================

%% @doc The name of the module implementing the agent_env behaviour
agent_env() -> emas.

%% @doc Island topologies (ring, mesh)
topology() -> mesh.

%% @doc The default path to write the logs to (the folder must exist). The standard_io atom cause the logs to be sent to the standard output
logDir() -> standard_io.

%% @doc The number of islands
islands() -> 4.
%% @doc The initial size of an island's population
populationSize() -> 100.

%% @doc How often the logs are writen to output (in milliseconds)
writeInterval() -> 1000.
%% @doc How long an arena should wait for agents to come before raising an error (in milliseconds or the atom infinity)
arenaTimeout() -> 5000.