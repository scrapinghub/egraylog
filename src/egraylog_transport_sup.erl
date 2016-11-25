-module(egraylog_transport_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([add_connection/1, add_connection/2, remove_connection/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


remove_connection(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).


add_connection(Config) ->
    add_connection(self(), Config).


add_connection(ParentPid, Config) ->
    supervisor:start_child(?MODULE, [[{parent_pid, ParentPid} | Config]]).


init([]) ->
    Child = {
        egraylog_transport,
        {egraylog_transport, start_link, []},
        temporary,
        5000,
        worker,
        [egraylog_transport]
    },
    {ok, {{simple_one_for_one, 1, 3600}, [Child]}}.
