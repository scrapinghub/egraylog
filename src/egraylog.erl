-module(egraylog).

-export([add_connection/1, add_connection/2]).
-export([remove_connection/1]).


-spec add_connection(TransportOpts) -> {ok, pid()} when
    TransportOpts :: proplists:proplist().
add_connection(TransportOpts) ->
    add_connection(self(), TransportOpts).


-spec add_connection(Pid, TransportOpts) -> {ok, pid()} when
    Pid :: pid(),
    TransportOpts :: proplists:proplist().
add_connection(Pid, TransportOpts) ->
    egraylog_transport_sup:add_connection(Pid, TransportOpts).

-spec remove_connection(Pid) -> ok when
    Pid :: pid().
remove_connection(Pid) ->
    egraylog_transport_sup:remove_connection(Pid).
