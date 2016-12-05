-module(egraylog).

-export([add_connection/1, add_connection/2]).
-export([remove_connection/1]).


add_connection(TransportOpts) ->
    add_connection(self(), TransportOpts).


add_connection(Pid, TransportOpts) ->
    egraylog_transport_sup:add_connection(Pid, TransportOpts).


remove_connection(Pid) ->
    egraylog_transport_sup:remove_connection(Pid).
