-module(egraylog_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).



start() ->
    {ok, _} = application:ensure_all_started(egraylog).


start(_StartType, _StartArgs) ->
    egraylog_sup:start_link().


stop(_State) ->
    ok.
