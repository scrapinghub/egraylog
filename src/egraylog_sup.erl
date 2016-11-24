-module(egraylog_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Config = application:get_all_env(egraylog),
    Logger = {egraylog_logger,
              {egraylog_logger, start_link, [Config]},
              permanent,
              brutal_kill,
              worker,
              [egraylog_logger]},
    {ok, {{one_for_one, 4, 3600}, [Logger]}}.
