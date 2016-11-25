-module(egraylog_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    TransportSup = {egraylog_transport_sup, 
                    {egraylog_transport_sup, start_link, []},
                    permanent,
                    5000,
                    supervisor,
                    [egraylog_transport_sup]},
    Logger       = {egraylog_logger,
                    {egraylog_logger, start_link, []},
                    permanent,
                    brutal_kill,
                    worker,
                    [egraylog_logger]},
                    
    {ok, {{one_for_one, 4, 3600}, [TransportSup, Logger]}}.
