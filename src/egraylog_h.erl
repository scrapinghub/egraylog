-module(egraylog_h).

%% @doc
%% Error logger handler extension.
%% Module will intercept all incoming messages, transform them to GELF format
%% and pass to graylog_logger.
%% @end

-behaviour(gen_event).

-export([start_link/0]).
-export([init/1, handle_call/2, handle_event/2,
         handle_info/2, terminate/2, code_change/3]).


-record(state, {
    host :: binary()
}).


start_link() ->
    gen_event:start_link({local, ?MODULE}).


init(_) ->
    {ok, #state{host = get_host()}}.


handle_event({EventType, _GLeader, EventData}, State) ->
    Event = transform_event(State, EventType, EventData),
    ok = egraylog_logger:log_async(Event),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.


handle_call(Request, State) ->
    {ok, {error, {bad_request, Request}}, State}.


handle_info(_Msg, State) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_Old, State, _Extra) ->
    {ok, State}.


%% internal functions


get_host() ->
    Config = application:get_all_env(egraylog),
    StrHostName = case proplists:get_value(nodename, Config) of
        undefined -> string:strip(os:cmd("hostname"), right, $\n);
        SomeName  -> SomeName
    end,
    list_to_binary(StrHostName).


%FIXME: spec
transform_event(#state{host = Host}, EventType, {Pid, Type, Data}) ->
    JsonObj = {[
        {version,       <<"1.1">>},
        {host,          Host},
        {short_message, EventType},
        {level,         map_level(EventType)},
        {full_message,  list_to_binary(io_lib:format("~tp~n", [Data]))},
        {'_pid',        list_to_binary(pid_to_list(Pid))},
        {'_type',       to_binary(Type)}
    ]},
    jiffy:encode(JsonObj).


-spec map_level(EventType) -> Level when
      EventType :: error
                 | error_report
                 | warning_msg,
      Level     :: 0..7.
%% @private
%% map erlang event type to syslog level
%% Integer -> Severity
%%       0 -> Emergency: System is unusable
%%       1 -> Alert: Action must be taken immediately
%%       2 -> Critical: Critical conditions
%%       3 -> Error: Error conditions
%%       4 -> Warning: Warning conditions
%%       5 -> Notice: Normal but significant condition
%%       6 -> Informational: Informational messages
%%       7 -> Debug: Debug-level messages
%% @end

map_level(error)          -> 3;
map_level(error_report)   -> 3;
map_level(warning_msg)    -> 4;
map_level(warning_report) -> 4;
map_level(info_msg)       -> 6;
map_level(info_report)    -> 6;
map_level(_)              -> 7.



to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, latin1);
to_binary(String) when is_list(String) ->
    unicode:characters_to_binary(String).
