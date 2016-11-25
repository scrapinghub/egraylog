-module(egraylog_logger).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, terminate/2, code_change/3]).
-export([log/1]).


-define(HANDLER, egraylog_h).


-record(state, {
    transport_pid :: pid(),
    hostname      :: binary()
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec log(Event) -> Result when
      Event  :: term(),
      Result :: ok.
log(Event) ->
    gen_server:cast(?MODULE, {event, Event}).


init([]) ->
    Config = application:get_all_env(egraylog),
    case proplists:get_value(install_handler, Config, false) of
        false ->
            ignore;
        true ->
            ok = install_error_logger_handler(),
            TransportSettings = [
                {host, _} = lists:keyfind(host, 1, Config),
                {port, _} = lists:keyfind(port, 1, Config),
                {transport, _} = lists:keyfind(transport, 1, Config),
                {transport_opts, proplists:get_value(transport_opts, Config, [])}
            ],
            ok = install_error_logger_handler(),
            {ok, Pid} = init_connection(TransportSettings),
            erlang:monitor(process, Pid),
            {ok, #state{transport_pid = Pid, hostname = get_hostname(Config)}}
    end.




handle_info({'DOWN', _Ref, _Type, Pid, Reason}, State = #state{transport_pid = Pid}) ->
    error_logger:error_msg("transport process down with reason: ~p~n", [Reason]),
    {stop, normal, State};
handle_info(Msg, State) ->
    error_logger:error_msg("Unexpected msg: ~p~n", [Msg]),
    {noreply, State}.


handle_cast({event, Event}, State) ->
    ok = do_handle_event(State, Event),
    {noreply, State};
handle_cast(Msg, State) ->
    error_logger:error_msg("Unexpected msg: ~p~n", [Msg]),
    {noreply, State}.


handle_call(Msg, _From, State) ->
    error_logger:error_msg("Unexpected msg: ~p~n", [Msg]),
    {reply, {error, {bad_msg, Msg}}, State}.


terminate(_Reason, _State) ->
    error_logger:delete_report_handler(?HANDLER),
    ok.


code_change(_Old, State, _Extra) ->
    {ok, State}.


%% internal funcs


-spec install_error_logger_handler() -> ok.
install_error_logger_handler() ->
    case lists:member(?HANDLER, gen_event:which_handlers(error_logger)) of
        true  -> ok;
        false -> ok = error_logger:add_report_handler(?HANDLER)
    end.


-spec init_connection(TransportOpts) -> Result when
      TransportOpts :: list(),
      Result        :: {ok, pid()}
                     | {error, term()}.
init_connection(TransportOpts) ->
    egraylog:add_connection(TransportOpts).



-spec get_hostname(Config) -> Hostname when
      Config   :: proplists:proplist(),
      Hostname :: binary().
get_hostname(Config) ->
    case proplists:get_value(hostname, Config) of
        {M, F, A} -> M:F(A);
        undefined -> list_to_binary(string:strip(os:cmd("hostname"), right, $\n))
    end.


-spec do_handle_event(State, Event) -> Result when
      State    :: #state{},
      Event    :: tuple(),
      Result   :: ok.
do_handle_event(State = #state{hostname = Host, transport_pid = TransportPid}, {EventType, _GL, _EventData} = Event) ->
    case prepare_event(Event) of
        {_, ignore} ->
            ok;
        ignore ->
            ok;
        {Head, Body} ->
            JsonObj = {[
                {version,       <<"1.1">>},
                {host,          Host},
                {level,         map_level(EventType)},
                {short_message, unicode:characters_to_binary(Head)},
                {full_message,  unicode:characters_to_binary(Body)},
                {timestamp,     list_to_binary(io_lib:format("~f", [os:system_time() / 1000000000]))}
            ]},
            Json = jiffy:encode(JsonObj),
            ok = egraylog_transport:send_async(TransportPid, Json)
    end.


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


-spec prepare_event(Event) -> {Head, Body} | ignore when
      Event :: tuple(),
      Head  :: iolist(),
      Body  :: iolist().
prepare_event({error,          _, {_, Format, Args}}) -> {fheader(error, Format),        fmessage(Format, Args)};
prepare_event({info_msg,       _, {_, Format, Args}}) -> {fheader(info_msg, Format),     fmessage(Format, Args)};
prepare_event({warning_msg,    _, {_, Format, Args}}) -> {fheader(warning_msg, Format),  fmessage(Format, Args)};
prepare_event({error_report,   _, {_, Type, Args}})   -> {fheader(error_report, Type),   freport(Type, Args)};
prepare_event({info_report,    _, {_, Type, Args}})   -> {fheader(info_report, Type),    freport(Type, Args)};
prepare_event({warning_report, _, {_, Type, Args}})   -> {fheader(warning_report, Type), freport(Type, Args)};
prepare_event(_)                                      -> ignore.



-spec fheader(EventType, Format) -> Header when
      EventType :: string(),
      Format    :: atom() | string(),
      Header    :: iolist().
fheader(error, _)                        -> "ERROR REPORT";
fheader(info_msg, _)                     -> "INFO REPORT";
fheader(warning_msg, _)                  -> "WARNING REPORT";
fheader(error_report, crash_report)      -> "CRASH REPORT";
fheader(error_report, supervisor_report) -> "SUPERVISOR REPORT";
fheader(error_report, _)                 -> "ERROR REPORT";
fheader(info_report, progress)           -> "PROGRESS REPORT";
fheader(info_report, _)                  -> "INFO REPORT";
fheader(warning_report, _)               -> "WARNING REPORT".


-spec fmessage(Format, Args) -> Message when
      Format  :: atom() | string(),
      Args    :: list(),
      Message :: iolist() | ignore.
fmessage(std_error, Message) ->
    Message;
fmessage(std_info, Message) ->
    Message;
fmessage(std_warning, Message) ->
    Message;
fmessage(Format, Args) ->
    case catch io_lib:format(Format, Args) of
        {'EXIT', _} -> io_lib:format("FORMAT ERROR: ~p - ~p", [Format, Args]);
        Message     -> Message
    end.


-spec freport(Type, Args) -> Report when
      Type   :: atom() | string(),
      Args   :: list(),
      Report :: iolist() | ignore.
freport(progress, Args) ->
    [io_lib:format("~w: ~p~n", [Tag, Data]) || {Tag, Data} <- Args];
freport(supervisor_report, Args) ->
    Args = [
        proplists:get_value(supervisor, Args, ""),
        proplists:get_value(errorContext, Args, ""),
        proplists:get_value(reason, Args, ""),
        proplists:get_value(offender, Args, "")
    ],
    Format = "Supervisor: ~p~n"
             "Context:    ~p~n"
             "Reason:     ~80.18p~n"
             "Offender:   ~80.18p",
    io_lib:format(Format, Args);
freport(crash_report, Args) ->
    Format = proc_lib:format(Args, latin1, unlimited),
    io_lib:format("~s~n", [Format]);
freport(Template, Args) when is_list(Template) ->
    case catch io_lib:format(Template, Args) of
        {'EXIT', _} -> io_lib:format("FORMAT ERROR: ~p - ~p", [Template, Args]);
        Message     -> Message
    end;
freport(_, _) ->
    ignore.
