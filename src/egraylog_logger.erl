-module(egraylog_logger).

%% @doc
%% Goal of the module is to maintain connection with graylog server
%% over tcp/ssl.
%% All log functions expect prepared binary data, that graylog will understand.
%% Messages delimited by null terminator.
%% @end

-behaviour(gen_server).

-export([start_link/1, log_async/1, log/1]).
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).


-define(TIMEOUT, 15000).


-type transport()     :: tcp
                       | ssl.
-type transport_mod() :: gen_tcp
                       | ssl.

-record(state, {
    host           :: inet:hostname()
                    | inet:ip_address(),
    port           :: inet:port_number(),
    connected      :: boolean(),
    connect_timer  :: reference(),
    socket         :: gen_tcp:socket()
                    | ssl:socket(),
    transport_mod  :: transport_mod(),
    transport_opts :: proplists:proplist()
}).


-spec log_async(Event) -> Result when
      Event  :: binary(),
      Result :: ok.
log_async(Event) when is_binary(Event) ->
    gen_server:cast(?MODULE, {log, Event}).


-spec log(Event) -> Result when
      Event  :: binary(),
      Result :: ok
              | {error, term()}.
log(Event) when is_binary(Event) ->
    gen_server:call(?MODULE, {log, Event}).


start_link(Config) ->
    Name = proplists:get_value(name, Config, ?MODULE),
    gen_server:start_link({local, Name}, ?MODULE, Config, []).


init(Config) ->
    ok = install_error_logger_handler(proplists:get_value(install_handler, Config, false)),
    {host, Host} = lists:keyfind(host, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    {transport, Transport} = lists:keyfind(transport, 1, Config),
    TransportMod = get_transport_module(Transport),
    TransportOpts = get_transport_options(TransportMod, proplists:get_value(transport_opts, Config, [])),
    State = #state{
        connected = false,
        socket = undefined,
        host = Host,
        port = Port,
        transport_mod = TransportMod,
        transport_opts = TransportOpts
    },
    {ok, init_connection(State)}.


handle_info(init_connection, State) ->
    {noreply, init_connection(State)};
handle_info({Closed, Sock}, State) when Closed == tcp_closed; Closed == ssl_closed ->
    {noreply, init_connection_timer(State#state{socket = undefined, connected = false})};
handle_info(Msg, State) ->
    io:format("~p~n", [Msg]),
    {noreply, State}.


handle_cast({log, Event}, State) ->
    {_, NewState} = log(State, Event),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call({log, Event}, _From, State) ->
    {Reply, NewState} = log(State, Event),
    {reply, Reply, NewState};
handle_call(Msg, _From, State) ->
    {reply, {error, {bad_msg, Msg}}, State}.


code_change(_Old, State, _Extra) ->
    {ok, State}.


terminate(_Reason, #state{transport_mod = TMod, socket = Socket}) ->
    case Socket of
        undefined -> ok;
        Socket    -> TMod:close(Socket)
    end,
    ok.



%% internal funcs


-spec install_error_logger_handler(Bool) -> Result when
      Bool   :: boolean(),
      Result :: ok.
install_error_logger_handler(true) ->
    Handler = egraylog_h,
    case lists:member(Handler, gen_event:which_handlers(error_logger)) of
        true  -> ok;
        false -> ok = error_logger:add_report_handler(Handler)
    end;
install_error_logger_handler(_) ->
    error_logger:info_msg("egray handler for error_logger will not be installed~n"),
    ok.


-spec init_connection_timer(State) -> NewState when
      State    :: #state{},
      NewState :: #state{}.
init_connection_timer(State = #state{connect_timer = ConnTimer}) ->
    catch erlang:cancel_timer(ConnTimer),
    NewConnTimer = erlang:send_after(1000, self(), init_connection),
    State#state{connect_timer = NewConnTimer}.


-spec get_transport_module(Transport) -> TransportMod when
      Transport    :: transport(),
      TransportMod :: transport_mod().
get_transport_module(tcp) -> gen_tcp;
get_transport_module(ssl) -> ssl.


-spec get_transport_options(TransportMod, TransportOpts) -> NewTransportOpts when
      TransportMod     :: transport_mod(),
      TransportOpts    :: proplists:property(),
      NewTransportOpts :: proplists:property().
get_transport_options(TransportMod, TransportOpts) ->
    RawTransportOpts = [{active, true}, binary],
    case TransportMod of
        gen_tcp -> RawTransportOpts;
        ssl     -> lists:append(RawTransportOpts, TransportOpts)
    end.


-spec init_connection(State) -> NewState when
      State    :: #state{},
      NewState :: #state{}.
init_connection(State = #state{connected = true}) ->
    State;
init_connection(State = #state{transport_mod = TMod, transport_opts = TOpts, host = Host, port = Port}) ->
    case TMod:connect(Host, Port, TOpts) of
        {ok, Socket} ->
            State#state{connected = true, socket = Socket};
        {error, Reason} ->
            error_logger:error_msg(
                "Failed to install ~p connection with graylog server at ~p/~p with reason: ~p~n",
                [TMod, Host, Port, Reason]
            ),
            init_connection_timer(State)
    end.


-spec log(State, Event) -> Result when
      State  :: #state{},
      Event  :: binary(),
      Result :: ok.
log(State = #state{connected = false}, _Event) ->
    Reply = {error, not_connected},
    {Reply, State};
log(State = #state{transport_mod = TMod, socket = Socket}, Event) ->
    case TMod:send(Socket, <<Event/binary, 0:8>>) of
        ok ->
            {ok, State};
        {error, Reason} = R ->
            {R, State#state{connected = false}}
    end.
