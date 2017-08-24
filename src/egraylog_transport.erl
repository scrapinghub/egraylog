-module(egraylog_transport).

%% @doc
%% Maintain connection with graylog server over tcp/ssl.
%% All send functions expect prepared binary data, that graysend will understand.
%% Messages delimited by null terminator.
%% @end

-behaviour(gen_server).

-export([start_link/1, send/2, send_async/2]).
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, code_change/3, terminate/2]).


-type transport()     :: tcp
                       | ssl.
-type transport_mod() :: gen_tcp
                       | ssl.

-record(state, {
    parent_pid     :: pid(),
    host           :: inet:hostname()
                    | inet:ip_address(),
    port           :: inet:port_number(),
    connected      :: boolean(),
    connect_timer  :: reference(),
    socket         :: gen_tcp:socket()
                    | ssl:sslsocket(),
    transport_mod  :: transport_mod(),
    transport_opts :: proplists:proplist()
}).


-spec send_async(Pid, Bin) -> Result when
      Pid    :: pid(),
      Bin    :: binary(),
      Result :: ok.
send_async(Pid, Bin) when is_binary(Bin) ->
    gen_server:cast(Pid, {send, Bin}).


-spec send(Pid, Bin) -> Result when
      Pid    :: pid(),
      Bin    :: binary(),
      Result :: ok
              | {error, term()}.
send(Pid, Bin) when is_binary(Bin) ->
    gen_server:call(Pid, {send, Bin}).


start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).


init(Config) ->
    {parent_pid, ParentPid} = lists:keyfind(parent_pid, 1, Config),
    {host, Host} = lists:keyfind(host, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    {transport, Transport} = lists:keyfind(transport, 1, Config),
    _Ref = erlang:monitor(process, ParentPid),
    TransportMod = get_transport_module(Transport),
    TransportOpts = get_transport_options(TransportMod, proplists:get_value(transport_opts, Config, [])),
    State = #state{
        parent_pid = ParentPid,
        connected = false,
        socket = undefined,
        host = Host,
        port = Port,
        transport_mod = TransportMod,
        transport_opts = TransportOpts
    },
    {ok, init_connection(State)}.


handle_info({'DOWN', _Ref, _Type, Pid, Reason}, State = #state{parent_pid = Pid}) ->
    error_logger:info_msg("Parent pid down with reason: ~p~n", [Reason]),
    {stop, normal, State};
handle_info(init_connection, State) ->
    {noreply, init_connection(State)};
handle_info({Closed, _Sock}, State) when Closed == tcp_closed; Closed == ssl_closed ->
    {noreply, init_connection_timer(State#state{socket = undefined, connected = false})};
handle_info(Msg, State) ->
    error_logger:error_msg("Unexpected message ~tp~n", [Msg]),
    {noreply, State}.


handle_cast({send, Bin}, State) ->
    {_, NewState} = do_send(State, Bin),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call({send, Bin}, _From, State) ->
    {Reply, NewState} = do_send(State, Bin),
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
                "Failed to install ~p connection with graysend server at ~p/~p with reason: ~p~n",
                [TMod, Host, Port, Reason]
            ),
            init_connection_timer(State)
    end.


-spec do_send(State, Event) -> Result when
      State  :: #state{},
      Event  :: binary(),
      Result :: ok.
do_send(State = #state{connected = false}, _Event) ->
    Reply = {error, not_connected},
    {Reply, State};
do_send(State = #state{transport_mod = TMod, socket = Socket}, Event) ->
    case TMod:send(Socket, <<Event/binary, 0:8>>) of
        ok ->
            {ok, State};
        {error, _Reason} = R ->
            %% socket close event should arrive, so we don't bother with closing socket here
            {R, State#state{connected = false}}
    end.
