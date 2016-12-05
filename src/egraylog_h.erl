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


start_link() ->
    gen_event:start_link({local, ?MODULE}).


init(_) ->
    {ok, no_state}.


handle_event(Event, State) ->
    ok = egraylog_logger:log(Event),
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
