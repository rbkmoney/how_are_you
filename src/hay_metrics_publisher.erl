-module(hay_metrics_publisher).
-behaviour(gen_server).

%%
-callback init(handler_options()) -> {ok, handler_state()} | {error, Reason :: term()}.
-callback get_interval(handler_state()) -> timeout().
-callback publish_metrics(metric_fold(), handler_state()) ->
    {ok, handler_state()} | {error, Reason :: term()}.

-export([start_link/1]).
-export([child_spec/2]).

%%

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 5000).

-type metric_folder(Acc) :: fun((hay_metrics:metric(), Acc) -> Acc).
-type metric_fold() :: fun((metric_folder(Acc), Acc) -> Acc).

-export_type([metric_folder/1]).
-export_type([metric_fold/0]).

%% Internal types

-record(state, {
    handler :: module(),
    handler_state :: handler_state(),
    timer = undefined :: undefined | reference()
}).

-type state() :: #state{}.
-type handler_options() :: any() | undefined.
-type handler_state() :: any().
-type handler() :: module().
-type handler_with_options() :: module() | {handler(), handler_options()}.

%% API

-spec child_spec(handler_with_options(), term()) ->
    supervisor:child_spec().

child_spec(Handler, ChildID) ->
    #{
        id => ChildID,
        start => {?MODULE, start_link, [Handler]},
        restart => permanent,
        type => worker
    }.

-spec start_link(handler_with_options()) -> {ok, pid()} | {error, term()}.

start_link(Handler) when is_atom(Handler) ->
    start_link({Handler, #{}});
start_link({Handler, Options}) ->
    gen_server:start_link(?MODULE, {Handler, Options}, []).

%%

-spec init({handler(), handler_options()}) -> {ok, state(), hibernate}.

init({Handler, Options}) ->
    {ok, HandlerState} = Handler:init(Options),
    {ok, start_timer(#state{handler = Handler, handler_state = HandlerState}), hibernate}.

-spec handle_call(term(), {pid(), term()}, state()) -> {noreply, state(), hibernate}.

handle_call(_Msg, _From, State) ->
    {noreply, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state(), hibernate}.

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state(), hibernate}.

handle_info(timeout, State0) ->
    %% TODO add some sort of monitoring
    %% to prevent metrics overloading entire system
    #state{handler = Handler, handler_state = HandlerState} = State = restart_timer(State0),
    {ok, NewHandlerState} = Handler:publish_metrics(fun hay_metrics:fold/2, HandlerState),
    {noreply, State#state{handler_state = NewHandlerState}, hibernate};

handle_info(_Msg, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% internal

-spec restart_timer(state()) -> state().

restart_timer(State = #state{timer = undefined}) ->
    start_timer(State);

restart_timer(State = #state{timer = TimerRef}) ->
    _ = erlang:cancel_timer(TimerRef),
    start_timer(State#state{timer = undefined}).

-spec start_timer(state()) -> state().

start_timer(State = #state{timer = undefined, handler = Handler, handler_state = HandlerState}) ->
    Interval = Handler:get_interval(HandlerState),
    State#state{timer = erlang:send_after(Interval, self(), timeout)}.
