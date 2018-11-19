-module(hay_metrics_publisher).
-behaviour(gen_server).

%%
-callback get_interval() -> pos_integer().
-callback publish_metric(hay_metrics:metric()) -> ok | {error, Reason :: term()}.

-export([start_link/1]).

%%

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 5000).

%%

-spec start_link(Handler :: module()) -> {ok, pid()} | {error, term()}.

start_link(Handler) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Handler, []).

%%

-record(state, {
    handler :: module(),
    timer = undefined :: undefined | reference()
}).

-type state() :: #state{}.

-spec init(Handler :: module()) -> {ok, state(), 0}.

init(Handler) ->
    {ok, #state{handler = Handler}, Handler:get_interval()}.

-spec handle_call(term(), {pid(), term()}, state()) -> {noreply, state()}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(timeout, #state{handler = Handler} = State) ->
    %% TODO add some sort of monitoring
    %% to prevent metrics overloading entire system
    ok = publish_metrics(Handler, hay_metrics:get()),
    {noreply, restart_timer(State)};

handle_info(_Msg, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {error, noimpl}.
code_change(_OldVsn, _State, _Extra) ->
    {error, noimpl}.

%% internal

publish_metrics(Handler, Metrics) ->
    %% TODO handle errors maybe?
    _ = [Handler:publish_metric(M) || M <- Metrics],
    ok.

-spec restart_timer(state()) -> state().

restart_timer(State = #state{timer = undefined}) ->
    start_timer(State);

restart_timer(State = #state{timer = TimerRef}) ->
    _ = erlang:cancel_timer(TimerRef),
    start_timer(State#state{timer = undefined}).

-spec start_timer(state()) -> state().

start_timer(State = #state{timer = undefined, handler = Handler}) ->
    Interval = Handler:get_interval(),
    State#state{timer = erlang:send_after(Interval, self(), timeout)}.
