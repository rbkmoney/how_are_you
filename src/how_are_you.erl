%%% @doc Public API, supervisor and application startup.
%%% @end

-module(how_are_you).
-behaviour(supervisor).
-behaviour(application).

%% Application API
-export([start/0]).
-export([stop/0]).

%% Metrics API
-export([metric_construct/3]).
-export([metric_register/1]).
-export([metric_push/1]).
-export([get_metrics_route/0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

%% Types

-type metric() :: hay_metrics:metric().
-type metric_key() :: hay_metrics:metric_raw_key().
-type metric_type() :: hay_metrics:metric_type().
-type metric_value() :: hay_metrics:metric_value().

-export_type([metric/0]).
-export_type([metric_type/0]).
-export_type([metric_key/0]).
-export_type([metric_value/0]).

%%
%% API
%%
-spec start() ->
    {ok, _}.
start() ->
    application:ensure_all_started(?MODULE).

-spec stop() ->
    ok.
stop() ->
    application:stop(?MODULE).

-spec metric_construct(metric_type(), metric_key(), metric_value()) ->
    metric().
metric_construct(Type, Key, Val) ->
    hay_metrics:construct(Type, Key, Val).

-spec metric_register(metric()) ->
    ok | {error, hay_metrics:register_error()}.
metric_register(Metric) ->
    hay_metrics:register(Metric).

-spec metric_push(metric()) -> ok.
metric_push(Metric) ->
    hay_metrics:push(Metric).

%% Supervisor callbacks

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 100, period => 10},
    ChildSpec = get_child_specs(),
    {ok, {SupFlags, ChildSpec}}.

get_child_specs() ->
    get_metrics_handlers_specs() ++ get_publishers_handlers_specs().

get_metrics_handlers_specs() ->
    [hay_metrics_handler:child_spec(H, H) || H <- get_metrics_handlers()].

get_publishers_handlers_specs() ->
    [hay_metrics_publisher:child_spec(H, H) || H <- get_publishers_handlers()].

get_metrics_handlers() ->
    application:get_env(?MODULE, metrics_handlers, [hay_vm_handler, hay_cgroup_handler]).

get_publishers_handlers() ->
    application:get_env(?MODULE, metrics_publishers, []).

%% Application callbacks

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
