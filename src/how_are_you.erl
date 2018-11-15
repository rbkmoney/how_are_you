%%% @doc Public API, supervisor and application startup.
%%% @end

-module(how_are_you).
-behaviour(supervisor).
-behaviour(application).

%% API
-export([start/0]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

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

%% Supervisor callbacks

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 60},
    ChildSpec = get_child_specs(),
    {ok, {SupFlags, ChildSpec}}.

get_child_specs() ->
    get_metrics_handlers_specs() ++ get_publishers_handlers_specs().

get_metrics_handlers_specs() ->
    lists:map(
        fun(Handler) ->
            #{
                id => Handler,
                start => {hay_metrics_handler, start_link, [Handler]},
                restart => permanent,
                type => worker
            }
        end,
        get_metrics_handlers()
    ).

get_publishers_handlers_specs() ->
    lists:map(
        fun(Handler) ->
            #{
                id => Handler,
                start => {hay_metrics_publisher, start_link, [Handler]},
                restart => permanent,
                type => worker
            }
        end,
        get_publishers_handlers()
    ).

get_metrics_handlers() ->
    [hay_vm_handler].

get_publishers_handlers() ->
    [hay_statsd_publisher].

%% Application callbacks

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
