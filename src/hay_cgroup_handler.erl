-module(hay_cgroup_handler).
-behaviour(hay_metrics_handler).

%%%

-export([init/1]).
-export([get_interval/1]).
-export([gather_metrics/1]).

%% Types

-type options() :: #{
    interval => timeout()
}.

-export_type([options/0]).

%% Internal types

-record(state, {
    interval :: timeout()
}).
-type state() :: #state{}.

%% API

-spec init(options()) -> {ok, state()}.
init(Options) ->
    {ok, #state{
        interval = maps:get(interval, Options, 1000)
    }}.

-spec get_interval(state()) -> timeout().
get_interval(#state{interval = Interval}) ->
    Interval.

-spec gather_metrics(state()) -> [hay_metrics:metric()].
gather_metrics(_State) ->
    lists:flatten([
        gather_cg_memory(),
        gather_cg_cpu()
    ]).

%% Internals

gather_cg_memory() ->
    Keys = [
        limit,
        swlimit,
        usage,
        swusage,
        cache,
        rss,
        rss_huge,
        mapped_file,
        pgpgin,
        pgpgout,
        swap,
        writeback,
        inactive_anon,
        active_anon,
        inactive_file,
        active_file
    ],
    gather_metrics(cg_mem_sup, Keys, [<<"cgroup">>, <<"memory">>]).

gather_cg_cpu() ->
    gather_metrics(cg_cpu_sup, [usage], [<<"cgroup">>, <<"cpu">>]).

gather_metrics(Module, MetricKeys, KeyPrefix) ->
    Data = [{Key, erlang:apply(Module, Key, [])} || Key <- MetricKeys],
    [
        hay_metrics:construct(gauge, [KeyPrefix, Key], Value)
        || {Key, Value} <- Data, Value =/= undefined
    ].
