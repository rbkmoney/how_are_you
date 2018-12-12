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
    Metrics = [
        {limit, fun cg_mem_sup:limit/0},
        {swlimit, fun cg_mem_sup:swlimit/0},
        {usage, fun cg_mem_sup:usage/0},
        {swusage, fun cg_mem_sup:swusage/0},
        {cache, fun cg_mem_sup:cache/0},
        {rss, fun cg_mem_sup:rss/0},
        {rss_huge, fun cg_mem_sup:rss_huge/0},
        {mapped_file, fun cg_mem_sup:mapped_file/0},
        {pgpgin, fun cg_mem_sup:pgpgin/0},
        {pgpgout, fun cg_mem_sup:pgpgout/0},
        {swap, fun cg_mem_sup:swap/0},
        {writeback, fun cg_mem_sup:writeback/0},
        {inactive_anon, fun cg_mem_sup:inactive_anon/0},
        {active_anon, fun cg_mem_sup:active_anon/0},
        {inactive_file, fun cg_mem_sup:inactive_file/0},
        {active_file, fun cg_mem_sup:active_file/0}
    ],
    gather_metrics(Metrics, [<<"cgroup">>, <<"memory">>]).

gather_cg_cpu() ->
    gather_metrics([{usage, fun cg_cpu_sup:usage/0}], [<<"cgroup">>, <<"cpu">>]).

gather_metrics(Metrics, KeyPrefix) ->
    Data = [{Key, Fun()} || {Key, Fun} <- Metrics],
    [
        hay_metrics:construct(gauge, [KeyPrefix, Key], Value)
        || {Key, Value} <- Data, Value =/= undefined
    ].
