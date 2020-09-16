-module(hay_metrics_prometheus_backend).

-export([register/2]).
-export([push/3]).
-export([get/0]).

-include_lib("prometheus/include/prometheus.hrl").

-define(REGISTRY, default). % TODO: config

-type metric_type() :: hay_metrics:metric_type().
-type metric_key() :: hay_metrics:metric_key().
-type metric_value() :: hay_metrics:metric_value().
-type register_error() :: {already_exists, metric_key()}.
-type push_error() :: {atom(), _}.

-spec register(metric_type(), metric_key()) ->
    ok | {error, register_error()}.

register(Type, Key) ->
    PrometheusKey = to_prometheus_key(Key),
    register_if_not_exist(Type, PrometheusKey).

-spec push(metric_type(), metric_key(), metric_value()) ->
    ok | {error, push_error()}.

push(Type, Key, Val) ->
    PrometheusKey = to_prometheus_key(Key),
    case push_(Type, PrometheusKey, Val) of
        ok ->
            ok;
        {error, {unknown_metric, PrometheusKey}} ->
            ok = register_(Type, PrometheusKey),
            push_(Type, PrometheusKey, Val)
    end.

-spec get() ->
    [].
get() ->
    []. % Not the way we will collect these

%% Internals

register_if_not_exist(Type, Key) ->
    case metric_exists(Type, Key) of
        true ->
            ok;
        false ->
            register_(Type, Key)
    end.


metric_exists(Type, Key) ->
    Table = get_prometheus_table_name(Type),
    case prometheus_metric:check_mf_exists(Table, ?REGISTRY, Key) of
        false -> false;
        _     -> true
    end.

register_(Type, Key) ->
    Spec = [{name, Key}, {help, Key}],
    Module = case Type of
        counter -> prometheus_counter;
        gauge   -> prometheus_gauge
    end,
    try Module:new(Spec) of
        ok ->
            ok
    catch error:{mf_already_exists, {?REGISTRY, Key}, _} ->
            {error, {already_exists, Key}}
    end.


push_(Type, Key, Val) ->
    try  do_push(Type, Key, Val) of
        ok ->
            ok
    catch error:{unknown_metric, ?REGISTRY, Key} ->
        {error, {unknown_metric, Key}}
    end.

do_push(counter, Key, Val) ->
    prometheus_counter:inc(Key, Val);
do_push(gauge, Key, Val) ->
    prometheus_gauge:set(Key, Val).

% This function relies on my knowledge of used key formats
% and an idea, that we can not rename existring metrics
% ideally there should be no such check and all the keys
% are expected to comply with prometheus format
to_prometheus_key(Key) ->
    binary:replace(Key, <<".">>, <<"_">>, [global]).

get_prometheus_table_name(gauge) ->
    ?PROMETHEUS_GAUGE_TABLE;
get_prometheus_table_name(counter) ->
    ?PROMETHEUS_COUNTER_TABLE.
