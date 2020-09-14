-module(hay_metrics_prometheus_backend).

-export([register/2]).
-export([push/3]).
-export([get/0]).

-include_lib("prometheus/include/prometheus.hrl").

-define(REGISTRY, default).

-spec register(_, _) ->
    _. % TODO

register(Type, Key) ->
    PrometheusKey = to_prometheus_key(Key),
    register_if_not_exist(Type, PrometheusKey).

-spec push(_, _, _) ->
    _. % TODO

push(Type, Key, Val) ->
    PrometheusKey = to_prometheus_key(Key),
    try push_(Type, PrometheusKey, Val) of
        ok -> ok
    catch error:{unknown_metric, ?REGISTRY, PrometheusKey} ->
        register_(Type, PrometheusKey),
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

register_(counter, Key) -> % TODO: Catch exceptions
    prometheus_counter:new([{name, Key}, {help, Key}]); % Maybe rewrite to use dynamic calls
register_(gauge, Key) ->
    prometheus_gauge:new([{name, Key}, {help, Key}]).


push_(counter, Key, Val) ->
    prometheus_counter:inc(Key, Val);
push_(gauge, Key, Val) ->
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
