-module(hay_statsd_publisher).
-behaviour(hay_metrics_publisher).

-export([get_interval/0]).
-export([publish_metric/1]).

%%
-spec get_interval() -> pos_integer().
get_interval() ->
    2000.

-spec publish_metric(hay_metrics:metric()) -> ok | {error, Reason :: term()}.
publish_metric(Metric) ->
    publish_metric(hay_metrics:type(Metric), hay_metrics:key(Metric), hay_metrics:value(Metric)).

%% internals

-define(RATE, 1.0).

-spec publish_metric(hay_metrics:metric_type(), hay_metrics:metric_key(), hay_metrics:metric_value()) ->
    ok.
publish_metric(meter, Key, Val) ->
    statsderl:counter(Key, Val, ?RATE);
publish_metric(gauge, Key, Val) ->
    statsderl:gauge(Key, Val, ?RATE).
