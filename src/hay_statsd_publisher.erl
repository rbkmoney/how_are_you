-module(hay_statsd_publisher).
-behaviour(hay_metrics_publisher).

-include("hay_metrics.hrl").

-export([get_interval/0]).
-export([publish_metric/1]).

%%
-spec get_interval() -> pos_integer().
get_interval() ->
    2000.

-spec publish_metric(metric()) -> ok | {error, Reason :: term()}.
publish_metric(#metric{type = Type, key = Key, value = Val}) ->
    publish_metric(Type, Key, Val).

%% internals

-define(RATE, 1.0).

publish_metric(meter, Key, Val) ->
    statsderl:counter(Key, Val, ?RATE);
publish_metric(gauge, Key, Val) ->
    statsderl:gauge(Key, Val, ?RATE).
