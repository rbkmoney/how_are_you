-module(hay_prometheus_collector).

-behaviour(prometheus_collector).

-export([deregister_cleanup/1,
         collect_mf/2]).

-import(prometheus_model_helpers, [create_mf/4]).

-include_lib("how_are_you/include/how_are_you.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% called to collect Metric Families
-spec collect_mf(_, _) -> _.
collect_mf(_Registry, Callback) ->
  Metrics = metrics(),
  [add_metric_family(Metric, Callback)
   || Metric <- Metrics],
  ok.

add_metric_family(#metric{type = Type, key = Key, value = Value}, Callback) ->
  Callback(create_mf(Key, Key, Type, Value)).

%% called when collector deregistered
-spec deregister_cleanup(_) -> ok.
deregister_cleanup(_Registry) -> ok.

metrics() ->
    hay_cgroup_handler:gather_metrics(ok).
