-module(hay_statsd_protocol).

-export([encode_metric/2]).

%% API

-spec encode_metric(hay_metrics:metric(), binary()) -> binary().
encode_metric(Metric, Prefix) ->
    Type = hay_metrics:type(Metric),
    Key = hay_metrics:key(Metric),
    Value = hay_metrics:value(Metric),
    <<Prefix/binary, Key/binary, ":", (format_value(Value))/binary, "|", (encode_type(Type)):8, "\n">>.

%% Internals

-spec encode_type(hay_metrics:metric_type()) -> byte().
encode_type(meter) ->
    %% We aggregate metrics in folsom. So just push final number
    $g;
encode_type(gauge) ->
    $g.

-spec format_value(hay_metrics:metric_value()) -> iodata().
format_value(Value) when is_integer(Value) ->
    erlang:integer_to_binary(Value);
format_value(Value) when is_float(Value) ->
    erlang:float_to_binary(Value, [{decimals, 2}]).
