-module(hay_metrics_folsom_backend).

-export([register/2]).
-export([push/3]).
-export([get/0]).
-export([fold/2]).

-type metric() :: hay_metrics:metric().
-type metric_type() :: hay_metrics:metric_type().
-type metric_key() :: hay_metrics:metric_key().
-type metric_value() :: hay_metrics:metric_value().
-type register_error() :: {already_registered, metric_key(), Type :: metric_type(), RegisteredType :: metric_type()}
    | {unsupported_metric_type, metric_type()}.
-type push_error() :: {metric_key(), nonexistent_metric}.
-type metric_folder() :: hay_metrics:metric_folder().

-include_lib("how_are_you/include/how_are_you.hrl").

%% API

-spec register(metric_type(), metric_key()) ->
    ok | {error, register_error()}.

register(Type, Key) ->
    register_if_not_exist(Type, Key).

-spec push(metric_type(), metric_key(), metric_value()) ->
    ok | {error, push_error()}.

push(Type, Key, Val) ->
    case push_(Type, Key, Val) of
        ok ->
            ok;
        {error, {Key, nonexistent_metric}} ->
            ok = register_if_not_exist(Type, Key),
            ok = push_(Type, Key, Val)
    end.

-spec get() -> [metric()].
get() ->
    fold(fun(M, Acc) -> [M | Acc] end, []).

-spec fold(metric_folder(), Acc) -> Acc when
    Acc :: any().
fold(Fun, Acc0) ->
    Acc1 = fold_counters(Fun, Acc0),
    fold_gauges(Fun, Acc1).

%% Internals

-spec register_if_not_exist(metric_type(), metric_key()) -> ok | {error, register_error()}.
register_if_not_exist(Type, Key) ->
    case check_metric_exist(Type, Key) of
        ok ->
            ok;
        {error, nonexistent_metric} ->
            case register_(Type, Key) of
                %% Avoid race condition errors with
                %% processes tries to push with the same key
                %% at the same time, when key is not yet registered
                ok ->
                    ok;
                {error, _, metric_already_exists} ->
                    ok;
                {error, Type, unsupported_metric_type} ->
                    {error,  {unsupported_metric_type, Type}}
            end;
        {error, {wrong_type, OtherType}} ->
            % already registered with other type
            {error, {already_registered, Key, Type, OtherType}}
    end.

register_(counter, Key) ->
    folsom_metrics:new_counter(Key);
register_(gauge, Key) ->
    folsom_metrics:new_gauge(Key).

check_metric_exist(Type, Key) ->
    case folsom_metrics:get_metric_info(Key) of
        [{Key, [{type, Type}, _]}] ->
            ok;
        [{Key, [{type, OtherType}, _]}] ->
            {error, {wrong_type, OtherType}};
        [{error, Key, nonexistent_metric}] ->
            {error, nonexistent_metric}
    end.

push_(Type, Key, Val) ->
    Event = case Type of
        counter -> {inc, Val};
        gauge   -> Val
    end,
    case folsom_metrics:notify(Key, Event) of
        ok -> ok;
        {error, Key, nonexistent_metric} -> {error, {Key, nonexistent_metric}}
    end.

-spec fold_counters(metric_folder(), FolderAcc) -> FolderAcc when
    FolderAcc :: any().
fold_counters(Fun, FolderAcc) ->
    Aggregation = ets:new(?MODULE, [set, private, {write_concurrency, false}, {read_concurrency, false}]),
    % TODO: Use public folsom interfaces
    Aggregation = ets:foldl(fun sum_counters/2, Aggregation, folsom_counters),
    NewFolderAcc = ets:foldl(
        fun({Key, Value}, Acc) ->
            Fun(#metric{type = counter, key = Key, value = Value}, Acc)
        end,
        FolderAcc,
        Aggregation
    ),
    true = ets:delete(Aggregation),
    NewFolderAcc.

-spec sum_counters({Key, Value}, ets:tid()) -> ets:tid() when
    Key :: {metric_key(), integer()},
    Value :: integer().
sum_counters({{CounterKey, _Part}, Value}, Table) ->
    _ = ets:update_counter(Table, CounterKey, Value, {CounterKey, Value}),
    Table.

-spec fold_gauges(metric_folder(), FolderAcc) -> FolderAcc when
    FolderAcc :: any().
fold_gauges(Fun, FolderAcc) ->
    % TODO: Use public folsom interfaces
    ets:foldl(
        fun({Key, Value}, Acc) ->
            Fun(#metric{type = gauge, key = Key, value = Value}, Acc)
        end,
        FolderAcc,
        folsom_gauges
    ).
