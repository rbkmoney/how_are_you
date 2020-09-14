-module(hay_metrics_folsom_backend).

-export([register/2]).
-export([push/3]).
-export([get/0]).
-export([fold/2]).

-record(metric, {
    type    :: metric_type(),
    key     :: metric_key(),
    value   :: metric_value()
}).

-opaque metric() :: #metric{}.
-type metric_type() :: counter | gauge.
-type metric_key() :: binary().
-type metric_raw_key() ::
      atom()
    | integer()
    | binary()
    | maybe_improper_list(metric_raw_key(), metric_raw_key()).
-type metric_value() :: number().

-type register_error() ::
    {already_registered, {metric_key(), Type :: metric_type(), RegisteredType :: metric_type()}}.

-export_type([metric/0]).
-export_type([metric_type/0]).
-export_type([metric_key/0]).
-export_type([metric_raw_key/0]).
-export_type([metric_value/0]).
-export_type([register_error/0]).

%% Internal types

-type metric_folder() :: fun((hay_metrics:metric(), Acc) -> Acc).

%% API

-spec register(_, _) -> _. % TODO
register(Type, Key) ->
    register_if_not_exist(Type, Key).

-spec push(_, _, _) -> _.
push(Type, Key, Val) ->
    case push_(Type, Key, Val) of
        ok ->
            ok;
        {error, _, nonexistent_metric} ->
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
                {error, _, metric_already_exists} ->
                    ok;
                Error ->
                    Error
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

push_(counter, Key, Val) ->
    folsom_metrics:notify(Key, {inc, Val});
push_(gauge, Key, Val) ->
    folsom_metrics:notify(Key, Val).

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
