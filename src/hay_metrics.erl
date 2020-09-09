-module(hay_metrics).

-export([construct/3]).
-export([type/1]).
-export([key/1]).
-export([value/1]).

-export([register/1]).
-export([push/1]).
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

-spec construct(metric_type(), metric_raw_key(), metric_value()) -> metric().
construct(Type, Key, Val) ->
    #metric{
        type    = Type,
        key     = construct_key(Key),
        value   = Val
    }.

-spec type(metric()) -> metric_type().
type(#metric{type = Type}) ->
    Type.

-spec key(metric()) -> metric_key().
key(#metric{key = Key}) ->
    Key.

-spec value(metric()) -> metric_value().
value(#metric{value = Val}) ->
    Val.

-spec register(metric()) -> ok | {error, register_error()}.
register(#metric{type = Type, key = Key}) ->
    register_if_not_exist(Type, Key).

-spec push(metric()) -> ok.
push(#metric{type = Type, key = Key, value = Val}) ->
    case push(Type, Key, Val) of
        ok ->
            ok;
        {error, _, nonexistent_metric} ->
            ok = register_if_not_exist(Type, Key),
            ok = push(Type, Key, Val)
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

-define(SEPARATOR, $.).

-spec construct_key(metric_raw_key()) -> metric_key().
construct_key(Bin) when is_binary(Bin) ->
    Bin;
construct_key(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
construct_key(Int) when is_integer(Int) ->
    erlang:integer_to_binary(Int);
construct_key([Head | List]) when is_list(List) ->
    construct_key(List, construct_key(Head)).

-spec construct_key(metric_raw_key(), binary()) -> metric_key().
construct_key([], Acc) ->
    Acc;
construct_key([Head | Tail], Acc) ->
    construct_key(Tail, <<Acc/binary, ?SEPARATOR, (construct_key(Head))/binary>>);
construct_key(NonList, Acc) ->
    <<Acc/binary, ?SEPARATOR, (construct_key(NonList))/binary>>.

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
    folsom_metrics:new_counter(Key),
    prometheus_counter:new([{name, Key}]);
register_(gauge, Key) ->
    folsom_metrics:new_gauge(Key),
    prometheus_gauge:new([{name, Key}]).

check_metric_exist(Type, Key) ->
    case folsom_metrics:get_metric_info(Key) of % TODO check in prometheus?
        [{Key, [{type, Type}, _]}] ->
            ok;
        [{Key, [{type, OtherType}, _]}] ->
            {error, {wrong_type, OtherType}};
        [{error, Key, nonexistent_metric}] ->
            {error, nonexistent_metric}
    end.

push(counter, Key, Val) ->
    prometheus_counter:inc(Key, Val),
    folsom_metrics:notify(Key, {inc, Val});
push(gauge, Key, Val) ->
    prometheus_gauge:set(Key, Val),
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
