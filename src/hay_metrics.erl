-module(hay_metrics).

-export([construct/3]).
-export([type/1]).
-export([key/1]).
-export([value/1]).

-export([register/1]).
-export([push/1]).
-export([get/0]).

-record(metric, {
    type    :: metric_type(),
    key     :: metric_key(),
    value   :: metric_value()
}).

-opaque metric() :: #metric{}.
-type metric_type() :: meter | gauge.
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

-spec construct(metric_type(), binary() | list(), metric_value()) -> metric().
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
    push(Type, Key, Val).

-spec get() -> [metric()].
get() ->
    FolsomInfo = folsom_metrics:get_metrics_info(),
    lists:foldl(
        fun
            ({Key, [{type, Type}, _]}, Acc) when Type =:= meter orelse Type =:= gauge ->
                [get(Type, Key) | Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        FolsomInfo
    ).

%% Internals

-define(SEPARATOR, $.).

-spec construct_key(metric_raw_key()) -> metric_key().
construct_key(Bin) when is_binary(Bin) ->
    Bin;
construct_key(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
construct_key(Int) when is_integer(Int) ->
    erlang:integer_to_binary(Int);
construct_key(List) when is_list(List) ->
    construct_key(List, <<>>).

-spec construct_key(metric_raw_key(), binary()) -> metric_key().
construct_key([], Acc) ->
    Acc;
construct_key([Head | Tail], Acc) ->
    construct_key(Tail, <<Acc/binary, ?SEPARATOR, (construct_key(Head))/binary>>);
construct_key(NonList, Acc) ->
    <<Acc/binary, ?SEPARATOR, (construct_key(NonList))/binary>>.

register_if_not_exist(Type, Key) ->
    case check_metric_exist(Type, Key) of
        ok ->
            ok;
        {error, nonexistent_metric} ->
            register_(Type, Key);
        {error, {wrong_type, OtherType}} ->
            % already registered with other type
            {error, {already_registered, Key, Type, OtherType}}
    end.

register_(meter, Key) ->
    folsom_metrics:new_meter(Key);
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

push(meter, Key, Val) ->
    folsom_metrics:notify({Key, Val});
push(gauge, Key, Val) ->
    folsom_metrics:notify({Key, Val}).

get(gauge, Key) ->
    #metric{
        type    = gauge,
        key     = Key,
        value   = folsom_metrics:get_metric_value(Key)
    };
get(meter, Key) ->
    Meter = folsom_metrics:get_metric_value(Key),
    Val = proplists:get_value(count, Meter, 0),
    #metric{
        type    = meter,
        key     = Key,
        value   = Val
    }.
