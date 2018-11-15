-module(hay_metrics).
-include("hay_metrics.hrl").

-export([construct/3]).
-export([register/1]).
-export([push/1]).
-export([get/0]).

-spec construct(metric_type(), binary() | list(), term()) -> metric().
construct(Type, Key, Val) ->
    #metric{
        type    = Type,
        key     = construct_key(Key),
        value   = Val
    }.

-spec register(metric()) -> ok | {error, already_registered}.
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

construct_key(Bin) when is_binary(Bin) andalso byte_size(Bin) > 0 ->
    Bin;
construct_key([FirstKey | KeyList]) ->
    lists:foldl(
        fun(Key, Acc) ->
            BinKey = key_to_binary(Key),
            <<Acc/binary, ?SEPARATOR, BinKey/binary>>
        end,
        key_to_binary(FirstKey),
        KeyList
    ).

key_to_binary(Bin) when is_binary(Bin) ->
    Bin;
key_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
key_to_binary(Int) when is_integer(Int) ->
    erlang:integer_to_binary(Int);
key_to_binary(String) when is_list(String) ->
    unicode:characters_to_binary(String, utf8).

register_if_not_exist(Type, Key) ->
    case check_metric_exist(Type, Key) of
        ok ->
            ok;
        {error, nonexistent_metric} ->
            register_(Type, Key);
        {error, wrong_type} ->
            % already registered with other type
            {error, already_registered}
    end.

register_(meter, Key) ->
    folsom_metrics:new_meter(Key);
register_(gauge, Key) ->
    folsom_metrics:new_gauge(Key).

check_metric_exist(Type, Key) ->
    case folsom_metrics:get_metric_info(Key) of
        [{Key, [{type, Type}, _]}] ->
            ok;
        [{Key, [{type, _OtherType}, _]}] ->
            {error, wrong_type};
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
