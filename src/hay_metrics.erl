-module(hay_metrics).

-export([construct/3]).
-export([type/1]).
-export([key/1]).
-export([value/1]).

-export([register/1]).
-export([push/1]).
-export([get/0]).
-export([fold/2]).

-include_lib("how_are_you/include/how_are_you.hrl").

-opaque metric() :: #metric{}.
-type metric_type() :: counter | gauge.
-type metric_key() :: binary().
-type metric_raw_key() ::
      atom()
    | integer()
    | binary()
    | maybe_improper_list(metric_raw_key(), metric_raw_key()).
-type metric_value() :: number().

-type metric_backend_error() :: {error, {module(), _}}.
-type metric_folder() :: fun((hay_metrics:metric(), Acc) -> Acc).

-export_type([metric/0]).
-export_type([metric_type/0]).
-export_type([metric_key/0]).
-export_type([metric_raw_key/0]).
-export_type([metric_value/0]).
-export_type([metric_folder/0]).
-export_type([metric_backend_error/0]).

-define(BACKENDS, []).

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

%%

-spec register(metric()) -> ok | {error, metric_backend_error()}.
register(#metric{type = Type, key = Key}) ->
    Results = [{Backend, Backend:register(Type, Key)} || Backend <- get_backends()],
    lists:foldl(
        fun fold_results/2,
        ok,
        Results
    ).

-spec fold_results({module(), ok | {error, _}}, ok | metric_backend_error()) ->
    ok | metric_backend_error().

fold_results({_, ok}, ok) -> % TODO: it's basically a foldwhile, maybe add foldwhile to genlib?
    ok;
fold_results(_, {Backend, {error, Error}}) ->
    {error, {Backend, Error}}; % TODO: try to find a better way to specify failing backend
fold_results({_, {error, _}} = Error, _) ->
    Error.


-spec push(metric()) -> ok.
push(#metric{type = Type, key = Key, value = Val}) ->
    Results = [{Backend, Backend:push(Type, Key, Val)} || Backend <- get_backends()],
    lists:foldl(
        fun fold_results/2,
        ok,
        Results
    ).

-spec get() -> [metric()].
get() ->
    fold(fun(M, Acc) -> [M | Acc] end, []).

-spec fold(metric_folder(), Acc) -> Acc when
    Acc :: any().
fold(Fun, Acc0) ->
    hay_metrics_folsom_backend:fold(Fun, Acc0).

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

get_backends() ->
    case application:get_env(how_are_you, metric_backends) of
        {ok, Val} -> Val;
        undefined -> []
    end.
