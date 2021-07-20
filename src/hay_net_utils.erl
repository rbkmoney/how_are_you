-module(hay_net_utils).

-export([ensure_ip_family_in_opts/2]).
-export([resolve/1]).
-export([resolve/2]).

-type host() :: string() | binary() | inet:ip_address().

-export_type([host/0]).

%% Internal types

-type socket_opt() :: inet:address_family() | any().

-define(ipv6(), {_, _, _, _, _, _, _, _}).
-define(ipv4(), {_, _, _, _}).

%% API

-spec ensure_ip_family_in_opts(host(), [socket_opt()]) -> [socket_opt()].
ensure_ip_family_in_opts(Host, Options) ->
    case get_opt_ip_family(Options) of
        undefined ->
            [detect_ip_family(Host) | Options];
        _IpFamily ->
            Options
    end.

-spec resolve(host()) -> {ok, inet:ip_address()} | {error, Reason :: term()}.
resolve(Host) ->
    resolve(Host, []).

-spec resolve(host(), socket_opt()) -> {ok, inet:ip_address()} | {error, Reason :: term()}.
resolve(Host, Opts) when is_binary(Host) ->
    resolve(erlang:binary_to_list(Host), Opts);
resolve(Host, Opts) ->
    case inet:parse_address(Host) of
        {ok, ?ipv6()} = Result ->
            Result;
        {ok, ?ipv4()} = Result ->
            Result;
        {error, einval} ->
            case get_opt_ip_family(Opts) of
                undefined ->
                    getaddr(Host, auto);
                IpFamily ->
                    getaddr(Host, IpFamily)
            end
    end.

%% Internals

-spec getaddr(host(), inet:address_family() | auto) ->
    {ok, inet:ip_address()} | {error, Reason :: term()}.
getaddr(Host, auto) ->
    case getaddr(Host, inet6) of
        {ok, ?ipv6()} = Result ->
            Result;
        {error, nxdomain} ->
            case getaddr(Host, inet) of
                {ok, ?ipv4()} = Result ->
                    Result;
                {error, _} = Error ->
                    Error
            end
    end;
getaddr(Host, Family) when Family =:= inet orelse Family =:= inet6 ->
    inet:getaddr(Host, Family).

-spec get_opt_ip_family([socket_opt()]) -> inet:address_family() | undefined.
get_opt_ip_family([]) ->
    undefined;
get_opt_ip_family([F | _Tail]) when F =:= inet orelse F =:= inet6 ->
    F;
get_opt_ip_family([_Other | Tail]) ->
    get_opt_ip_family(Tail).

-spec detect_ip_family(host()) -> inet:address_family().
detect_ip_family(Host) ->
    case resolve(Host) of
        {ok, ?ipv6()} ->
            inet6;
        {ok, ?ipv4()} ->
            inet;
        {error, _Details} ->
            inet6
    end.
