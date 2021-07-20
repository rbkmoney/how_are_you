-module(hay_statsd_publisher).
-behaviour(hay_metrics_publisher).

-export([init/1]).
-export([get_interval/1]).
-export([publish_metrics/2]).

%% Types

-type host() :: hay_net_utils:host().
-type port_number() :: inet:port_number().

-type options() :: #{
    key_prefix => binary(),
    interval => timeout(),
    host => host(),
    port => port_number(),
    mtu => non_neg_integer(),
    socket_opts => [gen_udp:option()]
}.
-type socket_opt() ::
    gen_udp:option()
    | {ip, inet:socket_address()}
    | {fd, non_neg_integer()}
    | {ifaddr, inet:socket_address()}
    | inet:address_family()
    | {port, inet:port_number()}
    | {netns, file:filename_all()}
    | {bind_to_device, binary()}.

-export_type([host/0]).
-export_type([port_number/0]).
-export_type([options/0]).
-export_type([socket_opt/0]).

%% Internal types

-record(state, {
    key_prefix :: binary(),
    interval :: timeout(),
    host :: host(),
    port :: port_number(),
    mtu :: non_neg_integer(),
    socket_opts :: [socket_opt()],
    socket = undefined :: undefined | gen_udp:socket(),
    address = undefined :: undefined | inet:ip_address()
}).
-type state() :: #state{}.
-type metric() :: hay_metrics:metric().
-type packet() :: iodata().
-type metric_fold() :: hay_metrics_publisher:metric_fold().

%% API

-spec init(options()) -> {ok, state()}.
init(Options) ->
    {ok, #state{
        key_prefix = maps:get(key_prefix, Options, <<"">>),
        interval = maps:get(interval, Options, 1000),
        host = maps:get(host, Options, "localhost"),
        port = maps:get(port, Options, 8125),
        mtu = maps:get(mtu, Options, 1500),
        socket_opts = maps:get(socket_opts, Options, [])
    }}.

-spec get_interval(state()) -> timeout().
get_interval(#state{interval = Interval}) ->
    Interval.

-spec publish_metrics(metric_fold(), state()) -> {ok, state()} | {error, Reason :: term()}.
publish_metrics(Fold, State0) ->
    State = resolve(ensure_socket_exists(State0)),
    ok =
        case Fold(fun process_metrics/2, {State, [], 0}) of
            {State, Packet, Size} when Size > 0 ->
                ok = send_packet(Packet, State);
            {State, [], 0} ->
                ok
        end,
    {ok, State}.

%% Internals

-spec ensure_socket_exists(state()) -> state().
ensure_socket_exists(#state{socket = Socket} = State) when Socket =/= undefined ->
    State;
ensure_socket_exists(#state{socket = undefined, socket_opts = Opts0, host = Host} = State) ->
    Opts1 = hay_net_utils:ensure_ip_family_in_opts(Host, Opts0),
    {ok, Socket} = gen_udp:open(0, [{active, false} | Opts1]),
    State#state{socket = Socket, socket_opts = Opts1}.

-spec resolve(state()) -> state().
resolve(#state{socket_opts = Opts, host = Host} = State) ->
    {ok, Address} = hay_net_utils:resolve(Host, Opts),
    State#state{address = Address}.

-spec process_metrics(metric(), Acc) -> Acc when
    Acc :: {state(), packet(), non_neg_integer()}.
process_metrics(Metric, {State, Packet, Size}) ->
    #state{mtu = MTU, key_prefix = Prefix} = State,
    MetricBin = hay_statsd_protocol:encode_metric(Metric, Prefix),
    MetricSize = erlang:byte_size(MetricBin),
    case Size + MetricSize of
        TotalSize when TotalSize > MTU ->
            ok = send_packet(Packet, State),
            {State, [MetricBin], MetricSize};
        TotalSize when TotalSize =< MTU ->
            {State, [MetricBin | Packet], TotalSize}
    end.

-spec send_packet(packet(), state()) -> ok.
send_packet(Packet, #state{socket = Socket, port = Port, address = Address}) ->
    ok = gen_udp:send(Socket, Address, Port, Packet).
