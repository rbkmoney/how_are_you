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
    gen_udp:option() |
    {ip, inet:socket_address()} |
    {fd, non_neg_integer()} |
    {ifaddr, inet:socket_address()} |
    inet:address_family() |
    {port, inet:port_number()} |
    {netns, file:filename_all()} |
    {bind_to_device, binary()}.

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
    socket = undefined :: undefined | gen_udp:socket()
}).
-type state() :: #state{}.
-type metrics() :: [hay_metrics:metric()].

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

-spec publish_metrics(metrics(), state()) -> {ok, state()} | {error, Reason :: term()}.
publish_metrics(Metrics, State0) ->
    State = ensure_socket_exists(State0),
    Packets = encode_metrics(Metrics, State),
    ok = send_packets(Packets, State),
    {ok, State}.

%% Internals

-spec ensure_socket_exists(state()) -> state().
ensure_socket_exists(#state{socket = Socket} = State) when Socket =/= undefined ->
    State;
ensure_socket_exists(#state{socket = undefined, socket_opts = Opts0, host = Host} = State) ->
    Opts1 = [{active, false} | Opts0],
    Opts2 = hay_net_utils:ensure_ip_family_in_opts(Host, Opts1),
    {ok, Socket} = gen_udp:open(0, Opts2),
    State#state{socket = Socket}.

-spec encode_metrics(metrics(), state()) -> [iodata()].
encode_metrics(Metrics, State) ->
    encode_packets(Metrics, State, [], [], 0).

encode_packets([], _State, Acc, [], _Size) ->
    Acc;
encode_packets([], _State, Acc, PacketAcc, _Size) ->
    [PacketAcc | Acc];
encode_packets([Metric | Rest], #state{mtu = MTU, key_prefix = Prefix} = State, Acc, PacketAcc, Size) ->
    MetricBin = hay_statsd_protocol:encode_metric(Metric, Prefix),
    MetricSize = erlang:byte_size(MetricBin),
    case Size + MetricSize of
        TotalSize when TotalSize > MTU ->
            encode_packets(Rest, State, [PacketAcc | Acc], [MetricBin], MetricSize);
        TotalSize when TotalSize =< MTU ->
            encode_packets(Rest, State, Acc, [MetricBin | PacketAcc], TotalSize)
    end.

-spec send_packets([iodata()], state()) -> ok.
send_packets(Packets, #state{socket = Socket, socket_opts = Opts, host = Host, port = Port}) ->
    {ok, Address} = hay_net_utils:resolve(Host, Opts),
    send_packets(Packets, Socket, Address, Port).

send_packets([], _Socket, _Address, _Port) ->
    ok;
send_packets([[] | Packets], Socket, Address, Port) ->
    send_packets(Packets, Socket, Address, Port);
send_packets([Packet | Rest], Socket, Address, Port) ->
    ok = gen_udp:send(Socket, Address, Port, Packet),
    send_packets(Rest, Socket, Address, Port).
