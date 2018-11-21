-module(hay_vm_handler).
-behaviour(hay_metrics_handler).

%%%

-export([get_interval/0]).
-export([gather_metrics/0]).

%%
-spec get_interval() -> pos_integer().
get_interval() ->
    2000.

-spec gather_metrics() -> [hay_metrics:metric()].
gather_metrics() ->
    lists:flatten([
        gather_vm_memory(),
        gather_io_stat(),
        gather_system_memory(),
        gather_load_stats(),
        gather_vm_info()
    ]).

gather_vm_memory() ->
    % VM memory (bytes)
    % [
    %     {total,48365432},
    %     {processes,11622848},
    %     {processes_used,11618152},
    %     {system,36742584},
    %     {atom,512625},
    %     {atom_used,505007},
    %     {binary,896368},
    %     {code,11733934},
    %     {ets,4499664}
    % ]
    lists:map(
        fun({Key, Val}) ->
            hay_metrics:construct(gauge, [<<"vm">>, <<"memory">>, Key], Val)
        end,
        erlang:memory()
    ).

gather_io_stat() ->
    % {{input,21978695},{output,9483435}}
    {Input, Output} = erlang:statistics(io),
    lists:map(
        fun({Key, Val}) ->
            hay_metrics:construct(gauge, [<<"vm">>, <<"io">>, Key], Val)
        end,
        [Input, Output]
    ).


%% TODO do we need this?
%% Probably not cuz it can be monitored through standart tools
gather_system_memory() ->
    % System memory (bytes)
    % [
    %     {system_total_memory,7303614464},
    %     {free_swap,1073737728},
    %     {total_swap,1073737728},
    %     {cached_memory,1495052288},
    %     {buffered_memory,215597056},
    %     {free_memory,4948811776},
    %     {total_memory,7303614464}
    % ]
    lists:map(
        fun({Key, Val}) ->
            hay_metrics:construct(gauge, [<<"system">>, <<"memory">>, Key], Val)
        end,
        memsup:get_system_memory_data()
    ).

gather_load_stats() ->
    Names = [
        total_run_queue_lengths,
        total_active_tasks,
        context_switches,
        reductions
    ],
    Stats0 = [{Name, erlang:statistics(Name)} || Name <- Names],
    {WallClock, _} = erlang:statistics(wall_clock),
    {Runtime, _} = erlang:statistics(runtime),
    Stats = [{wall_clock, WallClock}, {runtime, Runtime} | Stats0],
    lists:map(
        fun({Key, Val}) ->
            hay_metrics:construct(gauge, [<<"vm">>, <<"load">>, Key], Val)
        end,
        Stats
    ).

gather_vm_info() ->
    Names = [
        atom_count, atom_limit,
        ets_count, ets_limit,
        port_count, port_limit,
        process_count, process_limit,
        schedulers
    ],
    lists:map(
        fun(Name) ->
            hay_metrics:construct(gauge, [<<"vm">>, <<"info">>, Name], erlang:system_info(system_info))
        end,
        Names
    ).
