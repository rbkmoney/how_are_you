-module(hay_woody_event_handler).

%% Commented out to compile the module without woody
%% -behaviour(woody_event_handler).

%% woody_event_handler behaviour callback
-export([handle_event/4]).

%%
%% woody_event_handler behaviour callback
%%
-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().
handle_event(
    'invoke service handler', _RpcId,
    #{function := Function, service_schema := {Handler, Service}},
    _Opts) ->
        how_are_you:metric_push(
            how_are_you:metric_construct(counter, [<<"woody">>, <<"rpc">>, Handler, Service, Function], 1)),
    ok;
handle_event(_Event, _RpcId, _Meta, _Opts) ->
    ok.