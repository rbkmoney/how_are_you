-module(hay_prometheus_publisher).

-export([get_route/0]).

-spec get_route() ->
    {iodata(), module(), []}.
get_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.
