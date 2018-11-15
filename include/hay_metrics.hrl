
-record(metric, {
    type    :: metric_type(),
    key     :: iodata(),
    value   :: term()
}).

-type metric_type() :: meter | gauge. %| timing
-type metric() :: #metric{}.
