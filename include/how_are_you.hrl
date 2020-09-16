-ifndef(how_are_you_included__).
-define(how_are_you_included__, true).

%% struct 'metric'
-record(metric, {
    type    :: metric_type(),
    key     :: metric_key(),
    value   :: metric_value()
}).

-endif.
