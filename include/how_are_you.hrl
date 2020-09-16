-ifndef(how_are_you_included__).
-define(how_are_you_included__, true).

%% struct 'metric'
-record(metric, {
    type    :: how_are_you:metric_type(),
    key     :: how_are_you:metric_key(),
    value   :: how_are_you:metric_value()
}).

-endif.
