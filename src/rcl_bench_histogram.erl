-module(rcl_bench_histogram).

-export([new_histogram/2, notify/2, get_histogram_statistics/1]).

-define(DEFAULT_METRICS, [
    arithmetic_mean,
    geometric_mean,
    harmonic_mean,
    histogram,
    kurtosis,
    n,
    max,
    median,
    min,
    {percentile, [50, 75, 95, 99, 999]},
    skewness,
    standard_deviation,
    variance
   ]).

-record(metric, { tags = sets:new(), history_size }).
-record(histogram, { sample }).
 
new_histogram(Name, SampleSize) ->
    Sample = rcl_bench_sample_slide:new(SampleSize),
    Hist = #histogram{sample = Sample},
    ets:insert(rcl_bench_histogram, {Name, Hist}),
    true = ets:insert(rcl_bench, {Name, #metric{}}),
    ok.

notify(Name, Event) ->
    update(Name, Event).

get_value(Name) ->
    [{_, Value}] = ets:lookup(rcl_bench_histogram, Name),
    Value.

get_values(Name) ->
    Hist = get_value(Name),
    rcl_bench_sample_slide:get_values(Hist#histogram.sample).


update(Name, Value) ->
    Hist = get_value(Name),
    Sample = Hist#histogram.sample,
    case rcl_bench_sample_slide:update(Hist#histogram.sample, Value) of
        Sample ->
            %% sample didn't change, don't need to write it back
            true;
        NewSample ->
            ets:insert(rcl_bench_histogram, {Name, Hist#histogram{sample = NewSample}})
    end.


get_histogram_statistics(Name) -> 
    Values = get_values(Name),
    WantedMetrics = application:get_env(rcl_bench, enabled_metrics, ?DEFAULT_METRICS),
    bear:get_statistics_subset(Values, WantedMetrics).
