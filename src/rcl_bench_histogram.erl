-module(rcl_bench_histogram).

-export([new_histogram/3, notify/2, get_histogram_statistics/1]).

-ifdef(use_rand).
-define(SEED, rand:seed(exsplus)).
-else.
-define(SEED, os:timestamp()).
-endif.

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

-record(metric, { tags = sets:new(), type, history_size }).
-record(uniform, {
    size = 128,
    n = 1,
    reservoir = ets:new(rcl_bench_uniform,[set, {write_concurrency, true}, public]),
    seed = ?SEED
   }).
-record(histogram, { type = uniform, sample = #uniform{} }).


% folsom_ets:add_handler(histogram, Name, SampleType, SampleSize, Alpha).
% true = folsom_metrics_histogram:new(Name, SampleType, SampleSize, Alpha),
% Sample = folsom_sample:new(SampleType, SampleSize, Alpha),
% Hist = #histogram{type = SampleType, sample = Sample},
% ets:insert(histogram_table, {Name, Hist}).
% true = ets:insert(folsom_table, {Name, #metric{type = histogram}}),
% =========
% >>
 
% default
% slide, duration, 0.015
new_histogram(Name, slide, SampleSize) ->
    logger:notice("Creating histogram"),

    %% folsom_histogram
    Sample = rcl_bench_sample_slide:new(SampleSize),
    Hist = #histogram{type = slide, sample = Sample},

    %% TODO histogram_table is nowhere created
    ets:insert(rcl_bench_histogram, {Name, Hist}),

    %% folsom_ets
    true = ets:insert(rcl_bench, {Name, #metric{type = histogram}}),
    logger:notice("Finished creating histogram"),
    ok.

notify(Name,Event) ->
    {_, Info} = get_info(Name),
    Type = proplists:get_value(type, Info),
    notifyH(Name, Event, Type)
.

notifyH(Name, Value, histogram) ->
    update(Name, Value).

get_info(Name) ->
    [{_, #metric{type = Type, tags = Tags}}] = ets:lookup(rcl_bench, Name),
    {Name, [{type, Type}, {tags, Tags}]}.


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
