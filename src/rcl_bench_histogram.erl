-module(rcl_bench_histogram).

-export([new_histogram/4]).

-ifdef(use_rand).
-define(SEED, rand:seed(exsplus)).
-else.
-define(SEED, os:timestamp()).
-endif.

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
% =========
% ets:insert(histogram_table, {Name, Hist}).
% >>
% true = ets:insert(folsom_table, {Name, #metric{type = histogram}}),
 
% default
% slide, duration, 0.015
new_histogram(Name, slide, SampleSize, DefaultAlpha) ->
    Sample = rcl_bench_sample_slide:new(SampleSize, DefaultAlpha),
    Hist = #histogram{type = slide, sample = Sample},

    %%TODO CONTINUE HERE
    %% CHECK WHERE HISITOGRAM TABLE IS CREATED *NEW)
    ets:insert(histogram_table, {Name, Hist}),
    ok.