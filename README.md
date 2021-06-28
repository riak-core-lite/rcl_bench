# Riak Core Lite Bench

Simplification of `basho_bench` that can be used to benchmark total throughput and
latencies of operations of any custom-defined workload.


## Generate Graphs

```
make results
```

Produces a summary graph in `current/summary.png`.


## Get Started

1. Copy `examples/print_ops_driver.erl` to `src/`. Rename if wanted.
2. Set `name` and `cookie` in `config/vm.args`, if you want to connect to an Erlang node directly.
3. Uncomment `driver_module` configuration in `sys.config` and rename `my_driver` to your driver module.
4. If `print_ops_driver` is used, `make run` should start a dummy benchmark.
5. Implement the new driver operations.
6. Erlang dependencies can be added to `rebar.config` and to `rcl_bench.app.src`, e.g. a client to connect to your application.
7. Configure driver parameters and benchmark your application.


## Sample Implementations

1. [Antidote Benchmarks](https://github.com/albsch/antidote_bench)
2. [RclRef Benchmarks](https://github.com/wattlebirdaz/rcl_bench)
