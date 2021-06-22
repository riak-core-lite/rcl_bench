# Riak Core Lite Bench

Simplification of `basho_bench` that can be used to benchmark any application.

# Generate Graphs

```
# ./R/latency.R <op> <csv-path> <image-path>
./R/latency.R put tests/put_single.csv latency_put_single.png

# ./R/throughput.R <op> <csv-path> <image-path>
./R/throughput.R put tests/put_single.csv throughput_put_single.png
```

To install R and ggplot2 on ubuntu:

```
sudo apt install r-base

R

> install.packages("ggplot2")
> install.packages("dplyr")
> install.packages("scales")
> install.packages("lubridate")
```



## Get Started

1. Copy `examples/print_ops_driver.erl` to `src/`. Rename if wanted.
2. Set `name` and `cookie` in `config/vm.args`, if you want to connect to an Erlang node directly
3. Uncomment `driver_module` configuration in `sys.config` and rename
   `my_driver` to your driver module
4. If `print_ops_driver` is used, `make run` should start a dummy benchmark where the operations are printed to
   console
5. Implement the dummy driver operations or add news ones
6. Erlang dependencies can be added to `rebar.config` and to `rcl_bench.app.src`, e.g. a client to connect to your application
7. Configure driver parameters and benchmark your application

## Sample Implementations

1. [Antidote Benchmarks](https://github.com/albsch/antidote_bench)
2. [RclRef Benchmarks](https://github.com/wattlebirdaz/rcl_bench)
