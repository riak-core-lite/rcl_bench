# Riak Core Lite Bench

Simplification of `basho_bench` to be used by Riak Core Lite applications

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
2. Set `name` and `cookie` in `config/vm.args`
3. Uncomment `driver_module` configuration in `sys.config` and rename
   `my_driver` to your driver module
4. `make run` should start a dummy benchmark where the operations are printed to
   console
5. Implement the dummy driver operations or add news ones
6. Configure driver parameter 
