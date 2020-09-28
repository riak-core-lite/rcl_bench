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
