rcl_bench
=====

Simplification of `basho_bench`


1. Adjust Configuration
-----

    config/sys.config
    config/vm.args

Optional:
  
   * Implement more operations in `rcl_bench_driver.erl`


2. Run Benchmark
-----

    make run


3. Generate Graphs
-----

Change into the `R` directory. Change the operation you want to make a graph of in the file. Execute the R script.
