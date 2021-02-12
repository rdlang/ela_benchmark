# ela_benchmark
Single-objective boundary-constrained optimization benchmark suite based on the results from "An Exploratory Landscape Analysis Based Benchmark Suite" by Lang and Engelbrecht

# Usage

To use these functions, you need to install the [smoof](https://github.com/jakobbossek/smoof) and [zoo](https://cran.r-project.org/web/packages/zoo/index.html) R packages.

To use the functions, you need to load all the R function defintions into the global environment. Then, you can use the benchmark suite as follows:

```R
fn = benchmark_suite(dimensions = 2, fid = 1)
x = c(0, 0)
result = fn(x)
```

