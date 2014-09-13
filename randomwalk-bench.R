## @knitr walkbench
require(rbenchmark)
require(ggplot2)

source("randomwalk.R")

benchmark(
  walk1(1000),
  walk2(1000),
  walk3(1000), 
  walk4(1000), 
  columns = c("test", "elapsed", "relative"), order = "relative"
)

exp <- c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
bench.result <- do.call("rbind", lapply(exp, function(x) {  
  #print(x)

  b <- benchmark(
    "imperative" = walk1(x),
    "pre allocation" = walk2(x),
    "pre-sample()-ing" = walk3(x),
    "cumsum()" = walk4(x),
    "Reduce()" = walk5(x),
    columns = c("test", "elapsed"),
    replications = 100
  )
  
  b$size <- x
  
  b
}))

o <- aggregate(elapsed ~ test, bench.result, max)
bench.result$test <- factor(
  bench.result$test,
  as.character(o[order(-o$elapsed), "test"])
)
