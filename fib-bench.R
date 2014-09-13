## @knitr fibbench
require(rbenchmark)
require(ggplot2)

source("fib.R")

exp <- seq.int(1, 1000, 10) #, 50, 100, 200, 500, 1000)
remove <- vector("character")
forget(fib.m)
options(expressions=50000)
bench.result <- do.call("rbind", lapply(exp, function(x) {

  args <- list(
    columns = c("test", "elapsed"),
    replications = 100
  )

  if(!("Vectorized" %in% remove)) args[["Vectorized"]] <- expression(fib.vec(x))
  if(!("memoised" %in% remove)) args[["memoised"]] <- expression(fib.m(x))
  if(!("ugly" %in% remove)) args[["ugly"]] <- expression(fib.ugly(x))
  if(!("direct" %in% remove)) args[["direct"]] <- expression(fib.direct(x))
  
  b <- do.call("benchmark", args)
  b$size <- x

  # remove test which takes more than Xsec
  remove <<- unique(c(as.character(b[with(b, elapsed > 1), ]$test), remove))
  #print(paste("N = ", x, ";", paste(remove, collapse=","), ";", max(b$elapsed)))
  
  b
}))

o <- aggregate(elapsed ~ test, bench.result, max)
bench.result$test <- factor(
  bench.result$test,
  as.character(o[order(-o$elapsed), "test"])
)
