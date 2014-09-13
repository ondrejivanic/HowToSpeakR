require(rbenchmark)
require(ggplot2)
require(memoise)

source("knapsack.R")

exp <- 1:750 # should be fine for 1sec
data <- lapply(exp, function(x) {
  data.frame(
    v = round(runif(x, 10, 100)),
    w = round(runif(x, 1, 25))
  )
})

options(expressions=50000)
remove <- vector("character")
bench.result <- do.call("rbind", lapply(data, function(x) {
  max.w <- round(sum(x$w * 0.3))
  n <- nrow(x)

  args <- list(
    columns = c("test", "elapsed"),
    replications = 1
  )

  forget(knapsack.mem)

  if(!("imperative (DP)" %in% remove)) args[["imperative (DP)"]] <- expression(knapsack.dp(x, max.w))
  if(!("vectorised (DP)" %in% remove)) args[["vectorised (DP)"]] <- expression(knapsack.dp2(x, max.w))
  if(!("brute force" %in% remove)) args[["brute force"]] <- expression(knapsack(x, max.w))
  if(!("recursion" %in% remove)) args[["recursion"]] <- expression(knapsack.rec(x, max.w))
  if(!("memoisation" %in% remove)) args[["memoisation"]] <- expression(knapsack.mem(x, max.w))

  b <- do.call("benchmark", args)
  b$size <- n

  # remove test which takes more than Xsec
  remove <<- unique(c(as.character(b[with(b, elapsed > 1), ]$test), remove))
  #print(paste("N = ", n, ";", paste(remove, collapse=","), ";", max(b$elapsed)))

  b
}))

o <- aggregate(elapsed ~ test, bench.result, max)
bench.result$test <- factor(
  bench.result$test,
  as.character(o[order(-o$elapsed), "test"])
)
