# Random walk

## @knitr walk1
walk1 <- function(n) {
  x <- 0
  for(i in 2:n) {
    x[i] <- x[i - 1] + sample(c(-1, 1), 1)
  }
  x
}

## @knitr walk2
walk2 <- function(n) {
  x <- numeric(n)
  for(i in 2:n) {
    x[i] <- x[i - 1] + sample(c(-1, 1), 1)
  }
  x
}

## @knitr walk3
walk3 <- function(n) {
  x <- sample(c(-1, 1), n, replace = TRUE)
  for(i in 2:n) {
    x[i] <- x[i - 1] + x[i]
  }
  x
}

## @knitr walk4
steps <- function(n) sample(c(-1, 1), n, replace = TRUE)

walk4 <- function(n) cumsum(steps(n))

## @knitr walk5
walk5 <- function(n) Reduce(`+`, steps(n), accumulate = TRUE)
