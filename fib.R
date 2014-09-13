## @knitr fib
fib <- function(n) {
  if(n < 3) 1
  else fib(n - 1) + fib(n - 2)
}

## @knitr fib.ugly
fib.ugly <- function(xs) {
  cache <- new.env(TRUE, emptyenv())
  
  fib.n <- function(n) {
    key <- as.character(n)
    if(!exists(key, envir = cache, inherits = FALSE)) {
      val <- if(n < 3) 1 else fib.n(n - 1) + fib.n(n - 2)
      assign(key, val, envir = cache)
    }   
    get(key, envir = cache, inherits = FALSE)
  }
  
  for(i in 1:length(xs)) xs[i] <- fib.n(xs[i])
  xs  
}

## @knitr fib.m
fib.m <- memoise(function(n) {
  if(n < 3) 1
  else fib.m(n - 1) + fib.m(n - 2)
})

## @knitr fib.vec
fib.vec <- Vectorize(function (x) {
  fib.m(x)
})

## @knitr fib.direct
fib.direct <- function(xs) {
  alpha <- (1 + sqrt(5))/2
  round(alpha ^ xs / sqrt(5))
}
