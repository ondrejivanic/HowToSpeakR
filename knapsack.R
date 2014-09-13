# knapsack(items, 3) -> 7
# knapsack(items, 4) -> 9
# knapsack(items, 5) -> 9
# knapsack(items.big, 100) -> 55
# knapsack(items.bigdata, 100) -> 273

items <- data.frame(
  v = c(3, 4, 5),
  w = c(2, 1, 3)
)
items.big <- data.frame(
  v = c(40, 35, 18, 4, 10, 2),
  w = c(100, 50, 45, 20, 10, 5)
)
items.bigdata <- data.frame(
  v = c(50, 93, 89, 39, 84, 52, 41, 51, 65, 88, 88, 89, 3, 92, 79, 38, 5, 95, 49),
  w = c(92, 79, 38, 5, 95, 49, 94, 19, 99, 6, 73, 63, 50, 93, 89, 39, 84, 52, 41)
)

## @knitr knapsack1
knapsack <- function(items, w) {
  max <- c(0, w)
  bag <- function(x) c(sum(items$v[x]), sum(items$w[x]))
  # generate all combinations
  for(i in 1:nrow(items)) {
    res <- t(combn(nrow(items), i, bag))
    # remove comb. which exceeds max weight
    res <- res[res[, 2] <= w, , drop = FALSE]
    if(nrow(res) >= 1 & max[1] < res[1]) {
      # best item
      max <- res[order(res[, 1], decreasing = TRUE)[1], ]
    }
  }
  max[1]
}

## @knitr knapsack2
knapsack.dp <- function(items, w) {
  m_new <- m <- rep.int(0, w + 1)

  for(i in 1:nrow(items)) {
    for(j in 0:w) {
      if(items$w[i] <= j) {
        m_new[j + 1] <- max(m[j + 1], m[j + 1 - items$w[i]] + items$v[i])
      } else {
        m_new[j + 1] <- m[j + 1]
      }
    }
    m <- m_new
  }
  m[w + 1]
}

## @knitr knapsack3
knapsack.dp2 <- function(items, w) {
  m <- rep.int(0, w + 1)

  for(i in 1:nrow(items)) {
    m <- ifelse(
      items$w[i] <= 0:w, # cond
      pmax(m, m[pmax(1, 0:w + 1 - items$w[i])] + items$v[i]), # true
      m # false
    )
  }
  m[w + 1]
}

## @knitr knapsack4
knapsack.mem <- function(items, w) {
  solve <- memoise(function(i, j) {
    if(min(i, j) <= 0) 0
    else if(items$w[i] > j) solve(i - 1, j)
    else max(solve(i - 1, j), items$v[i] + solve(i - 1, j - items$w[i]))
  })

  solve(nrow(items), w)
}

## @knitr knapsack4.rec
knapsack.rec <- function(items, w) {
  solve <- function(i, j) {
    if(min(i, j) <= 0) 0
    else if(items$w[i] > j) solve(i - 1, j)
    else max(solve(i - 1, j), items$v[i] + solve(i - 1, j - items$w[i]))
  }

  solve(nrow(items), w)
}
