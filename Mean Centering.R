# 6 ways to mean centering in regression
# https://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/

rm(list=ls())
setwd("~/R/Regression/exercise")


# small dataset
set.seed(212)
Data = matrix(rnorm(15), 5, 3)

Data

# Nr. 1 : centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
center_scale(Data)


# Nr. 2 : center with 'apply()'
center_apply <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

# apply it
center_apply(Data)


# Nr 3 : center with 'sweep()'
center_sweep <- function(x, row.w = rep(1, nrow(x))/nrow(x)) {
  get_average <- function(v) sum(v * row.w)/sum(row.w)
  average <- apply(x, 2, get_average)
  sweep(x, 2, average)
}

# apply it
center_sweep(Data)



# Nr. 4 : center with 'colMeans()' <- This is the best option
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

# apply it
center_colmeans(Data)


# Nr 5 : center matrix operator
center_operator <- function(x) {
  n = nrow(x)
  ones = rep(1, n)
  H = diag(n) - (1/n) * (ones %*% t(ones))
  H %*% x
}

# apply it
center_operator(Data)


# Nr 6 : mean subtraction
center_mean <- function(x) {
  ones = rep(1, nrow(x))
  x_mean = ones %*% t(colMeans(x))
  x - x_mean
}

# apply it
center_mean(Data)


# To decide what to use
# fake data
X = matrix(runif(2000), 100, 20)

# test them
system.time(replicate(500, center_scale(X)))
system.time(replicate(500, center_apply(X)))
system.time(replicate(500, center_sweep(X)))
system.time(replicate(500, center_colmeans(X)))
system.time(replicate(500, center_operator(X)))
system.time(replicate(500, center_mean(X)))


# So the winner is center_colmeans() or center_mean()
