# Applying gradient descent – primer / refresher

# Every so often a problem arises where it’s appropriate to use gradient descent, and it’s fun 
# (and / or easier) to apply it manually. Recently I’ve applied it optimising a basic recommender 
# system to ‘unsuppressing’ suppressed tabular data. I thought I’d do a series of posts about how 
# I’ve used gradient descent, but figured it was worth while starting with the basics as a primer / refresher.

rm(list=ls())
setwd("~/R/Regression/exercise")

library(ggplot2)
set.seed(241)

nobs <- 250
b0 <- 4
b1 <- 2

# simulate data
x <- rnorm(nobs)
y <- b0 + b1*x + rnorm(nobs, 0, 0.5)
df <- data.frame(x, y)

# plot data
g1 <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 2) +
  theme_minimal()

# set model matrix
X <- model.matrix(y ~ x, data = df)
beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta

##                 [,1]
## (Intercept) 4.009283
## x           2.016444

# And just to convince ourselves this is correct

# linear model formulation
lm1 <- lm(y ~ x, data = df)
coef(lm1)

## (Intercept)           x 
##    4.009283    2.016444

g1 + geom_abline(slope = coef(lm1)[2], intercept = coef(lm1)[1], col = "darkmagenta", size = 1)


# Gradient descent

# The objective is to achieve the same result using gradient descent. It works by updating 
# the parameters with each iteration in the direction of negative gradient to minimise 
# the mean squared error

# gradient descent function
gradientDescent <- function(formula, data, par.init, loss.fun, lr, iters){
  formula <- as.formula(formula)
  X <- model.matrix(formula, data = data)
  y <- data[,all.vars(formula)[1]]
  par <- loss <- matrix(NA, nrow = iters+1, ncol = 2)
  par[1,] <- par.init
  for(k in 1:iters){
    loss[k,] <- loss.fun(X=X, y=y, par=par[k,])
    par[k+1,] <- par[k,] - lr*loss[k,]
  }
  return(list(par = par))
}

# loss function
loss.fun <- function(X, y, par) return(-2/nrow(X)*(t(X) %*% (y - X %*% par)))

# gradient descent. not much to it really
beta <- gradientDescent(y ~ x, data = df, par.init = c(1, 1), loss.fun = loss.fun, lr = 0.01, iters = 1000)$par

# plotting results
z <- seq(1, 1001, 10)
g1 + geom_abline(slope = beta[z,2], intercept = beta[z,1], col = "darkmagenta", alpha = 0.2, size = 1)

tail(beta, 1)

beta <- gradientDescent(y ~ x, data = df, par.init = c(6, -1), loss.fun = loss.fun, lr = 0.01, iters = 1000)$par

# plotting results
z <- seq(1, 1001, 10)
beta.df <- data.frame(b0 = beta[z,1], b1 = beta[z,2])
g1 + geom_abline(data = beta.df, mapping = aes(slope = b1, intercept = b0), col = "darkmagenta", alpha = 0.2, size = 1)

tail(beta, 1)

##             [,1]     [,2]
## [1001,] 4.009283 2.016444

ggif_minimal <- df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(size = 2) +
  theme_minimal() +
  geom_abline(data = beta.df, mapping = aes(slope = b1, intercept = b0), col = "darkmagenta", size = 1) +
  geom_text(
    data = data.frame(z, b0 = beta[z,1], b1 = beta[z,2]), 
    mapping = aes(
      x = -2.8, y = 9, 
      label = paste("b0 = ", round(b0, 2), "\nb1 = ", round(b1, 2))),
    hjust = 0,
    size = 6
  ) +
  transition_reveal(z) +
  ease_aes("linear") +
  enter_appear() +
  exit_fade()


animate(ggif_minimal, width = 1920, height = 1080, fps = 80)
