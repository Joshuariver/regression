# All you need to know on PCA …
# https://www.r-bloggers.com/all-you-need-to-know-on-pca/


rm(list=ls())
setwd("~/R/Factor Analysis")

install.packages("Factoshiny")
library(Factoshiny)
data(decathlon)
result <- Factoshiny(decathlon)
