## linear.R
##
## by Leah R Johnson, Stats, VT


## load the three.ex function
source("sampling.R")

## set inputs (covariates)
x <- seq(-1,2,length=30)

## set regression coefficients
alpha <- -3; beta <- 2

## monte-carlo linear fits
ex.mc(x, alpha, beta)
