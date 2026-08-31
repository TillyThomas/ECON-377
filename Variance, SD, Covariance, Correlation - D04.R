## ECN 377 - Day 4  |  variance, sd, covariance, correlation.  

## --- Sample variance & sd ---
## put a few numbers in a vector called x
x <- c(3, 3, 7)
## find its mean
xbar <- mean(x)
## by hand: subtract the mean from each value, square, add up, divide by n - 1
sum((x - xbar)^2) / (length(x)-1)
## check it with the built-in variance function
var <- var(x)
## the sd is the square root of the variance
sqrt(var)

## --- Sample covariance ---
## put two paired variables (same length) in vectors x and y
a <- c(6, 3, 5)
b <- c(4, 2, 1)
## sample means 
abar <- mean(a)
bbar <- mean(b)
## ________
## by hand: subtract each mean, multiply the pairs, add up, divide by n - 1
sum((a-abar)*(b-bbar)) / (length(a)-1)

## check it with the built-in covariance function
cov(a, b)
## __________
## --- Sample correlation ---
## the correlation is the covariance divided by (sd of x  times  sd of y)
cov(a, b) / (sd(a) * sd(b))
## check it with the built-in correlation function
cor(a, b)

## --- Nonlinear: log & exp (allow non-constant, curved relationships) ---
## make a range of positive x-values (call it x)
x <- c(1, 2, 3, 4)
## plot log(x): it flattens out (diminishing).  then plot exp(x): it grows fast.
log(x)
exp(x)
##______
## check that exp undoes log, and log undoes exp (pick any positive number):
##______
##______

## remember: var, sd, cov, cor are all SAMPLE quantities -- our best guesses at the population's.