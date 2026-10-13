# MASTER SCRIPT 

library(wooldridge)


## ================= 1. SAMPLE STATISTICS (data you are given) =================
x <- c(2, 5, 9, 4)                      # type the data as a vector
y <- c(3, 7, 8, 6)
length(x)                               # sample size n
sum(x)                                  # sum of the x's
mean(x)                                 # sample mean  (x-bar)
var(x)                                  # sample variance  (divides by n - 1)
sd(x)                                   # sample standard deviation = sqrt(var(x))
cov(x, y)                               # sample covariance
cor(x, y)                               # sample correlation (between -1 and 1, no units)

## Correlation from pieces:  cor = cov / (sd_x * sd_y)
sxy <- 6
sx <- 3                                 # careful: is this a standard deviation ...
vy <- 16                                # ... or a variance? sd = sqrt(variance)
sxy / (sx * sqrt(vy))

## A vector of n numbers: the first is a, the other n - 1 all equal b
n <- 12
a <- 30
b <- 6
(a + (n - 1) * b) / n                   # sample mean


## ================= 2. LINES, CHANGES, PERCENTS =================
## y = b0 + b1*x1 + b2*x2:  change in y = b1*(change in x1) + b2*(change in x2)
## (the intercept never matters for a CHANGE, and a FALL is a negative change)
b1 <- 2.5
b2 <- -1.2
b1 * 3 + b2 * (-5)                      # x1 rises by 3, x2 falls by 5

## Percent change vs percentage-point change
old <- 4.2
new <- 5.1
100 * (new - old) / old                 # percent change  (21.43)
new - old                               # percentage-point change  (0.9)


## ================= 3. A RANDOM VARIABLE FROM A PROBABILITY TABLE =================
xv <- c(0, 2, 5)                        # values X can take
p <- c(0.5, 0.3, 0.2)                   # their probabilities (must sum to 1)
EX <- sum(p * xv)                       # E[X]
EX2 <- sum(p * xv^2)                    # E[X^2]
VarX <- EX2 - EX^2                      # Var(X) = E[X^2] - (E[X])^2
sqrt(VarX)                              # sd(X)

## Conditional versions: same recipe, using the probabilities GIVEN X = x
yv <- c(10, 20, 40)
pc <- c(0.5, 0.3, 0.2)
EYx <- sum(pc * yv)                     # E[Y | X = x]
sum(pc * yv^2) - EYx^2                  # Var(Y | X = x)


## ================= 4. RULES FOR E, Var, Cov (no data needed) =================
EX <- 2
EX2 <- 13
a <- 3
b <- -4
a * EX + b                              # E[aX + b] = a E[X] + b          (the MEAN)
a^2 * (EX2 - EX^2)                      # Var(aX + b) = a^2 Var(X)         (b drops out)
abs(a) * sqrt(EX2 - EX^2)               # sd(aX + b) = |a| sd(X)           (not the mean!)

VarX <- 4
VarY <- 9
CovXY <- -1.5
a <- 2
c2 <- -1                                # coefficient on Y: "2X - Y" means c2 = -1
a^2 * VarX + c2^2 * VarY + 2 * a * c2 * CovXY   # Var(aX + c2*Y)  (watch the signs)


## ================= 5. TWO RANDOM VARIABLES FROM A JOINT TABLE =================
## Population formulas WEIGHT BY THE PROBABILITIES. cor(xv, yv) would ignore them: wrong here.
xv <- c(1, 2, 3, 6)
yv <- c(3, 4, 8, 9)
p <- c(0.1, 0.4, 0.3, 0.2)
EX <- sum(p * xv)
EY <- sum(p * yv)
CovXY <- sum(p * xv * yv) - EX * EY     # Cov = E[XY] - E[X]E[Y]
VarX <- sum(p * xv^2) - EX^2
VarY <- sum(p * yv^2) - EY^2
CovXY / sqrt(VarX * VarY)               # Cor(X, Y)
CovXY / VarX                            # population regression slope beta1


## ================= 6. OLS BY HAND (a small sample) =================
x <- c(1, 3, 4, 6, 8)
y <- c(2, 5, 4, 9, 10)
b1 <- cov(x, y) / var(x)                # slope = cov(x, y) / var(x)
b0 <- mean(y) - b1 * mean(x)            # intercept = ybar - b1 * xbar
reg <- lm(y ~ x)                        # same numbers from lm()
reg$coefficients

## From summary numbers instead of data
r <- 0.6
sx <- 2
sy <- 5
r * sy / sx                             # slope = cor * sd_y / sd_x  (= cov / var_x)


## ================= 7. PREDICTION, CHANGE, RESIDUALS  (uses x, y, reg from section 6) =================
b0 <- reg$coefficients[[1]]             # save the estimates as plain numbers (do not round them yet)
b1 <- reg$coefficients[[2]]             # [[ ]] drops the label, so results do not print as "(Intercept)"
b0 + b1 * 6                             # predicted y at x = 6  (fitted value)
b1 * 4                                  # predicted CHANGE in y when x rises by 4
reg$fitted.values[3]                    # fitted value of observation 3
reg$residuals[3]                        # residual of observation 3 = actual - fitted
y[3] - (b0 + b1 * x[3])                 # the same residual, by hand


## ================= 8. OLS PROPERTIES (answers you can give without computing) =================
sum(reg$residuals)                      # always 0  (R may print something like -8.7e-15: that IS 0)
sum(x * reg$residuals)                  # always 0
b0 + b1 * mean(x)                       # (xbar, ybar) is on the OLS line: this equals mean(y)
-sum(c(1.2, -0.5, 2.1, -1.4))           # missing residual = minus the sum of the others
## OLS = the line with the SMALLEST sum of squared residuals (least squares)


## ================= 9. SST, SSE, SSR, R^2  (uses y, reg from section 6) =================
SST <- sum((y - mean(y))^2)             # total variation
SSR <- sum(reg$residuals^2)             # unexplained
SSE <- SST - SSR                        # explained
SSE / SST                               # R^2 = 1 - SSR / SST
summary(reg)$r.squared                  # check
100 * summary(reg)$r.squared            # "what percentage is explained?" -> a percent

## From summary numbers instead of data
n <- 25
s2y <- 16
(n - 1) * s2y                           # SST = (n - 1) * sample variance of y
SSE0 <- 90
R20 <- 0.3
SSE0 / R20 - SSE0                       # SSR, from SSE and R^2  (SST = SSE / R^2)


## ================= 10. REAL DATA (wooldridge) =================
data("wage1")
nrow(wage1)                             # sample size
mean(wage1$wage)                        # a column with $
reg1 <- lm(wage ~ tenure, data = wage1) # y ~ x, data = the dataset
reg1$coefficients
reg1$residuals[2]                       # residual of the worker in row 2
mean(wage1$wage[wage1$educ == 12])      # average wage among workers with educ = 12


## ================= 11. UNITS OF MEASUREMENT =================
data("ceosal1")
reg2 <- lm(salary ~ roe, data = ceosal1)               # salary in $1000s
reg2$coefficients                                      # before: 963.19 and 18.50
ceosal1$salarydol <- 1000 * ceosal1$salary             # y times c:  b0 and b1 times c
lm(salarydol ~ roe, data = ceosal1)$coefficients
ceosal1$roedec <- ceosal1$roe / 100                    # x times c:  b1 divided by c, b0 same
lm(salary ~ roedec, data = ceosal1)$coefficients
## R^2 does not change when units change


## ================= 12. LOGS (Table 2.3) =================
reg3 <- lm(log(wage) ~ educ, data = wage1)             # log-level
100 * reg3$coefficients[[2]]            # % change in wage from +1 year of educ
reg4 <- lm(log(salary) ~ log(sales), data = ceosal1)   # log-log
reg4$coefficients[[2]]                  # elasticity: % change in salary from +1% sales
reg5 <- lm(salary ~ log(sales), data = ceosal1)        # level-log
reg5$coefficients[[2]] / 100            # change in salary ($1000s) from +1% sales
##   level-level:  change in y   = b1 * (change in x)
##   log-level:    % change in y = 100 * b1 * (change in x)
##   level-log:    change in y   = (b1 / 100) * (% change in x)
##   log-log:      % change in y = b1 * (% change in x)


## ================= MY RECIPES (add your own below) =================
