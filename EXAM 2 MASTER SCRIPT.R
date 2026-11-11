# MASTER SCRIPT - EXAM II
## HOW TO USE: every section has example numbers. Overwrite them with the exam's numbers and run.
## Keep unrounded values in R. Round to hundredths ONLY at the end:  round(answer, 2)
## K = number of SLOPE variables (no intercept).   df = n - K - 1  (simple regression: n - 2)
## Do NOT type rounded coefficients when the question says "use the unrounded coefficients".

options(scipen = 999)                   # stops R printing 1.547e-02; prints 0.01547 instead
library(wooldridge)
data("wage1"); data("gpa1"); data("ceosal1"); data("meap93"); data("bwght"); data("vote1"); data("hprice1")

# clear saved variables 
rm(list = ls())

## ======================================================================================================
## ================= A. FORMULA SHEET: EVERY KEY CALCULATION, NO DATA SET NEEDED =================
## ======================================================================================================
## Each block: the formula in a comment, then INPUT lines (overwrite those numbers), then the calculation lines.
## The number in (parentheses) is the answer for the example inputs, so you can check that the block works.
## K = number of SLOPE variables (no intercept).  Keep unrounded values; round to hundredths only at the end.

## ---- A1. SAMPLE MEAN, SST, VARIANCE, SD of a list of numbers (sample formulas divide by n - 1) ----
x <- c(3, 3, 7)                         # INPUT: your data
n <- length(x)                          # sample size                                    (3)
xbar <- sum(x) / n                      # sample mean                                    (4.33)
SST_x <- sum((x - xbar)^2)              # SST = sum of squared deviations from the mean  (10.67)
S2x <- SST_x / (n - 1)                  # sample variance                                (5.33)
Sx <- sqrt(S2x)                         # sample standard deviation                      (2.31)
## shortcuts:  mean(x)   var(x)   sd(x)   sum((x - mean(x))^2)

## ---- A2. SST_j FROM SUMMARY NUMBERS:  SST = (n - 1) * (sample VARIANCE) ----
## Works for SST_X (simple regression), SST_j (for X_j) and SST (for Y).
n <- 20                                 # INPUT: sample size
Sx <- 1.3                               # INPUT: sample standard deviation. If you are GIVEN THE VARIANCE, skip the next line.
S2x <- Sx^2                             # square the sd to get the variance              (1.69)
(n - 1) * S2x                           # SST                                            (32.11)

## ---- A3. SST, SSR, SSE, R^2 FROM y AND THE FITTED VALUES ----
y <- c(2, 2, 5)                         # INPUT: actual y values
yhat <- c(1.5, 3, 4.5)                  # INPUT: fitted values (b0 + b1*x ...)
uhat <- y - yhat                        # residuals = ACTUAL - FITTED                    (0.5, -1, 0.5)
SST <- sum((y - mean(y))^2)             # total variation in y                           (6)
SSR <- sum(uhat^2)                      # unexplained: sum of squared residuals          (1.5)
SSE <- SST - SSR                        # explained                                      (4.5)
SSE / SST                               # R^2 = SSE / SST = 1 - SSR / SST                (0.75)
100 * SSE / SST                         # "what percent is explained?" -> a percent      (75)

## ---- A4. SST, SSR, SSE, R^2 FROM SUMMARY NUMBERS (you are given two of them) ----
SSE <- 128                              # INPUT
R2 <- 0.33                              # INPUT
SST <- SSE / R2                         # SST = SSE / R^2                                (387.88)
SSR <- SST - SSE                        # SSR = SST - SSE                                (259.88)
## other directions:  SSE = R2 * SST    SSR = (1 - R2) * SST    R2 = 1 - SSR / SST    SST = SSE + SSR

## ---- A5. DEGREES OF FREEDOM, sigma-hat^2, sigma-hat (the "standard error of the regression") ----
n <- 32                                 # INPUT: number of observations
K <- 4                                  # INPUT: number of independent variables (simple regression: K = 1, so df = n - 2)
SSR <- 2921                             # INPUT
dof <- n - K - 1                        # degrees of freedom                             (27)
sigma2_hat <- SSR / dof                 # sigma-hat^2 = estimate of the error variance   (108.19)
sigma_hat <- sqrt(sigma2_hat)           # sigma-hat = sqrt(SSR / df)                     (10.40)
sigma_hat^2 * dof                       # SSR back from sigma-hat                        (2921)

## ---- A6. SIMPLE REGRESSION: Var(b1hat), sd(b1hat), se(b1hat) ----
## Var(b1hat) = sigma^2 / SST_X      sd(b1hat) = sigma / sqrt(SST_X)      se(b1hat) = sigma-hat / sqrt(SST_X)
sigma <- 4                              # INPUT: the error sd (put sigma-hat here and the result is the SE)
n <- 26                                 # INPUT
S2x <- 4                                # INPUT: sample VARIANCE of X (if you have the sd, square it)
SST_X <- (n - 1) * S2x                  # SST_X                                          (100)
sigma^2 / SST_X                         # Var(b1hat)                                     (0.16)
sigma / sqrt(SST_X)                     # sd(b1hat)  (or se(b1hat))                      (0.40)
sigma * 2 / sqrt(SST_X)                 # what if sigma doubles?  sd doubles             (0.80)
sigma / sqrt(SST_X * 4)                 # sample (and SST_X) 4 times larger: sd / sqrt(4) (0.20)
## Var(b1hat) is BIGGER when sigma^2 is larger, X is bunched up (small SST_X), or the sample is smaller.

## ---- A7. R_j^2 AND THE VIF ----
## R_j^2 = R-squared from regressing X_j on the OTHER independent variables (Y is NOT in that regression).
## With data it is  summary(lm(xj ~ x2 + x3))$r.squared.   By formula, from that auxiliary regression's sums of squares:
SST_j <- 100                            # INPUT: SST of X_j  (= (n - 1) * var(xj))
SSR_j <- 30                             # INPUT: SSR from the regression of X_j on the other x's
R2_j <- 1 - SSR_j / SST_j               # R_j^2                                          (0.70)
1 / (1 - R2_j)                          # VIF: how much collinearity inflates the variance (3.33)

## ---- A8. MULTIPLE REGRESSION: Var(bj-hat) and se(bj-hat) ----
## se(bj-hat) = sigma-hat / sqrt( SST_j * (1 - R_j^2) )          Var(bj-hat) = sigma^2 / ( SST_j * (1 - R_j^2) )
sigma_hat <- 7.9                        # INPUT  (= sqrt(SSR / (n - K - 1)))
SST_j <- 357                            # INPUT
R2_j <- 0.27                            # INPUT
sigma_hat / sqrt(SST_j * (1 - R2_j))    # se(bj-hat): DIVIDE by the square root; always positive    (0.49)
sigma_hat^2 / (SST_j * (1 - R2_j))      # Var(bj-hat)                                    (0.24)
## Solving backwards when you are given the se:
se <- 0.49                              # INPUT
sigma_hat^2 / (se^2 * (1 - R2_j))       # SST_j                                          (about 357)
1 - sigma_hat^2 / (se^2 * SST_j)        # R_j^2                                          (about 0.27)
se * sqrt(SST_j * (1 - R2_j))           # sigma-hat                                      (about 7.9)
## Bigger se: larger sigma, smaller SST_j, larger R_j^2.  As R_j^2 -> 1 the se grows without bound.

## ---- A9. SLOPE AND INTERCEPT BY HAND ----
## b1 = cov(x, y) / var(x)        b0 = ybar - b1 * xbar
x <- c(1, 2, 3)                         # INPUT: data
y <- c(2, 2, 5)
b1 <- cov(x, y) / var(x)                # slope                                          (1.5)
b0 <- mean(y) - b1 * mean(x)            # intercept                                      (0)
## From summary numbers instead of data:   b1 = cor * sd_y / sd_x
r <- 0.6                                # INPUT: correlation
sy <- 5                                 # INPUT: sd of y
sx <- 2                                 # INPUT: sd of x
xbar <- 10                              # INPUT
ybar <- 20                              # INPUT
b1 <- r * sy / sx                       # slope                                          (1.5)
ybar - b1 * xbar                        # intercept                                      (5)

## ---- A10. PREDICTION AND CHANGE (any number of x's) ----
b0 <- 2                                 # INPUT: intercept
b <- c(0.5, -0.3)                       # INPUT: slopes b1, b2, ...   (a negative slope needs the minus sign)
x <- c(4, 10)                           # INPUT: values of x1, x2, ...
b0 + sum(b * x)                         # y-hat = b0 + b1*x1 + b2*x2 + ...               (1)
dx <- c(1, 2)                           # INPUT: change in each x (0 = held fixed, a FALL is negative)
sum(b * dx)                             # change in y-hat (the intercept never matters)  (-0.1)

## ---- A11. LOG-LEVEL PREDICTION AND % CHANGE (log(y) on several x's) ----
b0 <- 0.28                              # INPUT
b <- c(0.092, 0.0041, 0.022)            # INPUT: slopes
x <- c(16, 4, 2)                        # INPUT: values of the x's
exp(b0 + sum(b * x))                    # y-hat in original units: exp() of the predicted log(y)   (6.13 with these rounded numbers)
dx <- c(0, 2, 1)                        # INPUT: change in each x
100 * sum(b * dx)                       # approximate % change in y                      (3.02)
100 * (exp(sum(b * dx)) - 1)            # EXACT % change (use only if the question asks) (3.07)
## An x that enters as LOG(x): plug in log(value), e.g. b[2] * log(5000)

## ---- A12. THE FOUR FUNCTIONAL FORMS (b1 = the slope; a FALL is negative) ----
b1 <- 3; dx <- 2                        # INPUTS  level-level
b1 * dx                                 # change in y                                    (6)
b1 <- 0.08; dx <- 2                     # INPUTS  log-level
100 * b1 * dx                           # % change in y                                  (16)
b1 <- 6.4; pct_dx <- 2                  # INPUTS  level-log
(b1 / 100) * pct_dx                     # change in y, in y's units                      (0.128)
b1 <- 0.49; pct_dx <- 16                # INPUTS  log-log
b1 * pct_dx                             # % change in y (b1 is an elasticity)            (7.84)
logyhat <- 1.5                          # INPUT: a predicted log(y)
exp(logyhat)                            # back to y                                      (4.48)

## ---- A13. RESCALING THE UNITS ----
##  y times cy:  slope times cy, intercept times cy.    x times cx:  slope divided by cx, intercept SAME.   R^2 unchanged.
b0 <- 9                                 # INPUT: original intercept
b1 <- 3.9                               # INPUT: original slope
cy <- 10                                # INPUT: y multiplied by cy   (dollars -> cents 100; thousands -> dollars 1000)
cx <- 1/1000                            # INPUT: x multiplied by cx   (DIVIDED by k means cx = 1/k)
b1 * cy / cx                            # new slope                                      (39000)
b0 * cy                                 # new intercept                                  (90)
## Inside logs (the SLOPE is unchanged by changing units of a logged variable):
b0 <- 5.93; b1 <- 0.55                  # INPUT: log-log intercept and slope
cy <- 1000                              # INPUT: y multiplied by cy
b0 + log(cy)                            # new intercept when y is rescaled               (12.84)
cx <- 100                               # INPUT: x multiplied by cx
b0 - b1 * log(cx)                       # new intercept when x is rescaled               (3.40)
## log-level, x rescaled (months -> years means cx = 1/12):  new slope = b1 / cx
100 * 0.0069 / (1/12)                   # % change in wage per extra YEAR when slope is per MONTH  (8.28)

## ---- A14. OMITTED-VARIABLE BIAS (x2 omitted) ----
## b1tilde = b1hat + b2hat * delta1       bias = beta2 * delta1       delta1 = slope of regressing x2 on x1
b1hat <- 1.29                           # INPUT: multiple-regression slope on x1
b2hat <- -0.99                          # INPUT: multiple-regression slope on x2
delta1 <- 0.26                          # INPUT: slope from regressing x2 on x1
b1hat + b2hat * delta1                  # b1tilde (simple-regression slope)              (1.03)
b2hat * delta1                          # the bias = b1tilde - b1hat                     (-0.26)
## Solve for another piece:
b1tilde <- 0.85
b1tilde - b2hat * delta1                # b1hat                                          (1.11)
(b1tilde - b1hat) / delta1              # b2hat
(b1tilde - b1hat) / b2hat               # delta1
## Direction of the bias: sign(beta2) * sign(delta1)  [delta1 has the sign of Cor(x1, x2)]
beta2 <- 0.78                           # INPUT: effect of the omitted variable on y
delta1 <- 0.51                          # INPUT: link between omitted and included variable
beta2 * delta1                          # bias                                           (0.40)
ifelse(beta2 * delta1 > 0, "UPWARD bias", ifelse(beta2 * delta1 < 0, "DOWNWARD bias", "NO bias"))
## No bias when beta2 = 0 or delta1 = 0.

## ---- A15. PROBABILITY TABLE: E[X], E[X^2], Var(X), sd(X) ----
xv <- c(0, 2, 5)                        # INPUT: values X can take
p <- c(0.5, 0.3, 0.2)                   # INPUT: their probabilities (must sum to 1)
EX <- sum(p * xv)                       # E[X]                                           (1.6)
EX2 <- sum(p * xv^2)                    # E[X^2]                                         (6.2)
VarX <- EX2 - EX^2                      # Var(X) = E[X^2] - (E[X])^2                     (3.64)
sqrt(VarX)                              # sd(X)                                          (1.91)

## ---- A16. RULES FOR E, Var, sd, Cov (no data needed) ----
EX <- 3                                 # INPUT
VarX <- 4                               # INPUT
a <- 2                                  # INPUT
b <- -7                                 # INPUT
a * EX + b                              # E[aX + b]                                      (-1)
a^2 * VarX                              # Var(aX + b)  (b drops out)                     (16)
abs(a) * sqrt(VarX)                     # sd(aX + b)                                     (4)
VarX <- 7; VarY <- 7; CovXY <- -0.8     # INPUTS
a <- 3; cc <- -3                        # INPUTS: "3X - 3Y" means a = 3 and cc = -3
a^2 * VarX + cc^2 * VarY + 2 * a * cc * CovXY    # Var(aX + cc*Y)                         (140.4)

## ---- A17. TWO RANDOM VARIABLES FROM A JOINT TABLE (weight by the probabilities; do NOT use cor() on the rows) ----
xv <- c(1, 5, 7, 8)                     # INPUT: x for each row
yv <- c(3, 3, 1, 3)                     # INPUT: y for each row
p <- c(0.2, 0.3, 0.4, 0.1)              # INPUT: probability of each row
EX <- sum(p * xv)                       # E[X]                                           (5.3)
EY <- sum(p * yv)                       # E[Y]                                           (2.2)
CovXY <- sum(p * xv * yv) - EX * EY     # Cov = E[XY] - E[X]E[Y]                         (-1.36)
VarX <- sum(p * xv^2) - EX^2            # Var(X)                                         (5.61)
VarY <- sum(p * yv^2) - EY^2            # Var(Y)                                         (0.96)
CovXY / sqrt(VarX * VarY)               # Cor(X, Y) = Cov / (sd_x * sd_y)                (-0.59)
CovXY / VarX                            # population regression slope

## ---- A18. PERCENT CHANGE vs PERCENTAGE-POINT CHANGE ----
old <- 4.2                              # INPUT
new <- 5.1                              # INPUT
100 * (new - old) / old                 # percent change                                 (21.43)
new - old                               # percentage-point change                        (0.9)

## ---- A19. OLS PROPERTIES: NUMBERS YOU CAN GET WITHOUT DATA ----
-sum(c(2.1, -0.9, -0.1, -0.2, 0.4))     # INPUT: the known residuals -> the missing one = minus their sum    (-1.3)
b0 <- 9; b1 <- 3.9; xbar <- 11          # INPUTS (simple regression)
b0 + b1 * xbar                          # ybar: the point of means is on the line        (51.9)
b0 <- 6.8; b <- c(0.6, -1.1); xbars <- c(14.6, 3.1)    # INPUTS (multiple regression)
b0 + sum(b * xbars)                     # ybar = b0 + b1*xbar1 + b2*xbar2                (12.15)
## Always 0: sum of residuals, sum of x * residual (for each x).  NOT always 0: the residuals' link with Y itself.


## ================= 1. FROM BEFORE EXAM I (a few questions) =================
## Sample statistics (var and sd divide by n - 1)
x <- c(3, 3, 7)
mean(x); var(x); sd(x)
nrow(wage1)                             # sample size                                   (526)
mean(wage1$wage)                        # sample mean of wage                           (5.90)
sd(wage1$educ)                          # sample sd of educ                             (2.77)
mean(wage1$wage[wage1$female == 1])     # mean within a group
mean(wage1$wage[wage1$educ == 16])      # E[wage | educ = 16]  (Boolean -> select -> mean)  (8.04)
var(wage1$wage[wage1$educ == 12])       # Var(wage | educ = 12)

## Probability table: E[X], E[X^2], Var(X)
xv <- c(0, 2, 5)                        # values
p <- c(0.5, 0.3, 0.2)                   # probabilities (must sum to 1)
EX <- sum(p * xv)
EX2 <- sum(p * xv^2)
VarX <- EX2 - EX^2                      # Var(X) = E[X^2] - (E[X])^2
sqrt(VarX)                              # sd(X)

## Rules.  Var(aX + b) = a^2 Var(X)  (b drops out).   sd(aX + b) = |a| sd(X).   E[aX + b] = a E[X] + b
EX <- 3
EX2 <- 13
a <- 2
b <- -7
a * EX + b                              # E[aX + b]
abs(a) * sqrt(EX2 - EX^2)               # sd(aX + b)                                    (= 4)
## Var(aX + cY) = a^2 Var(X) + c^2 Var(Y) + 2*a*c*Cov(X,Y)      "3X - 3Y" means c = -3
VarX <- 7
VarY <- 7
CovXY <- -0.8
a <- 3
c2 <- -3
a^2 * VarX + c2^2 * VarY + 2 * a * c2 * CovXY   # (= 140.4)

## Joint table: WEIGHT BY THE PROBABILITIES (do not use cor() on the rows)
xv <- c(1, 5, 7, 8)
yv <- c(3, 3, 1, 3)
p <- c(0.2, 0.3, 0.4, 0.1)
EX <- sum(p * xv)
EY <- sum(p * yv)
CovXY <- sum(p * xv * yv) - EX * EY     # Cov = E[XY] - E[X]E[Y]
VarX <- sum(p * xv^2) - EX^2
VarY <- sum(p * yv^2) - EY^2
CovXY / sqrt(VarX * VarY)               # Cor(X, Y)                                     (= -0.59)

## Population quantity (mu_Y, sigma^2) = a FIXED number.  Sample statistic (Ybar, S^2) = a RANDOM variable.


## ================= 2. UNITS OF MEASUREMENT (Day 13) =================
## y times c  -> slope AND intercept times c.      x times c -> slope divided by c, intercept SAME.
## Dividing by k is multiplying by c = 1/k.        R^2 and fit do not change.

b1 <- 0.01547                           # slope of salary (thousands) on sales
b1 * 1000                               # salary in DOLLARS (y times 1000): slope x 1000   (= 15.47, NOT 0.015)
b1 <- 1.32
b1 * 10                                 # every y times 10: new slope                    (= 13.2)
b1 <- 0.58
b1 / (1/1000)                           # every x divided by 1000 (c = 1/1000): slope x 1000   (= 580)
b1 / 10                                 # every x times 10: slope divided by 10 (intercept and R^2 unchanged)

## Check with data
reg2 <- lm(salary ~ sales, data = ceosal1)
reg2$coefficients[[2]]                  # 0.01547
ceosal1$salarydol <- 1000 * ceosal1$salary
lm(salarydol ~ sales, data = ceosal1)$coefficients[[2]]   # 15.47: slope times 1000
ceosal1$roedec <- ceosal1$roe / 100
lm(salary ~ roedec, data = ceosal1)$coefficients          # slope x 100, intercept same


## ================= 3. LOGS (Table 2.3) =================
##   level-level:  change in y   = b1 * (change in x)
##   log-level:    % change in y = 100 * b1 * (change in x)
##   level-log:    change in y   = (b1 / 100) * (% change in x)
##   log-log:      % change in y = b1 * (% change in x)          (b1 is an ELASTICITY)
## A FALL is a NEGATIVE change.  Give a percent as 12.34, not 0.1234.
b1 <- 0.08
100 * b1 * 2                            # log-level, x rises by 2                       (= 16)
100 * b1 * (-1)                         # log-level, x FALLS by 1
b1 <- 0.17
b1 * (-5)                               # log-log, x falls by 5%                        (= -0.85)
b1 <- 0.49
b1 * 16                                 # log-log, x rises by 16%                       (= 7.84)
b1 <- 6.4
(b1 / 100) * 2                          # level-log, x rises by 2%: change in y UNITS   (= 0.13)

## From data
reg3 <- lm(log(wage) ~ educ, data = wage1)
100 * reg3$coefficients[[2]]            # % change in wage from +1 year of educ
reg4 <- lm(log(salary) ~ log(sales), data = ceosal1)
reg4$coefficients[[2]]                  # elasticity
reg5 <- lm(salary ~ log(sales), data = ceosal1)
(reg5$coefficients[[2]] / 100) * 10     # level-log: change in salary ($1000s) when sales rise 10%  (= 26.29)
reg6 <- lm(log(salary) ~ roe, data = ceosal1)
100 * reg6$coefficients[[2]] * 5        # log-level: % change when roe rises 5 points   (= 6.93)
reg7 <- lm(log(wage) ~ exper, data = wage1)
100 * reg7$coefficients[[2]] * 10       # % change in wage from 10 more years of exper   (= 4.36)

## Units change INSIDE a log
## log-log, y times 1000:  slope does NOT change, intercept gets + log(1000)
5.93 + log(1000)                        # new intercept                                  (= 12.84)
## log-level, change the units of x (months -> years): slope times 12
100 * 0.0069 * 12                       # % change in wage per extra YEAR of tenure      (= 8.28)

## Predicted log(y) -> y :  exp()   (do not round before exp)
exp(1.5)
## Regressors in logs: plug in the LOG of the value
## exp(b[1] + b[2]*log(5000) + b[3]*15)      # sales = 5000, roe = 15


## ================= 4. SLR BY HAND, PREDICTION, RESIDUALS, R^2 =================
x <- c(1, 3, 4, 5, 8)
y <- c(10, 5, 6, 8, 5)
b1 <- cov(x, y) / var(x)                # slope
b0 <- mean(y) - b1 * mean(x)            # intercept
reg <- lm(y ~ x)
reg$coefficients
resid(reg)[3]                           # residual of observation 3 = actual - fitted    (= -0.9)
y[3] - (b0 + b1 * x[3])                 # same, by hand

## From the regression of wage on educ
reg <- lm(wage ~ educ, data = wage1)
b0 <- reg$coefficients[[1]]             # -0.90
b1 <- reg$coefficients[[2]]             # 0.54
b0 + b1 * 14                            # predicted wage at educ = 14                    (= 6.67)
b1 * 2                                  # predicted CHANGE from +2 years                 (= 1.08)
reg$residuals[12]                       # residual of the worker in row 12
SSR <- sum(reg$residuals^2)             # 5980.68
SST <- sum((wage1$wage - mean(wage1$wage))^2)
1 - SSR / SST                           # R^2                                           (= 0.16)
summary(reg)$r.squared                  # check
summary(reg)$sigma                      # sigma-hat = standard error of the regression  (= 3.38)

## SST, SSE, SSR from summary numbers
n <- 54
s2y <- 8
(n - 1) * s2y                           # SST = (n - 1) * sample variance of y          (= 424)
SSE <- 128
R2 <- 0.33
SST <- SSE / R2                         # SST = SSE / R^2
SST - SSE                               # SSR = SST - SSE                               (= 259.88)

## Which line is OLS?  Among lines whose residuals SUM TO 0, OLS has the SMALLEST sum of squared residuals.


## ================= 5. OLS PROPERTIES (answers without computing) =================
reg <- lm(wage ~ educ + exper, data = wage1)
sum(reg$residuals)                      # always 0   (R may print something like -8.7e-15: that IS 0)
sum(wage1$educ * reg$residuals)         # residuals and EACH x: always 0  (so sum x_i*u_i = 0, cov = 0)
sum(wage1$exper * reg$residuals)        # always 0
cor(wage1$wage, reg$residuals)          # NOT 0: residuals are uncorrelated with the FITTED values, not with Y
-sum(c(2.1, -0.9, -0.1, -0.2, 0.4))     # missing residual = minus the sum of the others  (= -1.3)
## Point of means: the line passes through (xbar, ybar):  ybar = b0 + b1*xbar
9 + 3.9 * 11                            # ybar from b0 = 9, b1 = 3.9, xbar = 11          (= 51.9)
6.8 + 0.6 * 14.6 - 1.1 * 3.1            # MLR: ybar = b0 + b1*xbar1 + b2*xbar2           (= 12.15)
## With data: predicting at the MEANS of the x's gives the mean of y
reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
b <- reg$coefficients
b[[1]] + b[[2]] * mean(gpa1$ACT) + b[[3]] * mean(gpa1$hsGPA)   # = mean(gpa1$colGPA)  (3.06)
mean(gpa1$colGPA)
## ALWAYS TRUE:  (1) residuals average to 0   (2) each x is uncorrelated with the residuals   (3) point of means on the line
## NOT always true: residuals uncorrelated with Y itself.


## ================= 6. VARIANCE AND SE OF THE SLOPE, SIMPLE REGRESSION (Days 15-16) =================
## Var(b1hat) = sigma^2 / SST_X ,  sd(b1hat) = sigma / sqrt(SST_X) ,  SST_X = (n - 1) * S_X^2
## S_X^2 is a VARIANCE. If you are given the sd of X, SQUARE it:  (n - 1) * sdx^2
## sigma = true error sd (sd, Var).  sigma-hat = sqrt(SSR / (n - 2)) (se).
132 / 77                                # Var(b1hat) from sigma^2 = 132, SST_X = 77      (= 1.71)
n <- 9
sigma <- 11
S2X <- 5
SSTX <- (n - 1) * S2X                   # = 40
sigma / sqrt(SSTX)                      # sd(b1hat)                                      (= 1.74; do NOT round before this)
## From the x values
x <- c(4, 5, 6, 7, 9)
SSTX <- sum((x - mean(x))^2)
5 / sqrt(SSTX)                          # sd(b1hat) with sigma = 5                       (= 1.30)
## Standard error needs sigma-hat = sqrt(SSR / (n - 2))
SSR <- 75
n <- 5
x <- c(3, 4, 5, 7, 10)
sigmahat <- sqrt(SSR / (n - 2))
sigmahat / sqrt(sum((x - mean(x))^2))   # se(b1hat)                                      (= 0.90)
SSR <- 462
n <- 10
SSTX <- 12
sqrt(SSR / (n - 2)) / sqrt(SSTX)        # se(b1hat)                                      (= 2.19)
756 / (31 - 2)                          # sigma-hat^2 = SSR / (n - 2)                    (= 26.07)
sigmahat <- 6.4                         # given sigma-hat and the SD of X:
n <- 12
Sx <- 1.6
sigmahat / sqrt((n - 1) * Sx^2)         # se(b1hat)                                      (= 1.21)
## Bigger sample: SST_X k times larger -> sd divides by sqrt(k), NOT by k
2.35 / sqrt(4)                          # (= 1.18)
## From data, then check against summary():
reg <- lm(salary ~ roe, data = ceosal1)
n <- nrow(ceosal1)
SSR <- sum(reg$residuals^2)
SSTX <- sum((ceosal1$roe - mean(ceosal1$roe))^2)
sqrt(SSR / (n - 2)) / sqrt(SSTX)        # (= 11.12)
summary(reg)$coefficients               # Std. Error column
sum((wage1$educ - mean(wage1$educ))^2)  # SST_X for educ                                 (= 4025.43)

## Effects on Var(b1hat) = sigma^2 / SST_X:
##   BIGGER: larger sigma^2 ; X values bunched together (smaller SST_X) ; smaller sample
##   SMALLER: more spread-out X (larger SST_X) ; larger sample
##   NO effect: the true slope beta1, the intercept beta0.


## ================= 7. MULTIPLE REGRESSION: PREDICTION AND CHANGES (Days 17-19) =================
## Each slope = ceteris paribus effect (others held fixed).  Intercept never matters for a CHANGE.
b0 <- 9
b1 <- 2.6
b2 <- 1.6
b0 + b1 * 8 + b2 * 16                   # predicted y at x1 = 8, x2 = 16                (= 55.4)
b1 <- 3.8
b2 <- -0.7
b1 * 1 + b2 * 4                         # change in y-hat: x1 +1 and x2 +4              (= 1)

## From data (levels)
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients                   # b[[1]] intercept, b[[2]] educ, b[[3]] exper, b[[4]] tenure
b[[1]] + b[[2]] * 14 + b[[3]] * 10 + b[[4]] * 5     # predicted wage                  (= 6.58)
b[[4]] * 3                              # tenure +3, others fixed                       (= 0.51)

## LOG-LEVEL with several variables at once
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
100 * (b[[3]] * 2 + b[[4]] * 1)         # % change: exper +2 AND tenure +1, educ fixed   (= 3.03)
exp(b[[1]] + b[[2]] * 12 + b[[3]] * 10 + b[[4]] * 3)   # wage-hat in dollars            (= 4.46)
100 * (0.066 * 3 + (-0.019) * 4)        # from summary numbers: x1 +3, x2 +4  (careful: b2 negative)   (= 12.2)
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b <- reg$coefficients
100 * b[[3]] * 10                       # roe +10 points (log-level term), sales fixed   (= 17.87)
exp(b[[1]] + b[[2]] * log(5000) + b[[3]] * 15)         # salary-hat ($1000s), sales = 5000, roe = 15   (= 1067.66)

## Predict at the MEANS (gpa1): equals mean(colGPA)
reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
b <- reg$coefficients
b[[1]] + b[[2]] * mean(gpa1$ACT) + b[[3]] * mean(gpa1$hsGPA)

## Compare simple and multiple regression (bwght)
reg_s <- lm(bwght ~ cigs, data = bwght)
reg_m <- lm(bwght ~ cigs + faminc, data = bwght)
reg_s$coefficients[[2]]                 # simple slope                                   (= -0.51)
reg_m$coefficients[[2]]                 # slope holding faminc fixed                     (= -0.46)
reg_s$coefficients[[2]] * 5             # 5 more cigarettes per day, simple regression   (= -2.57)


## ================= 8. ASSUMPTIONS MLR1-MLR4 AND PERFECT COLLINEARITY =================
## MLR3 (no perfect collinearity) FAILS when:
##   - one x is an exact linear function of the others (totexpend = expendA + expendB;
##     income in dollars AND income in thousands; x2 = 2*x1; dummy-variable trap)
##   - an x has NO variation (every worker has educ = 12)  (SLR3)
##   - more variables than observations (5 independent variables, 4 observations)
## It does NOT fail when two x's are merely highly (but not perfectly) correlated.
vote1$totexpend <- vote1$expendA + vote1$expendB
lm(voteA ~ expendA + expendB + totexpend, data = vote1)   # R reports NA for totexpend (it drops it)
## MLR4 E[U | x1,...,xK] = 0 FAILS (endogeneity) when: an omitted variable affects Y AND is correlated with an x
##   (ability omitted in wage on educ), wrong functional form.  NOT when units change, n is small, or x's are correlated.
## Unbiasedness needs MLR1-MLR4.  MLR5 (homoskedasticity) is needed for the variance formulas and Gauss-Markov.
## Why not add every variable?  More variables can RAISE variance and can even violate MLR3 (and R^2 never falls).


## ================= 9. OMITTED-VARIABLE BIAS (Days 15, 18-20) =================
## You OMIT x2 and regress y on x1 only.
##   b1tilde = simple slope ;  b1hat, b2hat = multiple-regression slopes ;  delta1 = slope of lm(x2 ~ x1)
## (3.23):  b1tilde = b1hat + b2hat * delta1
1.29 + (-0.99) * 0.26                   # b1tilde from b1hat = 1.29, b2hat = -0.99, delta1 = 0.26   (= 1.03)
0.85 - 1.27 * (-0.16)                   # solve for b1hat:  b1hat = b1tilde - b2hat*delta1          (= 1.05)
b1tilde <- 0.62; b1hat <- 0.5
(b1tilde - b1hat) / 0.3                 # solve for delta1 (given b2hat);   or  / delta1 for b2hat
0.78 * 0.51                             # Bias(b1tilde) = beta2 * delta1                            (= 0.40, biased UP)

## Check with data (bwght: omit faminc, keep cigs)
reg_s <- lm(bwght ~ cigs, data = bwght)
reg_m <- lm(bwght ~ cigs + faminc, data = bwght)
reg_d <- lm(faminc ~ cigs, data = bwght)                # omitted regressed on the included
b1tilde <- reg_s$coefficients[[2]]
b1hat <- reg_m$coefficients[[2]]
b2hat <- reg_m$coefficients[[3]]
delta1 <- reg_d$coefficients[[2]]
b2hat * delta1                          # = b1tilde - b1hat                              (= -0.05)
b1tilde - b1hat                         # check
## wage1: omit exper, keep educ
b1tilde <- lm(wage ~ educ, data = wage1)$coefficients[[2]]
reg_m <- lm(wage ~ educ + exper, data = wage1)
delta1 <- lm(exper ~ educ, data = wage1)$coefficients[[2]]
reg_m$coefficients[[3]] * delta1        # b2hat * delta1 = b1tilde - b1hat               (= -0.10)
b1tilde - reg_m$coefficients[[2]]       # check

## Table 3.2: sign of bias = sign(beta2) * sign(delta1) = sign(beta2) * sign(Corr(x1, x2))
##                      Corr(x1,x2) > 0          Corr(x1,x2) < 0
##    beta2 > 0         UPWARD (positive)        DOWNWARD (negative)
##    beta2 < 0         DOWNWARD (negative)      UPWARD (positive)
## NO bias when beta2 = 0 (omitted variable does not affect y)  OR  delta1 = 0 (uncorrelated with x1 in the sample).
## NOT unbiased just because the sample is large, or the omitted variable is "not in the regression".
## Stories:  ability raises wage (+) and rises with educ (+)  -> educ slope biased UP.
##           land quality raises yield (+) and farmers use more fertilizer on good land (+) -> UP (overstates fertilizer).
##           ability raises scores (+) but low-ability students sign up for tutoring (-) -> DOWN (understates tutoring).
##           smokers have lower incomes (delta1 < 0), income raises birth weight (beta2 > 0) -> cigs slope too negative
##           (-0.51 vs -0.46): about -0.46 is smoking holding income fixed, the other -0.05 is income riding along.


## ================= 10. VARIANCES AND STANDARD ERRORS, MULTIPLE REGRESSION (Days 20-21) =================
## Var(bj-hat) = sigma^2 / [ SST_j * (1 - R_j^2) ]       se(bj-hat) = sigma-hat / sqrt( SST_j * (1 - R_j^2) )
##   SST_j = (n - 1) * var(xj)   (n = FULL sample size)
##   R_j^2 = R-squared from regressing xj on ALL the OTHER x's (not on y)
##   sigma-hat = sqrt(SSR / (n - K - 1))      df = n - K - 1
## Bigger variance: larger sigma^2, smaller SST_j, LARGER R_j^2 (more collinearity). R_j^2 = 1 -> perfect collinearity.

## By hand from data, then check against summary()     (wage on educ and exper; se(educ) about 0.05)
reg <- lm(wage ~ educ + exper, data = wage1)
K <- 2
n <- nrow(wage1)                        # 526, NOT a leftover small n
dof <- n - K - 1
SSR <- sum(reg$residuals^2)
sigmahat <- sqrt(SSR / dof)
SSTj <- (n - 1) * var(wage1$educ)       # SST_j for educ
R2j <- summary(lm(educ ~ exper, data = wage1))$r.squared      # with more x's: lm(educ ~ exper + tenure, ...)
sigmahat / sqrt(SSTj * (1 - R2j))       # se(educ) by hand
summary(reg)$coefficients               # Std. Error column  (should match)

## From summary numbers
SSR0 <- 120
n0 <- 50
K0 <- 3
SSTj0 <- 200
R2j0 <- 0.4
sigmahat0 <- sqrt(SSR0 / (n0 - K0 - 1))
sigmahat0 / sqrt(SSTj0 * (1 - R2j0))    # se(bj-hat)                                     (about 0.15)
sigmahat0^2 / (SSTj0 * (1 - R2j0))      # Var(bj-hat)

## Solve backwards when the question gives the se
se0 <- 0.15
sigmahat0^2 / (se0^2 * (1 - R2j0))      # SST_j from se, sigma-hat and R_j^2
1 - sigmahat0^2 / (se0^2 * SSTj0)       # R_j^2 from se, sigma-hat and SST_j
se0 * sqrt(SSTj0 * (1 - R2j0))          # sigma-hat from se, SST_j and R_j^2
sigmahat0^2 * (n0 - K0 - 1)             # SSR from sigma-hat

## ---- Problem Set 12 (hprice1): price on sqrft, bdrms, lotsize;  X_j = bdrms ----
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
n <- nrow(hprice1)                      # 88
K <- 3                                  # independent variables (NOT the intercept)
dof <- n - K - 1                        # degrees of freedom                             (= 84)
SSR <- sum(reg$residuals^2)
sigma_hat <- sqrt(SSR / dof)            # standard error of the regression               (= 59.83)
SST_j <- sum((hprice1$bdrms - mean(hprice1$bdrms))^2)     # total variation of bdrms     (= 61.59)
R2_j <- summary(lm(bdrms ~ sqrft + lotsize, data = hprice1))$r.squared   # (= 0.28)  Y (price) is NOT in this regression
sigma_hat / sqrt(SST_j * (1 - R2_j))    # se(bdrms) by hand                              (= 9.01)
## Use the R object R2_j (unrounded), not the typed 0.28.
summary(reg)$coefficients[3, 2]         # bdrms is row 3 (intercept = row 1), Std. Error is column 2   (= 9.01)
summary(reg)$coefficients["bdrms", "Std. Error"]          # same, by name
summary(reg)                            # "Residual standard error: 59.83 on 84 degrees of freedom" = sigma_hat (and its df)

## MISSING DATA: lm() silently drops every row with an NA in ANY variable of the regression.
## Compute n, SST_j and R2_j on the SAME rows the regression used, or your hand SE will not match summary(reg):
mf <- model.frame(reg)                  # only the rows lm actually used
n <- nrow(mf)
SST_j <- sum((mf$bdrms - mean(mf$bdrms))^2)
R2_j <- summary(lm(bdrms ~ sqrft + lotsize, data = mf))$r.squared
## The same issue affects sample means and sds:  mean(x, na.rm = TRUE),  sd(x, na.rm = TRUE)

## What each piece is:
##   X_j = the independent variable whose coefficient you want the standard error of.
##   R_j^2 = R-squared from regressing X_j on the OTHER independent variables (Y is not involved):
##           in wage ~ educ + exper + tenure, for exper:  lm(exper ~ educ + tenure)
##   sigma-hat estimates sigma, the standard deviation of the error U (built from the residuals).
##   As R_j^2 -> 1 (X_j nearly a linear function of the others), 1 - R_j^2 -> 0 and se GROWS WITHOUT BOUND (multicollinearity).
##   se is always POSITIVE: sigma-hat DIVIDED by sqrt(...).  A negative se means a minus sign slipped in for the division.

## Gauss-Markov: under MLR1-MLR5, OLS is BLUE (Best Linear Unbiased Estimator).
##   "Best" = smallest variance among all LINEAR UNBIASED estimators (not among all estimators).


## ================= 11. CONCEPT ANSWER BANK (select-all / multiple choice) =================
## Unbiased (E[b1hat] = beta1):  averaged over MANY random samples, b1hat equals beta1.
##   NOT: every sample gives beta1 exactly / the residuals are zero / R^2 = 1 / no spread at all.
## Why prefer an unbiased estimator with SMALLER variance?  Its estimates land closer to beta1 from sample to
##   sample, so any ONE estimate is more reliable.  (Not because it raises R^2 or makes it unbiased.)
## Zero conditional mean E[U | X] = 0 FAILS when the average of U changes with X (family income is in U and richer
##   districts have smaller classes).  It does not hold just because the omitted variable is "not in the model".
## SLR3 fails: every X the same (all workers have educ = 12): slope cannot be computed.
## SLR4 fails: omitted variable correlated with X -> slope is biased.
## SLR5 (homoskedasticity) fails: spread of the errors grows with X (food spending vs income): the variance
##   formula sigma^2/SST_X no longer applies, but OLS is STILL unbiased.
## beta1-hat in wage = b0 + b1*educ + b2*exper: predicted change in wage from one more year of education,
##   HOLDING EXPERIENCE FIXED (ceteris paribus).
## Level-log slope 8 in  score = 12 + 8*log(income):  a 1% rise in income -> about 0.08 POINTS (b1/100).
##   In the exam wording: 1% rise in income is associated with a rise of 0.08 points.  (Check the exact wording given.)
## log-log slope 0.45: a 1% rise in income -> about a 0.45% rise in rent (elasticity).
## Model where %change(y) = 100*b1*change(x):  LOG-LEVEL, log(y) = b0 + b1*x.
## Perfect collinearity examples: sum of the other variables; same variable in different units; more variables than obs.
## A wrong-sign simple regression (lunch program lowers math scores) usually means a confounder (poverty drives both).
## Ability is the classic omitted variable in wage on educ: it raises wages and is higher for the more educated.


## ================= 12. R SKILLS QUICK REFERENCE =================
## reg <- lm(y ~ x1 + x2, data = d)          lm(log(wage) ~ educ + exper, data = wage1)
## reg$coefficients[2]      reg$coefficients[[2]] (drops the label)      reg$residuals[3]      reg$fitted.values
## summary(reg)$r.squared      summary(reg)$coefficients      summary(reg)$sigma
## nrow(d)   mean(x)   sd(x)   var(x)   mean(d$y[d$female == 1])   mean(d$y[d$x == 16])
## SST <- sum((x - mean(x))^2)           SSR <- sum(reg$residuals^2)        exp(...) undoes log()
## b[[1]] is the intercept, b[[2]] the first slope, b[[3]] the second, ...
## Use a NEW name for a new regression (reg2) so you do not overwrite one you still need.
## Round to the hundredths place at the very END only:  round(answer, 2)


## ======================================================================================================
## ================= 13. EXAM II: THE FIVE QUESTIONS THAT FOLLOW PROBLEM SET 12 (problems 2, 4, 6, 9, 12) ==
## ======================================================================================================
## The professor says about five exam questions closely follow Problem Set 12 problems 2, 4, 6, 9 and 12.
## Each block below is a template: overwrite the numbers / variable names and run.  Round to hundredths at the end.

## ---- PS12 #2: "Which regression gives R_j^2?" ----
## Regress X_j on ALL the OTHER independent variables.  Y is NOT in it, and X_j is on the LEFT.
## wage = b0 + b1 educ + b2 exper + b3 tenure + U ; want se(b2hat), so X_j = exper:
##    ANSWER:  lm(exper ~ educ + tenure)            (NOT lm(wage ~ exper), NOT lm(exper ~ wage), NOT wage on all three)
summary(lm(exper ~ educ + tenure, data = wage1))$r.squared      # this is R_j^2 for exper

## ---- PS12 #4: sigma-hat from SSR, n, K ----
## df = n - K - 1 ;  sigma-hat = sqrt(SSR / df)
n <- 32
K <- 4
SSR <- 2921
dof <- n - K - 1                        # 27
sqrt(SSR / dof)                         # sigma-hat                                      (= 10.40)

## ---- PS12 #6: se(bj-hat) from sigma-hat, SST_j, R_j^2 ----
## se = sigma-hat / sqrt( SST_j * (1 - R_j^2) )     Divide!  Note the (1 - R_j^2).  The answer is positive.
sigma_hat <- 7.9
SST_j <- 357
R2_j <- 0.27
sigma_hat / sqrt(SST_j * (1 - R2_j))    # se(bj-hat)                                     (= 0.49)
## If SST_j has to be built first:  SST_j = (n - 1) * (sd of X_j)^2   (square the sd!)
n <- 20
Sxj <- 1.3
(n - 1) * Sxj^2                         # SST_j                                          (= 32.11)

## ---- PS12 #9: sigma-hat by hand from data (hprice1) ----
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
n <- nrow(hprice1)
K <- 3                                  # sqrft, bdrms, lotsize
dof <- n - K - 1                        # 84
SSR <- sum(reg$residuals^2)
sigma_hat <- sqrt(SSR / dof)            #                                                (= 59.83)
summary(reg)$sigma                      # check: "Residual standard error" in summary(reg)

## ---- PS12 #12: full se(bj-hat) by hand from data ----
## Change xj and others to switch variables (plain variable names). Everything is computed on the rows lm used.
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
xj <- "bdrms"                           # the coefficient whose se you want
others <- c("sqrft", "lotsize")         # all the OTHER independent variables
mf <- model.frame(reg)                  # same rows the regression used (safe if there are NAs)
n <- nrow(mf)
K <- length(others) + 1                 # number of independent variables
sigma_hat <- sqrt(sum(reg$residuals^2) / (n - K - 1))
SST_j <- sum((mf[[xj]] - mean(mf[[xj]]))^2)
R2_j <- summary(lm(reformulate(others, response = xj), data = mf))$r.squared    # e.g. bdrms ~ sqrft + lotsize
sigma_hat / sqrt(SST_j * (1 - R2_j))    # se(bj-hat) by hand                             (bdrms: 9.01)
summary(reg)$coefficients[xj, "Std. Error"]       # check: should match
## Quick check pieces:  sigma_hat 59.83 | SST_j 61.59 | R2_j 0.28 | df 84   (bdrms)
## Then the quick way:  summary(reg)$coefficients   (Std. Error is column 2; row = the variable)

## ---- If the exam gives you some pieces and asks for another ----
## se, sigma-hat, SST_j, R_j^2 are linked: se = sigma-hat / sqrt(SST_j * (1 - R_j^2)).  Solve for the missing one:
se0 <- 0.49; sigma_hat <- 7.9; SST_j <- 357; R2_j <- 0.27
sigma_hat^2 / (se0^2 * (1 - R2_j))      # SST_j
1 - sigma_hat^2 / (se0^2 * SST_j)       # R_j^2
se0 * sqrt(SST_j * (1 - R2_j))          # sigma-hat
## Var(bj-hat) = se^2.   SSR = sigma-hat^2 * (n - K - 1).   R_j^2 near 1 -> se huge.  Missing data -> use model.frame(reg).





## =============== CODE FOR SPECIFIC QUESTIONS ======================================================

# Regress wage on educ : find SST_x, sigmahat, and se(b1hat)

reg <- lm(wage ~ educ, data = wage1)
n <- nrow(wage1)                                   
SSR <- sum(reg$residuals^2)                        
sigma_hat <- sqrt(SSR / (n - 2))                      
SST_X <- sum((wage1$educ - mean(wage1$educ))^2)                 
se(b1hat) <- sigma_hat / sqrt(SST_X)                            # se(b1hat) = 0.0532 -> 0.05

summary(reg)$sigma                                 # check sigma_hat
summary(reg)$coefficients                          # check: Std. Error for educ


# Regression to give Rj^2 for exper

summary(lm(exper ~ educ + tenure, data = wage1))$r.squared


# find sigmahat and seb1 given n, SSR and SST_x

# INPUTS
n <- 18                                 # sample size
SSR <- 400                              # sum of squared residuals
SST_X <- 50                             # given directly

sigma_hat <- sqrt(SSR / (n - 2))        # (a) simple regression: df = n - 2        5
se_b1 <- sigma_hat / sqrt(SST_X)        # (b) se(b1hat)                            0.7071 -> 0.71
se_b1_big <- se_b1 / sqrt(4)            # (c) sample 4 times as large              0.3536 -> 0.35

round(sigma_hat, 2)
round(se_b1, 2)
round(se_b1_big, 2)


## ======================== CHEAT CODES =================================================

n <- nrow(hprice1)                                                # k <- no. independent variables 

SSR <- SSR <- sum(reg$residuals^2)

sigma_hat <- sqrt(SSR / dof)

SST_j <- sum((hprice1$bdrms - mean(hprice1$bdrms))^2)             # SST_j for dataset

R2_j <- summary(lm(bdrms ~ sqrft + lotsize, data = hprice1))$r.squared

se(bjhat) <- sigma_hat / sqrt(SST_j * (1 - R2_j))

summary(reg)$coefficients                                         # to view std.error and other values 
