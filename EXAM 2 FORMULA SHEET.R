# STANDARDISED MASTER SCRIPT - EXAM II   (fill in the exam's numbers once, then look up any variable with Cmd+F)
## ======================================================================================================
## HOW IT WORKS
##  1. Fill in the INPUTS block below with the numbers the question GIVES you. Leave everything else as NA.
##  2. Cmd+F the thing you need, in square brackets, e.g.   >>> [SSR]   or   [se_bj]   (only entry headers use [ ] ).
##  3. Select that entry's lines and run (Cmd+Enter). Every line is ONE WAY to get the variable:
##        a number  = that way works with the inputs you filled in
##        NA        = that way needs an input you were not given (ignore it)
##  4. Multi-step question? Type the answer you just got (unrounded) back into the INPUTS block, then look up the next variable.
##  5. Lines starting with  #D  need your fitted model / data (reg, d, x, y). Remove the # (Cmd+Shift+C) and use them.
##  6. Round only at the very end:  round(answer, 2).   K = number of SLOPE variables (not the intercept).
## ======================================================================================================
options(digits = 10)                    # print many digits so you can copy unrounded values
library(wooldridge)                     # only needed for the #D (data) ways

## ======================================================================================================
## FORMULA SHEET (read once)
## ======================================================================================================
##  SST = sum (y - ybar)^2 = (n-1)*S2y            SSE = SST - SSR          SSR = sum uhat^2 = SST - SSE
##  R2  = SSE/SST = 1 - SSR/SST                   uhat = y - yhat          yhat = b0 + b1*x1 + ... + bK*xK
##  df  = n - K - 1 (simple regression: n - 2)    sigma2_hat = SSR/df      sigma_hat = sqrt(SSR/df)
##  SST_X = SST_j = (n-1)*(sample variance of x)  (square an sd first!)
##  SIMPLE:    Var(b1hat) = sigma^2 / SST_X       sd(b1hat) = sigma / sqrt(SST_X)     se(b1hat) = sigma_hat / sqrt(SST_X)
##  MULTIPLE:  Var(bj-hat) = sigma^2 / [SST_j (1 - R2_j)]      se(bj-hat) = sigma_hat / sqrt( SST_j (1 - R2_j) )
##             R2_j = R2 from regressing X_j on the OTHER x's (Y not included);   VIF = 1/(1 - R2_j)
##  OVB:       b1tilde = b1hat + b2hat*delta1      bias = beta2*delta1      delta1 = slope of lm(x2 ~ x1)
##  b1 = Cov(x,y)/Var(x) = Cor*Sy/Sx              b0 = ybar - b1*xbar      ybar = b0 + b1*xbar1 + ...   (point of means)
##  LOGS:      log-level  %dy = 100*b1*dx   |  level-log  dy = (b1/100)*%dx   |  log-log  %dy = b1*%dx   |  level-level  dy = b1*dx
##  UNITS:     y x cy -> slope and intercept x cy ;  x x cx -> slope / cx, intercept same ;  R2 unchanged
##  PROB:      E[X] = sum x*p   Var(X) = E[X^2] - E[X]^2   Var(aX+b) = a^2 Var(X)   sd(aX+b) = |a| sd(X)
##             Var(aX+cY) = a^2 VarX + c^2 VarY + 2ac Cov   Cov = E[XY] - E[X]E[Y]   Cor = Cov/(sdX*sdY)
## ======================================================================================================

## ======================================================================================================
## INPUTS  (fill what you are given; leave NA otherwise)
## ======================================================================================================
n <- NA;  K <- NA;  dof <- NA                       # sample size, number of slope variables, degrees of freedom
SST <- NA;  SSR <- NA;  SSE <- NA;  R2 <- NA        # sums of squares and R-squared (of the main regression)
S2y <- NA;  Sy <- NA                                # sample VARIANCE and sd of y
S2x <- NA;  Sx <- NA                                # sample VARIANCE and sd of an x
SST_X <- NA;  SST_j <- NA                           # total variation of an x (simple / multiple regression)
SSR_j <- NA;  R2_j <- NA;  VIF <- NA                # auxiliary regression of X_j on the other x's
sigma <- NA;  sigma2 <- NA                          # TRUE error sd / variance
sigma_hat <- NA;  sigma2_hat <- NA                  # ESTIMATED error sd / variance (standard error of the regression)
se <- NA;  Var_b <- NA                              # se (or sd) of the slope you are asked about, and its variance
m <- NA;  kk <- NA                                  # "sigma multiplied by m" ; "sample kk times as large"
b0 <- NA;  b1 <- NA;  xbar <- NA;  ybar <- NA       # intercept, slope, means (simple regression)
Cov <- NA;  Cor <- NA                               # sample covariance / correlation of x and y
b <- c(NA, NA)                                      # slopes b1, b2, ... of a multiple regression (same order as xvec)
xvec <- c(NA, NA)                                   # values of x1, x2, ... (a logged x enters as log(value))
dxvec <- c(NA, NA)                                  # changes in x1, x2, ... (0 = held fixed; a FALL is negative)
xbars <- c(NA, NA)                                  # means of x1, x2, ...
y0 <- NA;  u_known <- c(NA, NA)                     # an actual y value ; residuals you already know
b1hat <- NA;  b2hat <- NA;  b1tilde <- NA;  delta1 <- NA;  beta2 <- NA;  bias <- NA     # omitted-variable bias
dx <- NA;  pct_dx <- NA;  logyhat <- NA             # change in x (units) ; % change in x ; predicted log(y)
cy <- NA;  cx <- NA                                 # y multiplied by cy ; x multiplied by cx  (divided by k means c = 1/k)
val_old <- NA;  val_new <- NA                       # for % change
xv <- c(NA, NA, NA);  yv <- c(NA, NA, NA);  p <- c(NA, NA, NA)   # a probability table (values and probabilities)
EX <- NA;  EX2 <- NA;  VarX <- NA;  VarY <- NA;  CovXY <- NA;  sdX <- NA
a <- NA;  c0 <- NA;  cc <- NA                       # constants in aX + c0   and   aX + cc*Y


## ======================================================================================================
## A. SAMPLE STATISTICS
## ======================================================================================================

#### >>> [n]   sample size
dof + K + 1                             # W1 dof, K
SST / S2y + 1                           # W2 SST, S2y
SST_X / S2x + 1                         # W3 SST_X, S2x
#D nrow(d)    length(x)                 # W4 from data

#### >>> [S2x] and [S2y]   sample variance = SST / (n - 1)
Sx^2                                    # W1 from the sd
SST_X / (n - 1)                         # W2 x: SST_X, n
SST / (n - 1)                           # W3 y: SST, n
#D var(x)

#### >>> [Sx] and [Sy]   sample sd
sqrt(S2x)                               # W1 x from variance
sqrt(SST_X / (n - 1))                   # W2 x: SST_X, n
sqrt(SST / (n - 1))                     # W3 y: SST, n
#D sd(x)

#### >>> [cov_cor]   covariance / correlation of x and y
Cor * Sx * Sy                           # W1 Cov: Cor, Sx, Sy
Cov / (Sx * Sy)                         # W2 Cor: Cov, Sx, Sy
b1 * S2x                                # W3 Cov: b1, S2x
b1 * Sx / Sy                            # W4 Cor: b1, Sx, Sy
#D cov(x, y)    cor(x, y)


## ======================================================================================================
## B. SUMS OF SQUARES AND FIT
## ======================================================================================================

#### >>> [SST]   total sum of squares of y
(n - 1) * S2y                           # W1 n, S2y (VARIANCE)
(n - 1) * Sy^2                          # W2 n, Sy (SD: squared)
SSE / R2                                # W3 SSE, R2
SSE + SSR                               # W4 SSE, SSR
SSR / (1 - R2)                          # W5 SSR, R2
#D sum((y - mean(y))^2)

#### >>> [SST_X] and [SST_j]   total variation of an x
(n - 1) * S2x                           # W1 n, S2x (VARIANCE)
(n - 1) * Sx^2                          # W2 n, Sx (SD: squared)
sigma2 / Var_b                          # W3 SIMPLE: sigma^2, Var(b1hat)
(sigma / se)^2                          # W4 SIMPLE: sigma, sd(b1hat)
(sigma_hat / se)^2                      # W5 SIMPLE: sigma_hat, se(b1hat)
sigma_hat^2 / (se^2 * (1 - R2_j))       # W6 MULTIPLE: sigma_hat, se, R2_j
SSR_j / (1 - R2_j)                      # W7 MULTIPLE: SSR_j, R2_j
#D sum((x - mean(x))^2)     (with NAs use the rows lm used: mf <- model.frame(reg); sum((mf$xj - mean(mf$xj))^2))

#### >>> [SSR]   sum of squared residuals
SST - SSE                               # W1 SST, SSE
(1 - R2) * SST                          # W2 R2, SST
SSE * (1 - R2) / R2                     # W3 SSE, R2
sigma_hat^2 * (n - K - 1)               # W4 sigma_hat, n, K
sigma2_hat * (n - K - 1)                # W5 sigma2_hat, n, K
(se * sqrt(SST_X))^2 * (n - 2)          # W6 SIMPLE: se, SST_X, n
#D sum(reg$residuals^2)     sum((y - yhat)^2)

#### >>> [SSE]   explained sum of squares
SST - SSR                               # W1 SST, SSR
R2 * SST                                # W2 R2, SST
SSR * R2 / (1 - R2)                     # W3 SSR, R2
#D sum((yhat - mean(y))^2)

#### >>> [R2]   R-squared   (percent explained = 100 * R2)
1 - SSR / SST                           # W1 SSR, SST
SSE / SST                               # W2 SSE, SST
SSE / (SSE + SSR)                       # W3 SSE, SSR
Cor^2                                   # W4 SIMPLE regression: Cor
#D summary(reg)$r.squared     cor(y, yhat)^2


## ======================================================================================================
## C. COEFFICIENTS, PREDICTIONS, RESIDUALS
## ======================================================================================================

#### >>> [b1]   slope, simple regression
Cov / S2x                               # W1 Cov, S2x
Cor * Sy / Sx                           # W2 Cor, Sy, Sx
(ybar - b0) / xbar                      # W3 point of means: ybar, b0, xbar
#D cov(x, y) / var(x)     reg$coefficients[[2]]

#### >>> [b0]   intercept, simple regression
ybar - b1 * xbar                        # W1 ybar, b1, xbar
#D reg$coefficients[[1]]

#### >>> [yhat]   predicted value (any number of x's; for log(y) models take exp())
b0 + sum(b * xvec)                      # W1 b0, slopes b, x values xvec
exp(b0 + sum(b * xvec))                 # W2 LOG-LEVEL model: back to y
y0 - 0                                  # W3 (y0 - uhat: fitted = actual - residual; edit the 0 to the residual)
#D predict(reg, newdata = data.frame(x1 = , x2 = ))

#### >>> [uhat]   residual = ACTUAL - FITTED
y0 - (b0 + sum(b * xvec))               # W1 y0, b0, b, xvec
-sum(u_known)                           # W2 the MISSING residual (residuals sum to 0): u_known = the other residuals
#D reg$residuals[i]

#### >>> [ybar]   mean of y (point of means)
b0 + sum(b * xbars)                     # W1 MULTIPLE: b0, b, xbars
b0 + b1 * xbar                          # W2 SIMPLE: b0, b1, xbar
#D mean(y)

#### >>> [change_yhat]   predicted CHANGE in y-hat (intercept never matters)
sum(b * dxvec)                          # W1 level model: slopes b, changes dxvec
100 * sum(b * dxvec)                    # W2 LOG-LEVEL: approx % change in y
100 * (exp(sum(b * dxvec)) - 1)         # W3 LOG-LEVEL: exact % change (only if asked)


## ======================================================================================================
## D. DEGREES OF FREEDOM AND ERROR sd
## ======================================================================================================

#### >>> [df]   degrees of freedom
n - K - 1                               # W1 n, K
n - 2                                   # W2 simple regression
SSR / sigma2_hat                        # W3 SSR, sigma2_hat
#D reg$df.residual

#### >>> [K]   number of slope variables
n - dof - 1                             # W1 n, dof
#D length(coef(reg)) - 1

#### >>> [sigma2_hat]   estimate of the error variance
SSR / (n - K - 1)                       # W1 SSR, n, K
sigma_hat^2                             # W2 sigma_hat
#D summary(reg)$sigma^2

#### >>> [sigma_hat]   standard error of the regression ("Residual standard error")
sqrt(SSR / (n - K - 1))                 # W1 SSR, n, K
sqrt(SSR / (n - 2))                     # W2 simple regression
sqrt(sigma2_hat)                        # W3 sigma2_hat
se * sqrt(SST_X)                        # W4 SIMPLE: se, SST_X
se * sqrt(SST_j * (1 - R2_j))           # W5 MULTIPLE: se, SST_j, R2_j
#D summary(reg)$sigma
## TRAP: sd(reg$residuals) is NOT sigma_hat.

#### >>> [sigma]   TRUE error sd (usually given)
sqrt(sigma2)                            # W1 sigma2
se * sqrt(SST_X)                        # W2 from sd(b1hat), SST_X
sqrt(Var_b * SST_X)                     # W3 from Var(b1hat), SST_X


## ======================================================================================================
## E. VARIANCE AND STANDARD ERROR OF A SLOPE
## ======================================================================================================

#### >>> [Var_b1]   Var(b1hat), SIMPLE regression
sigma2 / SST_X                          # W1 sigma2, SST_X
sigma^2 / SST_X                         # W2 sigma, SST_X
sigma_hat^2 / SST_X                     # W3 sigma_hat, SST_X
se^2                                    # W4 se

#### >>> [sd_b1] and [se_b1]   sd (uses sigma) / standard error (uses sigma_hat) of the SIMPLE slope
sigma / sqrt(SST_X)                     # W1 sigma, SST_X
sigma_hat / sqrt(SST_X)                 # W2 sigma_hat, SST_X
sigma / sqrt((n - 1) * S2x)             # W3 sigma, n, S2x (VARIANCE)
sigma / sqrt((n - 1) * Sx^2)            # W4 sigma, n, Sx (SD: squared)
sigma_hat / sqrt((n - 1) * S2x)         # W5 sigma_hat, n, S2x
sqrt(SSR / (n - 2)) / sqrt(SST_X)       # W6 SSR, n, SST_X
sqrt(Var_b)                             # W7 Var(b1hat)
se * m                                  # W8 WHAT-IF sigma multiplied by m
se / sqrt(kk)                           # W9 WHAT-IF sample (SST_X) kk times as large: divide by sqrt(kk), NOT by kk
#D summary(reg)$coefficients[2, 2]
## BIGGER: larger sigma^2, X bunched (small SST_X), smaller sample.   Not affected by beta1 or beta0.

#### >>> [R2_j]   R-squared from regressing X_j on the OTHER x's (Y NOT included)
1 - SSR_j / SST_j                       # W1 SSR_j, SST_j
1 - 1 / VIF                             # W2 VIF
1 - sigma_hat^2 / (se^2 * SST_j)        # W3 sigma_hat, se, SST_j
#D summary(lm(xj ~ other1 + other2, data = d))$r.squared
## wage ~ educ + exper + tenure, want se(exper):  lm(exper ~ educ + tenure).  NOT lm(wage ~ exper), NOT lm(exper ~ wage).

#### >>> [VIF]   variance inflation factor
1 / (1 - R2_j)                          # W1 R2_j

#### >>> [Var_bj]   Var(bj-hat), MULTIPLE regression
sigma2 / (SST_j * (1 - R2_j))           # W1 sigma2, SST_j, R2_j
sigma_hat^2 / (SST_j * (1 - R2_j))      # W2 sigma_hat, SST_j, R2_j
se^2                                    # W3 se

#### >>> [se_bj]   standard error of bj-hat, MULTIPLE regression  (DIVIDE; always positive)
sigma_hat / sqrt(SST_j * (1 - R2_j))    # W1 sigma_hat, SST_j, R2_j
sqrt(Var_b)                             # W2 Var(bj-hat)
sqrt(SSR / (n - K - 1)) / sqrt(SST_j * (1 - R2_j))   # W3 SSR, n, K, SST_j, R2_j
#D summary(reg)$coefficients["xj", "Std. Error"]
## FULL BY-HAND TEMPLATE from data (replace reg, xj, others):
#D mf <- model.frame(reg); n <- nrow(mf); K <- length(coef(reg)) - 1       # rows lm used (safe with NAs)
#D sigma_hat <- sqrt(sum(reg$residuals^2) / (n - K - 1))
#D SST_j <- sum((mf$xj - mean(mf$xj))^2)
#D R2_j <- summary(lm(xj ~ other1 + other2, data = mf))$r.squared          # Y not included; use this object unrounded
#D sigma_hat / sqrt(SST_j * (1 - R2_j))                                    # compare with summary(reg)$coefficients
## Bigger se: larger sigma_hat, smaller SST_j, larger R2_j.   R2_j -> 1: se grows without bound (multicollinearity).
## Solve backwards (given se): SST_j -> see SST_j W6;  R2_j -> see R2_j W3;  sigma_hat -> see sigma_hat W5.


## ======================================================================================================
## F. OMITTED-VARIABLE BIAS   (x2 omitted)   b1tilde = b1hat + b2hat * delta1
## ======================================================================================================

#### >>> [b1tilde]   simple-regression slope (x2 omitted)
b1hat + b2hat * delta1                  # W1 b1hat, b2hat, delta1
b1hat + bias                            # W2 b1hat, bias
#D lm(y ~ x1)$coefficients[[2]]

#### >>> [b1hat]   multiple-regression slope on x1
b1tilde - b2hat * delta1                # W1 b1tilde, b2hat, delta1   (WATCH SIGNS)
b1tilde - bias                          # W2 b1tilde, bias
#D lm(y ~ x1 + x2)$coefficients[[2]]

#### >>> [b2hat]   multiple-regression slope on the omitted variable
(b1tilde - b1hat) / delta1              # W1 b1tilde, b1hat, delta1
#D lm(y ~ x1 + x2)$coefficients[[3]]

#### >>> [delta1]   slope from regressing omitted x2 on included x1 (sign = sign of Cor(x1, x2))
(b1tilde - b1hat) / b2hat               # W1 b1tilde, b1hat, b2hat
#D lm(x2 ~ x1)$coefficients[[2]]     cov(x1, x2) / var(x1)

#### >>> [bias]   bias of the simple slope
beta2 * delta1                          # W1 beta2, delta1
b1tilde - b1hat                         # W2 b1tilde, b1hat
b2hat * delta1                          # W3 sample size of the bias: b2hat, delta1

#### >>> [direction]   which way is the simple slope biased?
ifelse(beta2 * delta1 > 0, "UPWARD bias", ifelse(beta2 * delta1 < 0, "DOWNWARD bias", "NO bias"))   # beta2, delta1
##                    Corr(x1,x2) > 0        Corr(x1,x2) < 0
##    beta2 > 0       UPWARD (positive)      DOWNWARD (negative)
##    beta2 < 0       DOWNWARD (negative)    UPWARD (positive)
## NO bias when beta2 = 0 (omitted variable does not affect y) OR delta1 = 0 (uncorrelated with x1).
## Say it in words: "x2 affects y (sign of beta2) and moves with/against x1 (sign of delta1), so the simple slope gives x1
##   credit or blame for part of x2's effect."


## ======================================================================================================
## G. FUNCTIONAL FORM AND UNITS
## ======================================================================================================

#### >>> [pct_change_y]   "by about what PERCENT does y change?"   (a FALL in x is NEGATIVE)
100 * b1 * dx                           # W1 LOG-LEVEL: b1, dx (units of x)
100 * (exp(b1 * dx) - 1)                # W2 LOG-LEVEL exact (only if asked)
b1 * pct_dx                             # W3 LOG-LOG: b1, pct_dx (% change in x)
#D 100 * reg$coefficients[[2]] * dx
## slope per MONTH but x changes by a year: dx = 12.

#### >>> [change_y] and [elasticity]   "by how many UNITS does y change?"
b1 * dx                                 # W1 LEVEL-LEVEL
(b1 / 100) * pct_dx                     # W2 LEVEL-LOG: b1, pct_dx
## log-log slope b1 = the ELASTICITY (% change in y for a 1% change in x).

#### >>> [exp_back]   predicted log(y) back to y
exp(logyhat)                            # W1 logyhat   (do not round first)
log(cy)                                 # W2 log of a rescaling factor

#### >>> [rescale]   y multiplied by cy, x multiplied by cx
b1 * cy / cx                            # W1 new SLOPE (level model)
b0 * cy                                 # W2 new INTERCEPT (x rescaling never changes it)
b0 + log(cy)                            # W3 LOG-LOG: y x cy -> intercept + log(cy), slope same
b0 - b1 * log(cx)                       # W4 LOG-LOG: x x cx -> intercept - b1*log(cx), slope same
b1 / cx                                 # W5 LOG-LEVEL: x x cx -> slope / cx  (months -> years: cx = 1/12)
## R2 and fit never change with units.

#### >>> [pct_vs_pp]   percent change vs percentage-point change
100 * (val_new - val_old) / val_old     # W1 percent change
val_new - val_old                       # W2 percentage-point change


## ======================================================================================================
## H. PROBABILITY
## ======================================================================================================

#### >>> [EX]   expected value
sum(xv * p)                             # W1 table: values xv, probabilities p
a * EX + c0                             # W2 E[aX + c0]

#### >>> [EX2]   E[X^2]
sum(xv^2 * p)                           # W1 table
VarX + EX^2                             # W2 VarX, EX

#### >>> [VarX]   Var(X)
EX2 - EX^2                              # W1 EX2, EX
sum((xv - sum(xv * p))^2 * p)           # W2 table
sdX^2                                   # W3 sdX

#### >>> [sdX]   standard deviation
sqrt(VarX)                              # W1 VarX
abs(a) * sdX                            # W2 sd(aX + c0)

#### >>> [aX+b] and [Var_aXcY]   linear combinations
a * EX + c0                             # E[aX + c0]
a^2 * VarX                              # Var(aX + c0)   (c0 drops out)
abs(a) * sqrt(VarX)                     # sd(aX + c0)
a^2 * VarX + cc^2 * VarY + 2 * a * cc * CovXY    # Var(aX + cc*Y)   ("3X - 3Y": a = 3, cc = -3)
VarX + VarY + 2 * CovXY                 # Var(X + Y)
VarX + VarY - 2 * CovXY                 # Var(X - Y)

#### >>> [Cov_Cor_joint]   Cov and Cor from a JOINT table (weight by p; do NOT use cor() on the rows)
sum(p * xv * yv) - sum(p * xv) * sum(p * yv)                                    # Cov = E[XY] - E[X]E[Y]
(sum(p * xv * yv) - sum(p * xv) * sum(p * yv)) /
  sqrt((sum(p * xv^2) - sum(p * xv)^2) * (sum(p * yv^2) - sum(p * yv)^2))       # Cor = Cov / (sdX * sdY)

#### >>> [group_mean]   sample mean / variance / sd within a group
#D mean(d$y[d$group == 1])     var(d$y[d$educ == 12])     sd(d$y[d$educ == 12])


## ======================================================================================================
## I. OLS PROPERTIES
## ======================================================================================================

#### >>> [resid_properties]   ALWAYS true after OLS with an intercept
##   sum of residuals = 0 ;  sum of x * residual = 0 for EACH x (zero covariance) ;  residuals uncorrelated with the FITTED values ;
##   the line passes through the point of means (ybar = b0 + b1*xbar1 + ...).
##   NOT always true: residuals uncorrelated with Y itself.   OLS has the SMALLEST sum of squared residuals among lines whose residuals sum to 0.
#D sum(reg$residuals)     sum(d$x1 * reg$residuals)     cor(fitted(reg), reg$residuals)

#### >>> [missing_residual]
-sum(u_known)                           # minus the sum of the other residuals


## ======================================================================================================
## J. CONCEPT ANSWERS (select-all / multiple choice)
## ======================================================================================================

#### >>> [MLR3]   no perfect collinearity FAILS when ...
##   one x is an exact linear function of the others (total = A + B; income in dollars AND thousands; x2 = 100 - x1; dummy trap),
##   an x has NO variation (all workers have educ = 12; SLR3), or more variables than observations (K + 1 > n).
##   NOT when x's are only highly (not perfectly) correlated, or for x and x^2 / log(x).  R reports NA for the dropped coefficient.

#### >>> [MLR4]   E[U | x's] = 0 (endogeneity) FAILS when ...
##   an omitted variable affects Y AND is correlated with an x (ability omitted in wage on educ), wrong functional form, measurement error.
##   NOT because of units, a small sample, or correlated x's.

#### >>> [unbiased]
##   Averaged over MANY random samples, b1hat equals beta1 (not every sample; not zero spread).  Needs MLR1-MLR4 only (not homoskedasticity).

#### >>> [BLUE]   Gauss-Markov
##   Under MLR1-MLR5, OLS is the Best Linear Unbiased Estimator: "best" = smallest variance among all LINEAR UNBIASED estimators.

#### >>> [SLR_assumptions]
##   SLR3 fails: all X identical.   SLR4/MLR4 fails: omitted variable correlated with X -> biased.
##   SLR5 (homoskedasticity) fails: error spread grows with X -> sigma^2/SST_X no longer applies, but OLS is STILL unbiased.

#### >>> [variance_bigger]
##   BIGGER: larger sigma^2, X bunched together (smaller SST_X), smaller sample, larger R2_j.   SMALLER: more spread-out X, larger sample.
##   No effect: the true slope, the intercept.   Smaller variance (given unbiased) = estimates land closer to beta1 sample to sample.

#### >>> [R2_j_regression]
##   Regress X_j on ALL the OTHER independent variables (Y NOT in it).  X_j = the variable whose coefficient you want the se of.

#### >>> [why_not_add_all_variables]
##   More variables can raise the variance of the estimates and can even violate MLR3.  Adding a variable does not lower R2.

#### >>> [slope_meanings]
##   multiple-regression slope = change in y-hat from one more unit of x HOLDING THE OTHERS FIXED (ceteris paribus).
##   log-log slope = elasticity (1% in x -> b1 % in y).   level-log b1: 1% in x -> b1/100 units of y.   %dy = 100*b1*dx is LOG-LEVEL.
##   sigma-hat estimates sigma = the sd of the error U (built from the residuals); summary(reg) prints it as "Residual standard error".


## ======================================================================================================
## K. R TRAPS AND SKILLS
## ======================================================================================================

#### >>> [traps]
## - If R prints 1.547e-02 it means 0.01547.   Keep unrounded values; round(x, 2) only at the very end.
## - K counts SLOPE variables only; df = n - K - 1 (simple: n - 2).   SST uses (n - 1) times the VARIANCE (square an sd!).
## - se(bj) = sigma_hat / sqrt(...): DIVIDE, never subtract; always positive.   Use (1 - R2_j), not R2_j.
## - Missing data: lm() silently drops NA rows. Compute n, SST_j, R2_j on  mf <- model.frame(reg);  mean(x, na.rm = TRUE).
## - Residual = ACTUAL - FITTED; a fall in x is a NEGATIVE change; keep the minus sign on negative slopes.
## - b[[1]] is the intercept, b[[2]] the first slope, b[[3]] the second ...  Name each new regression differently (reg2).
## - summary(reg)$r.squared   summary(reg)$coefficients (Estimate | Std. Error | ...)   summary(reg)$sigma   nrow()   mean()   sd()
## - log() is the natural log; exp() undoes it; a variable entered as log(x) must be plugged in as log(value).
