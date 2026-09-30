
## ECN 377 - Day 11 STARTER  |  OLS properties 1-3;  SST = SSE + SSR, R^2
## ------------------------------------------------------------------
## ------------------------------------------------------------------

## example reminder
data("ceosal1")
reg <- lm(salary ~ roe, data = ceosal1)
b0 <- reg$coefficients[1]
b1 <- reg$coefficients[2]
b0 + b1 * (-15) ## prediction @ -15
b1 * -5   ## predicted change @ change = -5 

## properties of OLS
library(wooldridge)
data("wage1")
reg <- lm(wage ~ educ, data = wage1)
resids <- reg$residuals

## ---- OLS properties 1-3  (hold on ANY sample) ----
sum(resids)                     # 1) the residuals sum to 0            -- add up the residuals of reg
cov(resids, wage1$educ)         # 2) x & residuals are uncorrelated    -- add up educ * (the residuals)  (~ 0)
b0 <- reg$coefficients[1]       # 3) (xbar, ybar) is ON the line       -- does mean(wage) equal  b0 + b1*mean(educ)?
b1 <- reg$coefficients[2]
b0 +b1 * mean(wage1$educ) 
mean(wage1$wage).                  ## -> these two numbers are equal - shows property 3 

## ---- SST = SSE + SSR, and R^2   (bwght ~ cigs) ----
data("bwght")
reg2 <- lm(bwght ~ cigs, data = bwght)                 # regress bwght on cigs
SST <- sum((bwght$bwght - mean(bwght$bwght))^2)        # total variation:   squared deviations of bwght from its mean, summed
SSR <- sum(residuals(reg2)^2)                          # unexplained:       squared residuals of reg2, summed
SSE <- SST - SSR                                       # explained:         SST - SSR
R2  <- SSE / SST                                       # R^2 = SSE / SST    (~ 0.02: low is normal)

## ================= YOUR TURN =========================
## A regression has SST = 200 and SSR = 150.
SST0 <- 200
SSR0 <- 150
SSE0 <- SST0 - SSR0   # (a) explained sum of squares
R20  <- SSE0 / SST0   # (b) R^2
