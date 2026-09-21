## ECN 377 - Day 9 STARTER  |  Deriving the OLS estimates
## ------------------------------------------------------------------
## The derivation ends at  beta1-hat = cov(x,y)/var(x),  beta0-hat = ybar - beta1*xbar.
## Fill each ______ as we go, then COMMIT + PUSH.
## ------------------------------------------------------------------

## ---- Demo: estimate the wage-education line, by hand ----
library(wooldridge)
data("wage1")
x <- wage1$educ     # X = education
y <- wage1$wage     # Y = wage
b1 <- cov(x,y) / var(x)        # slope      -- the derived formula:  cov(x, y) / var(x)
b0 <- mean(y) - b1 * mean(x)        # intercept  -- mean(y) - b1 * mean(x)
c(b0 = b0, b1 = b1) # the fitted line  (should be about -0.90 and 0.54)

## ================= YOUR TURN =========================
## Tiny dataset:  x = (1, 2, 3),  y = (2, 2, 5)
x <- c(1, 2, 3)
y <- c(2, 2, 5)
b1    <- cov(x,y) / var(x)   # (a) slope
b0    <- mean(y) - b1 * mean(x)   # (b) intercept
pred4 <- c(b0 + b1 * 4) # (c) predicted y at x = 4
