
## ECN 377 - Day 12 STARTER  |  R^2;  units of measurement
## ------------------------------------------------------------------
## ------------------------------------------------------------------

library(wooldridge)

## ---- SST = SSE + SSR, and R^2   (bwght ~ cigs) ----
data("bwght")
reg2 <- lm(bwght ~ cigs , data = bwght)               # regress bwght on cigs
SST <- sum((bwght$bwght - mean(bwght$bwght))^2)       # total variation:   squared deviations of bwght from its mean, summed
SSR <- sum(residuals(reg2)^2)                         # unexplained:       squared residuals of reg2, summed
SSE <- SST -SSR                                       # explained:         SST - SSR
R2  <- SSE / SST                                      # R^2 = SSE / SST    (~ 0.02: low is normal)

## ---- Units of measurement   (Example 2.3: salary on roe, salary in $1000s) ----
data("ceosal1")
reg3 <- lm(salary ~ roe , data = ceosal1)           # regress salary on roe                         (963.19 and 18.50)
ceosal1$salarydol <- 1000 * ceosal1$salary          # salary in DOLLARS:  Y x 1000          (hint: 1000 * the salary column)
lm(salarydol ~ roe, data = ceosal1)$coefficients                # regress salarydol on roe: both estimates x 1000?  (look at $coefficients)
ceosal1$roedec <- ceosal1$roe / 100      # roe as a DECIMAL:   X x 1/100         (hint: the roe column / 100)
lm(salary ~ roedec, data = ceosal1)$coefficients                # regress salary on roedec: slope x 100, intercept unchanged?
summary(reg3)$r.squared                # R^2 of reg3 -- same for all three regressions  (hint: summary(...)$r.squared)

## ================= PROBLEMS (your turn) =========================
## A regression has SST = 200 and SSR = 150.
SST0 <- 200
SSR0 <- 150
SSE0 <- SST0 - SSR0   # (a) explained sum of squares
R20  <- SEE0 / SST0   # (b) R^2
unex <- SSR / SST   # (c) fraction of the variation UNEXPLAINED
