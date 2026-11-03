
## ECN 377 - Day 19 STARTER  |  Example 3.1;  omitted-variable bias (3.23)
## ------------------------------------------------------------------
## Type the examples along with class. Fill each ______ and ANSWER in the problems, then upload to GitHub.
## ------------------------------------------------------------------

library(wooldridge)

## ---- Example 3.1 (gpa1) ----
## SLR: colGPA on ACT (save the ACT slope)
reg_SLR <- lm(colGPA ~ ACT, data = gpa1)
summary(reg_SLR)

## MLR: add hsGPA; change in colGPA-hat from +10 ACT, hsGPA fixed
reg_MLR <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
summary(reg_MLR)
b0 <- reg_MLR$coefficients[1]
b1 <- reg_MLR$coefficients[2]
b2 <- reg_MLR$coefficients[3]
change_colgpa <- b1*10
change_colgpa

## the regression plane (first time only, in the Console: install.packages(c("car", "rgl")); Mac: rgl also needs XQuartz)
install.packages(c("carr", "rgl", "XQuartz"))
library(car)
scatter3d(colGPA ~ ACT + hsGPA, data = gpa1, fit = "linear")   # drag to rotate: flat in ACT, steep in hsGPA

## ---- Omitted-variable bias (Example 3.3, gpa1) ----
## delta1: regress hsGPA (omitted) on ACT (included)
reg <- lm(hsGPA ~ ACT, data = gpa1)
delta0 <- reg$coefficients[1]
delta1 <- reg$coefficients[2]
delta1

## check (3.23): SLR slope = MLR slope + (hsGPA slope) * delta1
reg_MLR <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
b1hat <- reg_MLR$coefficients[2]
b2hat <- reg_MLR$coefficients[3]
b1tilde <- reg$coefficients[2]

b1tilde
SLR_slope <- b1hat + b2hat * delta1
SLR_slope

OVB <- b1tilde - SLR_slope
OVB

## ================= PROBLEMS (your turn) =========================
## (a) colGPA-hat = 1.29 + 0.0094*ACT + 0.453*hsGPA
pred_a <- 1.29 + 0.0094*30 + 0.453*3.0    # (a) colGPA at ACT = 30, hsGPA = 3.0   (hint: 1.29 + 0.0094*30 + 0.453*3.0)
## (b) b2 > 0 and delta1 > 0: SLR slope biased up or down?   ANSWER:
## (c) b2 > 0 and delta1 < 0: which way?                     ANSWER:
## (d) When is there NO omitted-variable bias?               ANSWER:
