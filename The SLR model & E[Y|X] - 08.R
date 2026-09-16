## ECN 377 - Day 8 STARTER  |  The SLR model & E[Y|X]
## ------------------------------------------------------------------
## Model:  Y = b0 + b1*X + U,  so  E[Y|X] = b0 + b1*X  (a straight line).
## Today we only EVALUATE the line; estimating b0,b1 from data is Day 9.
## Fill the TODO, then COMMIT + PUSH.
## ------------------------------------------------------------------

## ---- Demo: colGPA as a FUNCTION of hsGPA  (illustrates R functions) ----
## Notes:  E[colGPA | hsGPA] = 1.5 + 0.5*hsGPA
b0 <- 1.5                      # intercept
b1 <- 0.5                      # slope
colgpa <- function(hsgpa) {b0+b1*hsgpa}       # define colGPA as a function of hsGPA
                                  # the line:  b0 + b1*hsgpa

colgpa(3.6)                       # use it: predicted colGPA at hsGPA = 3.6   (-> 3.3)
colgpa(3.6) - colgpa(2.6)                            # how much does colGPA change from hsGPA 2.6 to 3.6?  (use colgpa)
hsGPA <- seq(2, 4, by = 0.1)      # a range of hsGPA values
plot(hsGPA, colgpa(hsGPA), type = "l",
     xlab = "hsGPA", ylab = "E[colGPA | hsGPA]")   # plot the line

## ================= PROBLEMS (your turn) =========================
pred_30 <- function(hsgpa) {b0+b1*3.0} # (a) expected colGPA at hsGPA = 3.0   (hint: b0 + b1*3.0)
## (b) In one sentence (comment): what does E[U | X] = 0 say?
##     ANSWER: this is the zero conditional mean assumption -> meaning that U does not change at any value of x 
## (c) In one sentence (comment): why might it fail for wage = b0 + b1*educ + U?
##     ANSWER: Because a possible 'u' is ability. Ability does not remain the same as education increases. Assumption is unrealistic.
