## MASTER SCRIPT 

### ============== LINEAR FUNCTIONS AND PERCENTAGES ======================================================

# y = b0 + b1 * x
# change y = b1 * change x

##EXAMPLE:  quantity = 120 - 9.8*price + 0.03*income 
## the two slopes:
b1 <- -9.8       # slope on price    (hint: -9.8)
b2 <- 0.03        # slope on income   (hint:  0.03)
## price up $1, income held fixed  ->  change in quantity?
b1*1              
## income DOWN by $100, price fixed  -> change in quantity?
b2*-100
## price DOWN $2 and income UP $300  ->  change in quantity?
b1*(-2) + b2*300

## --- Percentages ---

## turn the proportion 0.250 into a percent:
0.250 * 100              
## percent change when x goes 54 -> 57:
(57 - 54 / 54 * 100)              
## percentage-POINT change 54 -> 57:
57 - 54              

### ========= NON LINEAR FUNCTIONS + EXPECTATION ==========================================================

# Log and exponential 
u <- seq(0.5, 5, by = 0.1)      # a range of positive values
plot(u, log(u), type = "l")     # log: diminishing (flattens out)
plot(u, exp(u), type = "l")     # exp: grows ever faster
exp(log(3))                     # log then exp -> back to 3
log(exp(3))                     # exp then log -> back to 3

# Expected value:  E[X] = sum of  value * probability
x <- c(5,4,3)
p <- c(0.2, 0.3, 0.5)
EX <- sum (x*p)
## E[X^2] weights the SQUARED values: sum of  x^2 * p
EX2 <- sum(x^2*p)

# ========================================================================================================

### ======== EXPECTATION, VARIANCE, COVARIANCE, CORRELATION ==============================================

# Properties of expectation:  E[aX + bY] = a*E[X] + b*E[Y]
## Example: suppose E[X] = 2 and E[Y] = 3.  Find E[4X + 5Y - 3].
EX <- 2
EY <- 3
4*EX + 5*EY -3           # E[4X + 5Y - 3]    -- use linearity; E[constant] = the constant

# Variance & standard deviation:  Var(X) = E[X^2] - E[X]^2
x <- c(6,6,4)
y <- c(4,8,4)
p <- c(0.2,0.3,0.5)
EX <- sum(x*p)
EX2 <- sum(x^2 * p)
varX <- EX2 - EX^2
sdX  <- sqrt(varX) 
  
# Covariance:  Cov(X,Y) = E[XY] - E[X]*E[Y] 
x <- c(6,6,4)
y <- c(4,8,4)
p <- c(0.2,0.3,0.5)
EXY <- sum((x * y) * p)
CovXY <- EXY - (sum(x*p) * sum(y*p))


# Correlation:  Cor(X,Y) = Cov(X,Y) / ( sd(X) * sd(Y) ) 
## Unitless; always between -1 and 1.   (Remember: correlation is NOT causation.)
x <- c(1,2,3)
y <- c(4,5,6)
CorXY <- cov(x,y) / (sd(x) * sd(y))

# =========================================================================================================

### =================== POPULATION CORRELATION, COND. EXPECATIONS, COND. VARAINCE =========================

# Population correlation from a probability table 

xj <- c(0, 0, 1, 1)          
yj <- c(0, 1, 0, 1)          
pj <- c(0.4, 0.1, 0.1, 0.4)  
EX  <- sum(xj * pj)                # E[X]   = sum(xj * pj)
EY  <- sum(yj * pj)                # E[Y]   = sum(yj * pj)
EXY <- sum(xj * yj * pj)                # E[XY]  = sum(xj * yj * pj)
Cov <- EXY - EX * EY                # Cov(X,Y) = E[XY] - E[X]*E[Y]

# sd of a 0/1 variable: sd = sqrt(E[X^2] - E[X]^2), and X^2 = X here so E[X^2] = E[X]
EX2 <- sum(xj^2 * pj)
EY2 <- sum(yj^2 * pj) 
varx <- EX2 - EX^2
vary <- EY2 - EY^2
sdX <- sqrt(varx)                # sqrt(EX - EX^2)
sdY <- sqrt(vary)                # sqrt(EY - EY^2)
Cor <- Cov / (sdX * sdY)                # Cor = Cov / (sdX * sdY)   -- should come out to 0.6

#Conditional expectation:  E[Y | X = x] = average of Y within the X = x subgroup =====
## Build it in STEPS.  Goal: the average wage among people with exactly 12 years of educ.
is12 <- wage1$educ == 12  
w12  <- wage1$wage[is12]   
mean(w12)           # E[wage | educ = 12] = the average of those wages          (mean(w12))
## Now educ == 16 (college), the same idea in one line:
is16 = wage1$educ == 16           # E[wage | educ = 16]   (mean(wage1$wage[wage1$educ == 16]))
w16 = wage1$wage[is16]
mean(w16)
# The conditional mean RISES with education.

# Special case  E[X | X] = X: condition on X and there's nothing left to average.
mean(wage1$educ[wage1$educ == 12])

# Conditional variance:  Var(Y | X = x) = spread of Y within the X = x subgroup 
# Var(wage | educ = 12)  -- var() of the SAME selected wages: var(w12)
# Var(wage | educ = 16)  -- var(wage1$wage[wage1$educ == 16])

### =================== SIMPLE LINEAR REGRESSION (SLR) =====================================================

# EXAMPLE: colGPA as a FUNCTION of hsGPA  (illustrates R functions) ----
## SLR EQUATION:  E[colGPA | hsGPA] = 1.5 + 0.5*hsGPA
b0 <- 1.5                      # intercept
b1 <- 0.5                      # slope
colgpa <- function(hsgpa) {b0+b1*hsgpa}       # define colGPA as a function of hsGPA
# the line:  b0 + b1*hsgpa

colgpa(3.6)                       # use it: predicted colGPA at hsGPA = 3.6   (-> 3.3)
b1*1                            # how much does colGPA change from hsGPA 2.6 to 3.6?  (use colgpa)
hsGPA <- seq(2, 4, by = 0.1)      # a range of hsGPA values

### ================ SLR -- DERIVING OLS ESTIMATES ===========================================================

# Y = Dependent variable  , X = Independent variable 
# SLR formula: y = b0 + b1 * x + u
# To calc change in y = b1 * change in x + change in u
# b1 = ceteris parabus effect of X on Y
# b0 = y-intercept 
# zero cond. mean assumption: E[U|X] = 0  --> avg. of U doesnt depend on X
# Under SLR model and zero cond. mean; E[Y|X=x] = b0 + b1 * x

# OLS slope estimate bhat1 = cov(x,y) / var(x)
bhat1 <- cov(x,y) / var (x)

# OLS slope estimate bhat0 = mean(y) - bhat1 * mean(x)
bhat0 <- mean(y) - bhat1 * mean(x)

# WORKING EXAMPLE: find OLS line and predict 9 @ x=6
x <- c(5,8,8)
y <- c(5,9,5)
b1 <- cov(x,y) / var (x)
b0 <- mean(y) - b1 * mean(x)
b0 + b1 * 6

# WORKING EXAMPLE: if x rises by b1 how much does y change
(5/10)*b1

### ------- lm(), fitted values, residuals ---------------------------------------------------------------------------------------------------

## example 
data("ceosal1")                               # open specific data set
reg <- lm(salary ~ roe, data = ceosal1)       # fit the OLS line:  lm(wage ~ educ, data = wage1)
b0 <- reg$coefficients[1]                     # intercept:  reg$coefficients[1]
b1 <- reg$coefficients[2]                     # slope:    reg$coefficients[2] 
b0 + b1 * (-15)                               ## prediction @ -15
b1 * -5                                       ## predicted change @ change = -5 

# Yhat_i <- bhat0 + bhat1 * x_i  -->  fitted value yhat
yhati <- 6 + (4/10)*14

# residual -> uhati = Yi - Yhati
uhati <- 19 - yhati

## for the fitted line "" and the three points "" - find SSR

# Fitted line: yhat = -2 + (7/10)*x
x <- c(4, 5, 10)
y <- c(9, 4, 12)
# Calculate fitted values
y_hat <- -2 + (7/10)*10
# Calculate residuals
residuals <- 12 - y_hat
# Sum of squared residuals
SSR <- sum(residuals^2)
# add all SSR values 
67.24 + 6.25 + 49

## ---------- OLS properties 1-3  (hold on ANY sample) ---------------------------------------------------------------------

sum(resids)                     # 1) the residuals sum to 0            -- add up the residuals of reg    formula =(sum of u(hat)i = 0 )
cov(resids, wage1$educ)         # 2) x & residuals are uncorrelated    -- add up educ * (the residuals)  (~ 0)    formula = sum(Xi x U(hat)i = 0 )
b0 <- reg$coefficients[1]       # 3) (xbar, ybar) is ON the line       -- does mean(wage) equal  b0 + b1*mean(educ)?   
b1 <- reg$coefficients[2]
b0 +b1 * mean(wage1$educ) 
mean(wage1$wage).                  ## -> these two numbers are equal - shows property 3 

# Yi > Y(hat)i  --> under prediction 
# Yi < Y(hat)i  --> over prediction 

## ---------- Goodness of fit (R^2) ------------------------------------------------------------------------------------------------

data("bwght")
reg2 <- lm(bwght ~ cigs, data = bwght)                                      # regression line of bwght on cigs
SST <- sum((bwght$bwght - mean(bwght$bwght))^2)                             # total variation
SSR <- sum(residuals(reg2)^2)                                               # unexplained variation
SSE <- SST - SSR                                                            # explained variation
R2 <- SSE / SST                                                             # R-squared = fraction of the sample variation in Y explained by X

# FRACTION of variation in y unexplained = SSR / SST
fracSSR <- SSR / SST

# R2 close to 0 = the OLS line explains little of the variation in Y

# given SST and R2 find SSR
SSR <- ( 1 - R2 ) * SST




