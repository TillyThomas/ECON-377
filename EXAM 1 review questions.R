# EXAM 1 REVIEW QUESTIONS 

# Average conditional mean assumption fails because the average of U changes with the independent variable (X)

# Least square estimates -> OLS line 
  ## Answer must have residuals = 0 and minimise SSR 

# Find SD given EX and EX2 
  ## Step 1: find variance 
  ## Step 2: find SD using equation
EX <- 3
EX2 <- 13
var <- EX2 - EX^2
sd <- sqrt(var)
2*(sd)                # Equation was 2X - 7   (in linear equation the 7 is ignored)

# Find variance of equation -> given var(x), var(y) and cov(x,y)
varX <- 7 
varY <- 7
covXY <- -0.8
a <- 3
b <- -3
(a^2 * varX) + (b^2 * varY) + (2 * (a*b) * covXY)      # var(aX + bY)

# X and Y have joint distribution in the table. Find cor(X,Y)
x <- c(1, 5,7,8)
y <- c(3,3,1,3)
p <- c(0.2, 0.3, 0.4, 0.1)
EX <- sum(p * x)
EY <- sum(p * y)
CovXY <- sum(p * x * y) - EX * EY     # Cov = E[XY] - E[X]E[Y]
VarX <- sum(p * x^2) - EX^2
VarY <- sum(p * y^2) - EY^2
CovXY / sqrt(VarX * VarY)               # Cor(X, Y) ANSWER 
CovXY / VarX                            # population regression slope beta1

# For sample x=(...), y=(...) regress y on x. Find residual of 3rd observation
x <- c(1,3,4,5,8)
y <- c(10,5,6,8,5)
b1 <- cov(x,y) / var (x)
b0 <- mean(y) - b1 * mean(x)
yhati <- b0 + b1*4                     # Yhat_i <- bhat0 + bhat1 * x_i  -->  fitted value yhat
uhati <- 6 - yhati                     # residual -> uhati = Yi - Yhati
  
resid(lm(y ~ x))[3]               # simple formula 

# Find OLS last residual -> OLS always adds to zero 
sum <- 2.1 + (-0.9) + (-0.1) + (-0.2)
0 - sum

# Given SSE and R2, fins SSR
SSE <- 128
R2 <- 0.33
SST <- SSE/ R2
SSR <- SST - SSE

# Given 'n' and sample variance, fins SST 
n <- 54
svar <- 8
SST <- svar * (n-1)

# Using wage1 find OLS residual of worker in row 12
data(wage1)
reg <- lm(wage ~ tenure, data = wage1)
reg$residuals[12]

# Given OLS regression line b1, b0 and sample mean x, find sample mean y
b0 <- 9
b1 <- 3.9
xhat <- 11
yhat <- b0 + b1 * xhat






