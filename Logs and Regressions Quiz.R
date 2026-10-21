## Wed quiz - logs regressions 

# In a regression of y on x, the OLS slope is b1 = 0.58. Every value of x is divided by 1000, WHAT IS NEW OLS SLOPE?

beta1 <- 0.58
k <- 1000

new_slope <- beta1 * k
round(new_slope, 2)
#-----------------------

# Log(y) = 1.5 + 1.08x. By what % does y change when x rises by 2?
beta1 <- 0.08
delta_x <- 2
percent_change <- 100 * beta1 * delta_x
round(percent_change, 2)
#-------------------------------

# yhat = 99 + 6.4 log(x). By how many units does y change when x rises by 2%?
beta1 <- 6.4
percent_change_x <- 2
change_y <- (beta1 / 100) * percent_change_x
round(change_y, 2)
#-------------------------

# Log- log regression of CEO salary in firm sales. If salary changes from thousands of dollars to dollars, what happens?
  # b1 doesnt change, b0 changes (multiply by 1000)
#--------------

# Log-log regression gives log y = 1.4 + 0.17 log(x)
  # By about what percent does y change when x falls by 5%?

beta1 <- 0.17
percent_change_x <- -5
percent_change_y <- beta1 * percent_change_x
round(percent_change_y, 2)
#-----------

# DATA CEOSAL1 -- > By about what percent does predicted salary change when return on equity rises by 5 points?
library(wooldridge)
data(ceosal1)
  # Run the log-level regression
model <- lm(log(salary) ~ roe, data = ceosal1)

  # Extract the unrounded slope
b1 <- coef(model)["roe"]

  # Calculate the approximate percent change for a 5-point increase in roe
round(100 * b1 * 5, 2)
#----------------------

#In wage = b0 + b1* educ + u, which of the following is the most likely to contribute omitted-variable bias to the OLS slope on educ?
  # ANSWER = The worker's ability, which raises wages and tends to be higher for people with more education.
#------------------------

# Holding everything else fixed, which change makes var(bhat1) smaller?
  # ANSWER = More spread-out values of X (a larger SSTx ).
#-------------------------

# Under SLR1–SLR5, the error standard deviation is 5. A sample has the five X values x= (4,5,6,7,9) . Find sd(bhat1)
x <- c(4, 5, 6, 7, 9)
sigma <- 5

# Calculate the sum of squared deviations
SST_x <- sum((x - mean(x))^2)

# Calculate the standard deviation of the slope
sd_b1 <- sigma / sqrt(SST_x)
#------------------------------

# A simple regression on n = 31 observations has SSR = 756. Find the estimate of the error variance
SSR <- 756
n <- 31
round(SSR / (n - 2), 2)
#------------------------------

# A simple regression on n = 10 observations has SSR = 462, and SSTx = 12. Find the standard error
SSR <- 462
n <- 10
SSTx <- 12

sigma2_hat <- SSR / (n - 2)
se_b1 <- sqrt(sigma2_hat / SSTx)

round(se_b1, 2)




