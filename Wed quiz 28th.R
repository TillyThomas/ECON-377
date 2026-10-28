## Wed 0ctober 28 Quiz Questions 

## A regression gives test score = 12 + 8log(income). What does the 8 mean?
  # A 1% rise in income is associated with a rise of about 0.08 points in the predicted test score.

## Log-level regression gives log(y) = 0.5 + 0.6x
# By what percent does y change when x falls by 1. 
beta1 <- 0.06
change_x <- -1
percent_change <- 100 * beta1 * change_x

## Run the Log-log regression... by what percent does predicted birth weight change when fam inc rises by 20%
data(bwght)
reg <- lm(log(bwght) ~ log(faminc), data = bwght)
b1 <- coef(reg)[2]
  # Family income rises by 20%
percent_change_x <- 20
percent_change_y <- b1 * percent_change_x


## Suppose you could draw 1,000 different random samples from the same population and compute beta_hat_1 in each one. If OLS is unbiased (SLR1-SLR4 hold), which statement is correct?
  # The average of the 1,000 estimates is close to beta_1.

## Under SLR1-SLR5, Var(beta_hat_1) = sigma^2 / SST_X. Holding everything else fixed, which changes would make Var(beta_hat_1) larger? Select all that apply.

  # Correct answers:
  # 1. A larger error variance sigma^2
  # 2. X values bunched more tightly together (a smaller SST_X)
  # 4. A smaller sample (fewer observations with X spread the same way,so a smaller SST_X)


## A simple regression on n = 12 observations has sigma_hat = 6.4,and the sample standard deviation of X is S_X = 1.6.
# Find the standard error se(beta_hat_1), rounded to the hundredths.
n <- 12
sigma_hat <- 6.4
Sx <- 1.6
# Calculate SST_X
SSTx <- (n - 1) * Sx^2
# Calculate standard error of the slope
se_b1 <- sigma_hat / sqrt(SSTx)
se_b1

## Under SLR1-SLR5, a regression has sd(beta_hat_1) = 2.35. If the sample were 4 times as large, with the X values spread out in the same way (so SST_X is also 4 times as large),what would sd(beta_hat_1) be? Round to the hundredths.
sd_b1 <- 2.35
k <- 4
# A fourfold increase in SST_X divides the standard deviation by sqrt(4)
new_sd_b1 <- sd_b1 / sqrt(k)
new_sd_b1

## A multiple regression gives: y_hat = 9 + 2.6*x1 + 1.6*x2
# Find the predicted value y_hat at x1 = 8 and x2 = 16.Round to the hundredths.

b0 <- 9
b1 <- 2.6
b2 <- 1.6
x1 <- 8
x2 <- 16
y_hat <- b0 + b1*x1 + b2*x2
round(y_hat, 2)

## A multiple regression gives: y_hat = 2 + 3.8*x1 - 0.7*x2
# If x1 rises by 1 and x2 rises by 4, by how much does y_hat change? Round to the hundredths.

b1 <- 3.8
b2 <- -0.7
change_x1 <- 1
change_x2 <- 4
change_y <- b1*change_x1 + b2*change_x2
round(change_y, 2)

## Run reg <- lm(wage ~ educ + exper + tenure, data = wage1). What is the predicted hourly wage of a worker with 14 years of education, 10 years of experience and 5 years of tenure?

reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
predicted_wage <- b[1] + b[2]*14 + b[3]*10 + b[4]*5
round(predicted_wage, 2)

## Using the regression of wage on educ, exper and tenure, by how much does predicted hourly wage change when tenure rises by 3 years, holding education and experience fixed?

reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
change_tenure <- 3
change_wage <- b[4] * change_tenure
round(change_wage, 2)











