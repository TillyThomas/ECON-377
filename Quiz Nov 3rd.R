## Wed quiz November 3rd

# Percentage change in predicted y (log-level MLR)
## log(y) = 2.2 + 0.066x1 - 0.019x2. If x1 rises by 3 and x2 rises by 4. What % does y change?

b1 <- 0.066
b2 <- -0.019
change_x1 <- 3
change_x2 <- 4
100 * (b1 * change_x1 + b2 * change_x2)

# Run regression of log(wage) on educ, exper, tenure. 
## By about what percent does predicted wage change when experience rises by 2 years and tenure rises by 1 year, holding education fixed?

reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
100 * (b[3] * 2 + b[4] * 1)
round(100 * (b[3] * 2 + b[4] * 1), 2)

# Use same regression as above
## What is the predicted hourly wage, in dollars, of a worker with 12 years of education, 10 years of experience and 3 years of tenure?

reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
round(exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3), 2)


# After an OLS multiple regression of Y on X1, ..., XK (with an intercept) and residuals U_hat_i = Y_i - Y_hat_i, which statements are always true? Select all that apply.

# A. The sample average of the residuals is 0.

# B. The sample covariance between each X_j and the residuals is 0.

# C. The sample covariance between Y and the residuals is 0.

# D. The regression passes through the point of sample means: Y_bar = b0 + b1*X1_bar + ... + bK*XK_bar.

# E. Every residual is 0 when the sample is large enough.

# CORRECT ANSWERS: A, B, D


# A multiple regression gives: y_hat = 6.8 + 0.6*x1 - 1.1*x2
# Sample means: x1 = 14.6 and x2 = 3.1
# Calculate sample mean of Y:

y_mean <- 6.8 + (0.6 * 14.6) + (-1.1 * 3.1)
round(y_mean, 2)


# MLR3 - NO PERFECT COLLINEARITY
# Which regressions violate MLR3? So OLS cannot estimate them

# A. colGPA on ACT and hsGPA, positively but not
# perfectly correlated = DOES NOT VIOLATE MLR3

# B. wage on educ and exper, both vary = DOES NOT
# VIOLATE MLR3

# C. voteA on expendA, expendB and totexpend,
# where totexpend = expendA + expendB
# = VIOLATES MLR3 (perfect collinearity)

# D. wage on income in dollars and income in thousands
# = VIOLATES MLR3 (same variable, different units)

# E. 5 independent variables with only 4 observations
# = CANNOT ESTIMATE OLS

# CORRECT ANSWERS: C, D, E



# MLR4 - ZERO CONDITIONAL MEAN
# Which statement makes MLR4 fail... so that x variables are endogenous?

# A. Y measured in thousands instead of dollars
# DOES NOT VIOLATE MLR4

# B. A variable that affects Y and is correlated
# with X1 is left out of the regression.
# VIOLATES MLR4 (OMITTED VARIABLE BIAS)

# C. X1 and X2 are highly, but not perfectly,
# correlated.
# DOES NOT VIOLATE MLR4

# D. The sample has only 50 observations.
# DOES NOT VIOLATE MLR4

# CORRECT ANSWER: B




# MULTIPLE REGRESSION SLOPE: b1tilde = b1hat + b2hat x sigma1
# Given:
# b1_tilde = 0.85
# Multiple regression coefficient b2 = 1.27
# Slope from regressing X2 on X1 = -0.16

beta1 <- 0.85 - (1.27 * (-0.16))
round(beta1, 2)



# OMITTED VARIABLE BIAS

# Regress faminc on cigs. Compute b2 * sigma1

# Step 1: Regress faminc on cigs to get delta1:
reg1 <- lm(faminc ~ cigs, data = bwght)

# Step 2: Run multiple regression:
reg2 <- lm(bwght ~ cigs + faminc, data = bwght)

# Step 3: Extract faminc coefficient (beta2):
beta2 <- reg2$coefficients[3]

# Step 4: Extract slope from faminc on cigs (delta1):
delta1 <- reg1$coefficients[2]

# Step 5: Multiply beta2 by delta1:
round(beta2 * delta1, 2)

# CHECK ANSWER:
# Simple regression cigs slope - Multiple regression cigs slope
simple_reg <- lm(bwght ~ cigs, data = bwght)
round(simple_reg$coefficients[2] - reg2$coefficients[2], 2)


# OMITTED VARIABLE BIAS
# An omitted variable X2 lowers Y (beta2 < 0), and X2 is negatively correlated with the included X1.
# The simple-regression slope on X1 is:

# Formula:
# Bias = beta2 * delta1
# beta2 is NEGATIVE
# delta1 is NEGATIVE
# NEGATIVE * NEGATIVE = POSITIVE
# Therefore, the simple regression slope is BIASED UPWARD.
# CORRECT ANSWER: C



# OMITTED VARIABLE BIAS
# Researcher regresses crop yield on fertilizer, leaving out land quality.

# Better land raises yield = POSITIVE beta2
# Better land is correlated with more fertilizer = POSITIVE delta1

# Formula:
# Bias = beta2 * delta1
# POSITIVE * POSITIVE = POSITIVE BIAS
# The simple-regression fertilizer slope OVERSTATES the effect of fertilizer.

# CORRECT ANSWER: A


