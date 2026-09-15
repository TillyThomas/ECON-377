
## ECN 377 - Day 6 STARTER  |  Expectation properties, variance, covariance, correlation
## ------------------------------------------------------------------
## One idea powers all of today:  weight each value by its probability, then add.
##      E[g(X)] = sum( g(x) * p )
## Fill each ______ as we go in class, then COMMIT + PUSH.
## ------------------------------------------------------------------

## ===== 1. Properties of expectation:  E[aX + bY] = a*E[X] + b*E[Y] =====
## Example: suppose E[X] = 2 and E[Y] = 3.  Find E[4X + 5Y - 3].
EX <- 2
EY <- 3
4*EX + 5*EY -3           # E[4X + 5Y - 3]    -- use linearity; E[constant] = the constant

## ===== 2. Variance & standard deviation:  Var(X) = E[X^2] - E[X]^2 =====
## A coin in {0, 1}, each face with probability 1/2.
x <- c(0,1)        # the values           -- the two faces
p <- c(0.5,0.5)         # their probabilities   -- must sum to 1
EX  <- sum(x*p)       # E[X]                  -- weight & add:  sum(x * p)
EX2 <- sum(x^2 * p)       # E[X^2]                -- same idea, weight x^2
VarX <- EX2 - EX^2      # Var(X) = E[X^2] - E[X]^2
sdX  <- sqrt(VarX)     # sd(X)                 -- square root of the variance

## ===== 3. CVarX## ===== 3. Covariance:  Cov(X,Y) = E[XY] - E[X]*E[Y] =====
## Two INDEPENDENT coin flips.  The four (x, y) pairs, each with probability 1/4:
xj <- c(0,0,1,1)        # x across pairs (0,0) (0,1) (1,0) (1,1)
yj <- c(0,1,0,1)        # y across those same pairs
pj <- 0.25        # probability of each pair
EXY <- sum((xj*yj)*pj)       # E[XY]                 -- same idea, weight xj*yj
CovXY <- EXY - (sum(xj*0.25)*sum(yj*0.25))    # Cov = E[XY] - E[X]*E[Y]    -- independent, so expect 0

## ===== 4. Correlation:  Cor(X,Y) = Cov(X,Y) / ( sd(X) * sd(Y) ) =====
## Unitless; always between -1 and 1.   (Remember: correlation is NOT causation.)
              # Cor(X, Y)             -- here sd(X) = sd(Y) = 0.5

## ================= YOUR TURN =========================
## X takes values 1, 2, 3 with probabilities 0.2, 0.5, 0.3.
x <- c(1,2,3); p <- c(0.2,0.5,0.3)
EX   <- sum(x*p)      # (a) E[X]
EX2  <- sum(x^2*p)      # (b) E[X^2]
VarX <- sum(EX2 - EX^2) # (c) Var(X) = E[X^2] - E[X]^2
