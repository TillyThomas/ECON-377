
## ECN 377 - Day 5  |  nonlinear functions (log/exp) + random variables & expectation.

## --- Nonlinear functions: log & exp  (self-contained demo -- run it and explore!) ---
u <- seq(0.5, 5, by = 0.1)      # a range of positive values
plot(u, log(u), type = "l")     # log: diminishing (flattens out)
plot(u, exp(u), type = "l")     # exp: grows ever faster
exp(log(3))                     # log then exp -> back to 3
log(exp(3))                     # exp then log -> back to 3

## --- Expected value:  E[X] = sum of  value * probability  (we fill this in together) ---
## a random variable's values and their probabilities (must sum to 1)
x <- seq(1:6)        # the values -- a colon makes a sequence, e.g. 1:6 for a die
p <- rep(1/6, 6)       # the probabilities -- rep() repeats a value, e.g. rep(1/6, 6)
## E[X] is a weighted average: sum of  x * p
sum(p*x)
## E[X^2] weights the SQUARED values: sum of  x^2 * p
sum(x^2*p)
