
## ECN 377 - Day 3  |  ceteris paribus + percentages.   

## --- Example A.2:  quantity = 120 - 9.8*price + 0.03*income ---
## the two slopes:
b1 <- -9.8       # slope on price    (hint: -9.8)
b2 <- 0.03        # slope on income   (hint:  0.03)
## price up $1, income held fixed  ->  change in quantity?
b1*1              # hint: b1*1 + b2*0
## income DOWN by $100, price fixed  -> change in quantity?
b2*-100
## price DOWN $2 and income UP $300  ->  change in quantity?
              # hint: b1*(-2) + b2*300
b1*(-2) + b2*300
## --- Percentages ---
## turn the proportion 0.250 into a percent:
0.250 * 100              # hint: 0.250 * 100
## percent change when x goes 54 -> 57:
(57 - 54 / 54 * 100)              # hint: (57 - 54) / 54 * 100
## percentage-POINT change 54 -> 57 (just the difference):
57 - 54              # hint: 57 - 54

## ================= PROBLEMS =========================
pct_chg <- (25-20)/20*100   # (a) percent change from $20 to $25   (hint: (25-20)/20*100)
## (b) interest rate 3% -> 5%: percentage-POINT change = 2pp ; percent change = 66.66%   (comment)
# percent point change 
pp_chg <- 5-3
# percent change 
pct_chng2 <- (5-3)/3*100
