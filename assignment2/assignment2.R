# 1
# a P(Star)
# P(Star|Winning Record)
S <- c(0.1, 0.9)
W <- c(0.75, 0.5)
and <- S*W^5
and / sum(and)
# 2
# a
library(distrEx)
X <- Binom(size = 10, prob = .1)
E(X); var(X)
# b
pbinom(1, size = 10, prob = .1, lower.tail = FALSE)
# 3
X <- Binom(size = 40, prob = 0.05)
# a 
E(X)
# b 
var(X)
# c
pbinom(0, size = 40, prob = .05, lower.tail = FALSE)
# d
pnorm(0.5, 2, 1.378, lower.tail = FALSE)
# 4
# a
ppois(2, lambda = 1, lower.tail = FALSE)
# b
ppois(10, 12, lower.tail = FALSE)
# 5
em <- Norm(mean = .1, sd = .15)
us <- Norm(mean = .06, sd = .1)
# a
.4*E(em) + .6*E(us)
# b
(.4^2*var(em) + .6^2*var(us) + 2*.4*.4*.6*.15*.1)^.5
# c
pnorm(0, mean = (.4*E(em) + .6*E(us)), 
      sd = (.4^2*var(em) + .6^2*var(us) + 2*.4*.4*.6*.15*.1)^.5)
# 6
# a
dgeom(0, prob = 0.05)
# b
dgeom(1, prob = 0.05)
# c
dgeom(2, prob = 0.05)
# d
pgeom(2, prob = 0.05, lower.tail = FALSE)
# 7
# a
dhyper(2, m = 3, n = 97, k = 20)
# b
phyper(1, m = 3, n = 97, k = 20)
# c
phyper(1, m = 3, n = 97, k = 20, lower.tail = FALSE)
# d
# about 10% of the time there are more than 2 errors, NOT GOOD!
