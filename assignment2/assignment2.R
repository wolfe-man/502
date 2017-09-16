# 1
# a P(Star)
0.10
# P(Star|Winning Record)
S <- c(0.1, 0.9)
W <- c(0.75, 0.5)
and <- S*W^8
and / sum(and)
# 2
# a
X <- Binom(size = 10, prob = .1)
E(X); var(x)
# b
pbinom(1, size = 10, prob = .1, lower.tail = FALSE)
# 3
X <- Binom(size = 40, prob = 0.05)
# a 
E(X)
# b 
var(X)
# c
pbinom(0, size = 10, prob = .1, lower.tail = FALSE)
# d
pnorm(1, 2, 1.9, lower.tail = FALSE)
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
(.4*var(em) + .6*var(us) - 2*.4*var(us)*var(em))^.5
# c
pnorm(0, mean = (.4*E(em) + .6*E(us)), 
      sd = (.4*var(em) + .6*var(us) - 2*.4*var(us)*var(em))^.5)
# 6
# a
dgeom(1, prob = 0.05)
# b
dgeom(2, prob = 0.05)
# c
dgeom(3, prob = 0.05)
# d
pgeom(3, prob = 0.05, lower.tail = FALSE)
# 7
# a
dhyper(2, m = 3, n = 100, k = 20)
# b
phyper(1, m = 3, n = 100, k = 20)
# c
phyper(1, m = 3, n = 100, k = 20, lower.tail = FALSE)
# d
# about 10% of the time there are more than 2 errors, NOT GOOD!
