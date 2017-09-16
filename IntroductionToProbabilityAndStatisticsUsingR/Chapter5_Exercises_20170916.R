library(distrEx)
# mean and variance
x <- c(0, 1, 2, 3)
X <- DiscreteDistribution(supp = x, prob = c(1, 3, 3, 1)/8)
E(X); var(X); sd(X)

# uniform distribution (choose 27 random numbers between 30 and 70)
sample(30:70, size = 27, replace = TRUE)

# all functions are d - PDF, p - CDF, q - quantile, r - random variable 
library(distr)
# binomial distribution: size 3 coins flipped 
X <- Binom(size = 3, prob = 1/2)
plot(X)
# PDF P(X = 1) probability of 1 heads
dbinom(1, size = 3, prob = 1/2)
# CDF P(1 <= X <= 2)
diff(pbinom(c(0,2), size = 3, prob = 1/2))

# hypergeometric distribution: known events m from sample n choosing k
X <- Hyper(m = 17, n = 233, k = 5)
plot(X)
# PDF P(X = 3)
dhyper(3, m = 17, n = 233, k = 5)
# CDF P(X <= 2)
phyper(2, m = 17, n = 233, k = 5, lower.tail = TRUE)

# geometric distribution: time until event occurs
X <- Geom(prob = 0.812)
plot(X)
# PDF (P = 1)
dgeom(1, prob = 0.812)
# CDF (P >= 5) event not occuring 5 times in a row
pgeom(4, prob = 0.812, lower.tail = FALSE)

# poission distribution: events in an interval of time
X <- Pois(lambda = 2)
plot(X)
# PDF P(X = 1)
dpois(1, lambda = 2)
# CDF P(48 <= X <= 50)
diff(ppois(c(47, 50), lambda = 50))