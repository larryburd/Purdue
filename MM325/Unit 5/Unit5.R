# return pop std dev for given total std dev s and sample number n
xbarSD <- function(s, n) {
  return(s/sqrt(n))
}

# Returns zscore for given mean, standard deviation,
# random variable x, and sample size
xbarZScore <- function(x, mu, s, n) {
  xbarSigma <- xbarSD(s, n)
  numerator <- x - mu
  zScore <- numerator / xbarSigma
  return(zScore)
}

# Returns z score for given sample proportion p.hat, population proportion p, and sample size n
phatZScore <- function(p.hat, p, n) {
  numerator <- p.hat - p
  denominator <- sqrt((p * (1 - p)) / n)
  return(numerator / denominator)
}
xbarZScore(43, 40, 8, 120)
xbarZScore(5.3, 5.1, .81, 12)
pnorm(.86)

75/150
phatZScore(.59, .63, 200)
pnorm(-1.17)
pnorm(2.5, lower.tail = FALSE)
91.0834/13.0833

zScore <- xbarZScore(154, 150, 15, 100)
pnorm(zScore)

sqrt(1.21)
pf(6.96, 2, 6, lower.tail = F)
xbarZScore(4.6, 4.8, .7, 110)
pnorm(-3)

xbarZScore(6.9, 6.7, .5, 6)

# H0: mu = 412
# Ha: mu < 412 | z < -2.365

xbarZScore(427, 431, 20, 26)

