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
75/150
phatZScore(.5, .57, 220)
pnorm(-1.32) * 2
pnorm(2.5, lower.tail = FALSE)


zScore <- xbarZScore(154, 150, 15, 100)
pnorm(zScore)

sqrt(1.21)

xbarZScore(4.6, 4.8, .7, 110)
pnorm(-3)

xbarZScore(6.9, 6.7, .5, 6)

# H0: mu = 412
# Ha: mu < 412 | z < -2.365

xbarZScore(427, 431, 20, 26)

