# Find probability of x1 and x2 given the mean and standard deviaiton
zBetween <- function(x1, x2, m, s) {
  z1 <- pnorm(x1, mean = m, sd = s, lower.tail = TRUE)
  z2 <- pnorm(x2, mean = m, sd = s, lower.tail = TRUE)
  
  return(z1 - z2)
}

# Find the mode for provided data
getMode <- function(data) {
  uniqv <- unique(data)
  mode <- uniqv[which.max(tabulate(match(data, uniqv)))]
  return(mode)
}

# MM325's way of finding quartiles and IQR
book_quartiles <- function(v) {
  # Sort the provided vector
  sortedValues <- sort(v)
  
  #Get count of values
  numValues <- length(sortedValues)
  
  # Get initial quartile index locations
  q1Loc <- numValues * .25
  q2Loc <- numValues * .5
  q3Loc <- numValues * .75
  
  # Set actual quartile values
  q1 <- getQuartile(q1Loc, sortedValues)
  q2 <- getQuartile(q2Loc, sortedValues)
  q3 <- getQuartile(q3Loc, sortedValues)

  returnFrame <- data.frame("Min"=min(sortedValues),
                            "Q1"=q1, "Q2"=q2, "Q3"=q3,
                            "Max"=max(sortedValues), 
                            "IQR"=q3-q1)
  return(returnFrame)
}


# Get the quartile from the provided values v based on the
# provided index i
getQuartile <- function(i, v) {
  # Find middle value of q1 location and its next neighbor
  # if location is an integer
  if (i %% 1 == 0) {
    return((v[i] + v[i + 1]) / 2)
  } else {
    # if the location is a decimal, then round up and use that
    # as the location
    return(v[ceiling(i)])
  }
}

# Return the Coefficient of Variance (standard deviation / mean) 
# for the provided data
getCV <- function(data) {
  return(sd(data)/mean(data))
}

# find stand deviation given a total proportion p 
# and a given pop size n
phatSD <- function(p, n) {
  th <- p * (1 - p)
  pSD <- sqrt(th/n)
  return(round(pSD, digits = 4))
}

# Takes population probability (phat), overall probability (p),
# and sample size (n) to return zscore
phatZScore <- function(phat, p, n) {
  phSD <- phatSD(p, n)
  zScore <- ((p -  phat) - p) / phSD
  return(round(zScore, digits = 2))
}

# Get sample variance and sample std deviation for given group width w,
# number of groups n, lowest class value l, and frequency vector f
groupedSV <- function(w, n, l, f) {
  groupValues <- c()
  lowerV <- l
  # Calculate group values based on lowest value and number of groups
  for (i in 1:n) {
    upperV <- lowerV + w
    newValue <- (lowerV + upperV) / 2
    groupValues <- append(groupValues, newValue)
    
    lowerV <- upperV + 1
  }
  ymean <- sum(groupValues*f)/sum(f)
  samvar <- sum(f*(groupValues-ymean)^2) / (sum(f) - 1)
  samSD <- sqrt(samvar)
  
  return(data.frame("Mean"=ymean, "Sample Variance"=samvar, "Sample Standard Deviation"=samSD))
}

# return pop std dev for given total std dev s and sample number n
xbarSD <- function(s, n) {
  return(s/sqrt(n))
}

# Returns zscore for given mean, standard deviation,
# variation/difference, and sample size
pxbar <- function(m, s, v, n) {
  xs <- xbarSD(s, n)
  a <- ((m - v) - m)
  return(a / xs)
} 

