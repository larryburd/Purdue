#################################################
# Title:  Unit5Discussion.R
# Author: Laurence Burden
# Date:   02 Feb 2023
#
# Purpose: Investigate claims of Case Study 1
# H.0: mu = .25
# H.a: mu != .25
# alpha < 0.01 || confidence level of 99%
#################################################
library(dplyr)

# file path is split to keep code under 80 char width
folderpath <- "/home/larryburd/Documents/Purdue/MM325/Unit 3/" 
filename <- "Mount_Pleasant_Real_Estate_Data.csv"
filepath <- paste(folderpath, filename, sep = "")

# Retrieve data and remove any rows with NA in Acreage column
homeData <- read.csv(filepath) |>
  filter(!is.na(Acreage))

# Perform two tailed test against the mean being .25 acres
t.test(x = homeData$Acreage, alternative = "two.sided", 
       mu = .25, conf.level = 0.99)

# Retrieve quartiles and mean of the data
summary(homeData$Acreage)

# Plot boxplot
boxplot(x=homeData$Acreage, 
        main = "Real Estate Acreage", ylab = "Acreage")

# Plot histogram
hist(homeData$Acreage, breaks = 5,
     main="Real Estate Acreage", xlab = "Acreage")

# plot Normal Q-Q Plot with "theoretical" line
qqnorm(y = homeData$Acreage, datax = TRUE)
qqline(y = homeData$Acreage, datax = TRUE)
