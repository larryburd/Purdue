############################################################################
# Title:  Unit6Discussion.R
# Author: Laurence Burden
# Date:   10 Feb 2023
#
# Purpose: Perform ANOVA test of question 51 and
#          question 262 of WVS data
# H.0: mu.1 = mu.2 = mu.3 = mu.4
# H.a: not all means are equal
############################################################################

# load tidyverse to make manipulating data easier
library(tidyverse)

# Load World Values Survey data
# Break file path apart to stay under 80 chars
folderPath <- "/home/larryburd/Documents/"
fileName <- "WVS_Cross-National_Wave_7_rData_v5_0.rdata"
filePath <- paste(folderPath, fileName, sep = "")
load(filePath)

# Remove responses at 0 or lower to question 51
# and question 262
WVS <- `WVS_Cross-National_Wave_7_v5_0` |>
  filter(Q51 > 0 & Q262 > 0)

# Response levels summaries
for (n in 1:4) {
  # Filter for response level n
  rLvl <- WVS |>
    filter(Q51 == n)
  
  # Get all ages for this level and summary stats
  rAges <- rLvl$Q262
  aCount <- length(rAges)
  aSum <- sum(rAges)
  aMu <- mean(rAges)
  aVar <- var(rAges)
  
  # put it all in a data frame to make a prettier print
  df <- data.frame(Response_lvl = n, Count = aCount, 
                   Sum = aSum, Mean = aMu, Variance = aVar)
  
  # Print summary stats
  print(df)
}

# Perform ANOVA test against questions 262 (age) and
# question 50 (Frequency of going without food in last 12 months)
# Age (Q262) is our continuous/dependent variable and question response (Q51)
# is the categorical/independent variable
anovaResult <- aov(Q262 ~ Q51, data = WVS)

# Display results
summary(anovaResult)
