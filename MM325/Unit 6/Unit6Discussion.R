############################################################################
# Title:  Unit6Discussion.R
# Author: Laurence Burden
# Date:   8 Feb 2023
#
# Purpose: Perform ANOVA test of question 50 and
#          question 262 of WVS data
# H.0: mu.1 = mu.2 = mu.3 = mu.4 = mu.5 = mu.6 = mu.7 = mu.8 = mu.9 = mu.10 
# H.a: not all means are equal
############################################################################

# load tidyverse to make manipulating data easier
library(tidyverse)

# Load World Values Survey data
folderPath <- "/home/larryburd/Documents/"
fileName <- "WVS_Cross-National_Wave_7_rData_v5_0.rdata"
filePath <- paste(folderPath, fileName, sep = "")
load(filePath)

# Remove responses at 0 or lower to question 50
# and question 262
WVS <- `WVS_Cross-National_Wave_7_v5_0` |>
  filter(Q50 > 0 & Q262 > 0)

# Perform ANOVA test against questions 262 (age) and
# question 50 (Satisfaction with financial situation)
anovaResult <- aov(Q262 ~ Q50, data = WVS)

# Display results
summary(anovaResult)
