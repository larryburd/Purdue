library(tidyverse)

# Load WVS file
folderPath <- "/home/larryburd/Documents/"
fileName <- "WVS_Cross-National_Wave_7_rData_v5_0.rdata"
filePath <- paste(folderPath, fileName, sep = "")
load(filePath)

# Remove responses at 0 or lower to question 50
# and question 262
WVS <- `WVS_Cross-National_Wave_7_v5_0` |>
  filter(Q50 > 0 & Q262 > 0)

# Perform ANOVA test against questions 50 and 262
anovaResult <- aov(Q50 ~ Q262, data = WVS)

anovaResult

summary(anovaResult)


# vector to hold response levels
ageMeans <- c()

# Add the mean of each age for each response level to the vector
for (x in 1:10) {
  # Filter question 50 data to just current level
  temp <- WVS |>
    filter(Q50 == x)
  
  # Get the age mean for the current level
  currentMean <- mean(temp$Q262)

  # Add to the overall vector
  ageMeans <- c(ageMeans, currentMean)
}

ageMeans

