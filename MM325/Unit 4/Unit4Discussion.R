library(tidyverse)

# Load stock data
gme <- read.csv("/home/larryburd/Documents/Purdue/MM325/Unit 3/GME.csv", header=TRUE)
fslr <- read.csv("/home/larryburd/Documents/Purdue/MM325/Unit 3/FSLR.csv", header=TRUE)

# Calculate daily Gain/Loss (GL) with Open-Close
gme_GL <- gme$Close- gme$Open
fslr_GL <- fslr$Close - fslr$Open

dfStocks <- data.frame(GME=gme_GL, FSLR=fslr_GL)

# Create and print histogram for GME
gmeHist <- dfStocks |> ggplot(aes(x = GME)) + 
  geom_histogram(color = "cadetblue4", fill = "cadetblue", binwidth = .5) +
  labs(title = "GME Gain Loss Histogram", y = "Number of Days", x = "Gain/Loss Amount in USD") +
  scale_x_continuous(breaks = seq(-4 , 4, .5))

gmeHist

# Create and print histogram for First Solar
fslrHist <- dfStocks |> ggplot(aes(x = FSLR)) +
  geom_histogram(color="cadetblue4", fill="cadetblue", binwidth = 1) +
  labs(title = "FSLR Gain/Loss Histogram", y = "Number of Days", x = "Gain/Loss Amount in USD") +
  scale_x_continuous(breaks = seq(-10, 12, 1))

fslrHist

