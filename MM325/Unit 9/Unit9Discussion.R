#####################################################################
# title: Unit9Discussion.R
# Author: Laurence Burden
# Date: 5Mar2023
# Purpose: Utilize text analysis techniques
#
# Note: must run install.packages("textdata") and 
#       accept NRC's terms to fully run this script
#
# Note: Text must be saved without any carriage returns or line feeds
#       to execute properly
#
# Citation: Sentiment Analysis: Automatically Detecting Valence, 
#             Emotions, and Other Affectual States from Text. Saif 
#             M. Mohammad, arXiv:2005.11882, Jan 2021.
#####################################################################
# Load libraries for storing and analyzing data
library(tidyverse)
library(reshape2)
library(tidytext)
library(readr)

# Function to retrieve sentiment analysis based on provided
# lexicon (lex), sentiment (s), and tidytext formatted vector (tText)
getSentimentCount <- function(lex, s, tText) {
  # Load sentiment lexicon and filter the supplied sentiment
  senWords <- get_sentiments(lex) |>
    filter(sentiment == s)
  
  # Count words that match given sentiment
  wordCount <- tText |>
    inner_join(senWords) |>
    count(word, sort = TRUE)
  
  return(wordCount)
}

# Function to plot bar graph of NCR sentiments for given tidy text vector
plotSentimentsBarGraph <- function(articles) {
  # Sentiment values for NRC lexicon
  senTypes <- c("trust", "fear", "negative", "sadness", "anger",
                "surprise", "positive", "disgust", "joy", "anticipation")
  # initialize an empty vector to hold all sentiment counts
  senCounts <- c()
  
  # Tibble to hold all the values of the sentiment counts
  artSentiments <- tibble(negative = "",
                          sadness = "", anger = "",
                          disgust = "", fear = "",
                          positive = "", trust = "",
                          surprise = "", joy = "",
                          anticiptation = "") 
  
  # Get word count for each sentiment and add it to sentiment count tibble
  for (sen in senTypes) {
    wordCount <- getSentimentCount("nrc", sen, articles)
    artSentiments[sen] <- sum(wordCount$n)
  }
  
  # Melt data to enable easier plotting
  meltedData <- melt(artSentiments)
  
  # Bar chart showing sentiment word counts
  ggplot(data = meltedData, aes(x = variable, y = value, fill = variable)) +
    geom_bar(stat="identity", position = "dodge") +
    labs(title = "Sentiment Analysis for Unit 9 Discussion", 
         x = "Sentiment", y = "Word Count")
}


# ****MAIN FUNCTION SECTION****
# Get text from file
winFilePath <- paste("/Users/laure/OneDrive/Documents",
                     "/Purdue/Purdue/MM325/Unit 9/",
                     "MM325_U9_Text1.txt",
                     sep = "")
messyText <- read_file(winFilePath)

# Split the words into individual rows
articleText <- strsplit(messyText, " +")

# Unnest articleText to a tibble for use with TidyText
artTibble <- tibble(line = 1:length(articleText[[1]]), text = articleText[[1]])

# Reformat text into TidyText friendly format
tidyArticles <- artTibble |>
  unnest_tokens(word, text)

# Load "stop word" to remove
data("stop_words")

# Remove words such as "the", "of", "to", etc
tidyArticles <- tidyArticles |>
  anti_join(stop_words)

# Call the sentiment function
plotSentimentsBarGraph(tidyArticles)