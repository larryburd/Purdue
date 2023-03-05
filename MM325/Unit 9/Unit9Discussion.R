#####################################################################
# title: Unit9Discussion.R
# Author: Laurence Burden
# Date: 20230304
# Purpose: Create text analysis techniques
#
# Note: must run install.packages("textdata") and 
#       accept NRC's terms to fully run this script
#
# Citation: Sentiment Analysis: Automatically Detecting Valence, 
#             Emotions, and Other Affectual States from Text. Saif 
#             M. Mohammad, arXiv:2005.11882, Jan 2021.
#####################################################################
# Load libraries for storing and analyzing data
library(tidyverse)
library(tidytext)
library(reshape2)
library(readr)

# Global to hold plot
articleBarGraph <- NULL

main <- function() {
  # Get text from file
  filePath <- paste("/Users/laure/OneDrive/Documents",
                    "/Purdue/Purdue/MM325/Unit 9/",
                    "MM325_U9_Text1.txt",
                    sep = "")
  messyText <- read_delim(filePath, delim = "\r\n")
  messyText <- read_file(filePath)
  messyText <- messyText[1]
  messyText
  # Transform data into tidy text format
  tidyArticles <- messyText |>
    unnest_tokens(word, text)
  
  # Load "stop word" to remove
  data("stop_words")
  
  # Remove words such as "the", "of", "to", etc
  tidyArticles <- tidyArticles |>
    anti_join(stop_words)
  
  # Create plot and add them to the global variables
  articleBarGraph <<- plotSentimentsBarGraph(tidyArticles, articleTitles) 
}

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

# Function to plot line graph of NCR sentiments for given tidy text vector
# (tArticles) and list of titles (titles)
plotSentimentsLineGraph <- function(articles) {
  # Sentiment values for NRC lexicon
  senTypes <- c("trust", "fear", "negative", "sadness", "anger",
                "surprise", "positive", "disgust", "joy", "anticipation")
  # initialize an empty vector to hold all sentiment counts
  senCounts <- c()
  
  # Get word count for each sentiment and add it to sentiment count vector
  for (sen in senTypes) {
    wordCount <- getSentimentCount("nrc", sen, articles)
    senCounts <- c(senCounts, sum(wordCount$n))
  }
  
  
  # Gather all data together in a tibble. Each sentiment has 4 counts, one 
  # for each article
  artSentiments <- tibble(negative = senCounts[9:12],
                          sadness = senCounts[13:16], anger = senCounts[17:20],
                          disgust = senCounts[29:32], fear = senCounts[5:8],
                          positive = senCounts[25:28], trust = senCounts[1:4],
                          surprise = senCounts[21:24], joy = senCounts[33:36],
                          anticiptation = senCounts[37:40]) 
  
  # melt data down to create named variables and value counts for plotting
  #meltedData <- melt(artSentiments, id.vars = "title")
  

  # Bar chart showing positive and negative word counts per article
  ggplot(data = artSentiments, aes(y = value, fill = variable)) +
    geom_bar(stat="identity", position = "dodge") +
    labs(title = "Sentiments for Student Debt Articles", 
         x = "Sentiments", y = "Word Count")
}

# Run program and display plot
main()
articleBarGraph