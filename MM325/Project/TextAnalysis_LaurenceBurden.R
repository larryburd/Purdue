#####################################################################
# title: Unit5Assignment_LaurenceBurden.R
# Author: Laurence Burden
#
# Purpose: Create 3 unique text analysis techniques
#          on four different articles
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
library(wordcloud)
library(RColorBrewer)

# Globals to hold plots
articleBarGraph <- NULL
articleLineGraph <- NULL


main <- function() {
  # Get text from file
  filePath <- paste("/Users/laure/OneDrive/Documents",
                    "/Purdue/Purdue/MM325/Project/",
                    "MM325_Unit3_Project_LaurenceBurden.txt",
                    sep = "")
  messyText <- read.delim(filePath, header = TRUE, sep = ":")
  
  # Transform text to a tibble
  articles <- tibble(title = messyText$Title, 
                     text = messyText$Text, date = messyText$Date)
  
  # Get article titles for easier analysis later
  articleTitles <- articles$title
  
  # Transform data into tidy text format
  tidyArticles <- articles |>
    unnest_tokens(word, text)
  
  # Load "stop word" to remove
  data("stop_words")
  
  # Remove words such as "the", "of", "to", etc
  tidyArticles <- tidyArticles |>
    anti_join(stop_words)
  
  # Create plots and add them to the global variables
  # Word cloud automatically prints to the plotting device
  createTidyWordcloud(tidyArticles)
  articleBarGraph <<- plotSentimentsBarGraph(tidyArticles, articleTitles)
  articleLineGraph <<- plotSentimentsLineGraph(tidyArticles, articleTitles)  
}

# Function to create and plot a word cloud based on tidytext formatted vector
createTidyWordcloud <- function(tText) {
  # Get counts for each word
  wordCount <- tText |>
    count(word, sort = TRUE)
  
  # Create word cloud
  set.seed(1234) # Picked for reproducibility
  return( wordcloud(words = wordCount$word, freq = wordCount$n,
            min.freq = 1, max.words = 200, random.order = FALSE,
            random.color = FALSE, rot.per = 0.15, 
            colors = brewer.pal(8, "Dark2"), use.r.layout = FALSE))
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

# Function to plot a bar chart showing positive and negative word counts
# for each tidy text article (tArticles) and vector of titles (titles) passed in
plotSentimentsBarGraph <- function(tArticles, titles) {
  # Initialize empty vectors to store word counts
  posCounts <- c()
  negCounts <- c()
  
  # Loop through each article and get positive and negative word count
  for(t in titles) {
    article <- tArticles |>
      filter(title == t)
    
    # Get negative and positive word counts
    negWordCount <- getSentimentCount("bing", "negative", article)
    posWordCount <- getSentimentCount("bing", "positive", article)
    
    # Add the sum total of each sentiment's word count to a vector
    negCounts <- c(negCounts, sum(negWordCount$n))
    posCounts <- c(posCounts, sum(posWordCount$n))
  }
  
  # combine data into a tibble
  artSentiments <- tibble(title = titles,
                          negativeCount = negCounts,
                          positiveCount = posCounts)
  
  # Melt data for easier comparison in bar chart
  meltedData <- melt(artSentiments, id.vars="title")
  
  # Bar chart showing positive and negative word counts per article
  ggplot(data = meltedData, aes( x = title, y = value, fill = variable)) +
    geom_bar(stat="identity", position = "dodge") +
    labs(title = "Sentiments for Student Debt Articles", 
         x = "Articles", y = "Word Count")
}

# Function to plot line graph of NCR sentiments for given tidy text vector
# (tArticles) and list of titles (titles)
plotSentimentsLineGraph <- function(tArticles, titles) {
  # Sentiment values for NRC lexicon
  senTypes <- c("trust", "fear", "negative", "sadness", "anger",
                "surprise", "positive", "disgust", "joy", "anticipation")
  # initialize an empty vector to hold all sentiment counts
  senCounts <- c()
  
  # Loop through each article 
  for (t in titles) {
    article <- tArticles |>
      filter(title == t)
    
    # Get word count for each sentiment and add it to sentiment count vector
    for (sen in senTypes) {
      wordCount <- getSentimentCount("nrc", sen, article)
      senCounts <- c(senCounts, sum(wordCount$n))
    }
  }
  
  # Gather all data together in a tibble. Each sentiment has 4 counts, one 
  # for each article
  artSentiments <- tibble(title = titles, negative = senCounts[9:12],
                          sadness = senCounts[13:16], anger = senCounts[17:20],
                          disgust = senCounts[29:32], fear = senCounts[5:8],
                          positive = senCounts[25:28], trust = senCounts[1:4],
                          surprise = senCounts[21:24], joy = senCounts[33:36],
                          anticiptation = senCounts[37:40]) 
  
  # melt data down to create named variables and value counts for plotting
  meltedData <- melt(artSentiments, id.vars = "title")
  
  # Return the completed line graph
  ggplot(meltedData, aes(x = variable, y = value, group = title, color = title)) +
    geom_line() + geom_point() + theme(legend.position = "bottom") +
    labs(title = "NCR Senitment Analysis for Student Debt Articles", 
         y = "Word Count", x = "Sentiment")
}

# Run program and display plots
main()
articleBarGraph
articleLineGraph
