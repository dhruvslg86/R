install.packages("pacman")
library(pacman)
pacman::p_load(rtweet, ggplot2, dplyr, tidytext, twitteR, stringr, wordcloud, 
               NLP, twitteR, syuzhet, tm, SnowballC, stringi, topicmodels, 
               syuzhet, ROAuth, gridExtra, tidytext, wordcloud2, arules,rtweet,
               twitteR,ROAuth,jsonlite,rjson,tokenizers,tidyverse,plyr,dplyr,
               ggplot2,syuzhet,stringr,arulesViz,rtweet)
tweets_data <- read.csv(file = "/cloud/project/twitter/tweets.csv", header = TRUE, stringsAsFactors = FALSE)
tweets_data2 <- tweets_data
tweets_data2$text <- str_replace_all(tweets_data2$text, "https://t.co/[a-z,A-Z,0-9]*","")
tweets_data2$text <- str_replace_all(tweets_data2$text, "http://t.co/[a-z,A-Z,0-9]*","")
clean_tweet1 <- tweets_data2

clean_tweet1$text = gsub("&amp", "", clean_tweet1$text)
clean_tweet1$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet1$text)
clean_tweet1$text = gsub("@\\w+", "", clean_tweet1$text)
clean_tweet1$text = gsub("[[:punct:]]", "", clean_tweet1$text)
clean_tweet1$text = gsub("[[:digit:]]", "", clean_tweet1$text)
clean_tweet1$text = gsub("http\\w+", "", clean_tweet1$text)
clean_tweet1$text = gsub("[ \t]{2,}", "", clean_tweet1$text)
clean_tweet1$text = gsub("^\\s+|\\s+$", "", clean_tweet1$text) 

clean_tweet1$text <- str_replace_all(clean_tweet1$text," "," ")

clean_tweet1$text <- str_replace(clean_tweet1$text,"RT @[a-z,A-Z]*: ","")

clean_tweet1$text <- str_replace_all(clean_tweet1$text,"#[a-z,A-Z]*","")

clean_tweet1$text <- str_replace_all(clean_tweet1$text,"@[a-z,A-Z]*","")   

wordcloud(clean_tweet1$text, min.freq = 10, colors=brewer.pal(8, "Dark2"), 
          random.color = TRUE, max.words = 500)

n.tweet<-length(clean_tweet1$text)
only_tweet<-clean_tweet1$text
big_data2<- as.data.frame(clean_tweet1)

tranTweetsFile = "valentine_total_data.csv"
Trans <- file(tranTweetsFile)
Tokens <- tokenizers::tokenize_words(clean_tweet1$text[1],
                                     stopwords = stopwords::stopwords("en"),
                                     lowercase = TRUE, strip_punct = TRUE,
                                     strip_numeric = TRUE, simplify = TRUE)
cat(unlist(str_squish(Tokens)), "\n", file = Trans, sep = ",")
close(Trans)
clean_tweet1$text[1]

tranTweetsFile = "valentine_total_data.csv"
Trans <- file(tranTweetsFile, open = "a")
for (i in 1:nrow(tweets_data)) {
  Tokens <- tokenizers::tokenize_words(clean_tweet1$text[i], 
                                       stopwords = stopwords::stopwords("en"), 
                                       lowercase = TRUE, strip_punct = TRUE, 
                                       strip_numeric = TRUE, simplify = TRUE)
  cat(unlist(str_squish(Tokens)), "\n", file = Trans, sep = ",")
}
close(Trans)

library(arules)
Sentiment <- read.transactions("valentine_total_data.csv",
                               rm.duplicates = FALSE,
                               format = "basket",
                               sep = ",",
                               cols = NULL)
inspect(Sentiment)
rules  <- arules:: apriori(Sentiment, parameter = list(support = .007,confidence=.01,minlen=3))
inspect(rules)
SortedRules <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(SortedRules[1:10])

SortedRulesL <- sort(rules, by="lift", decreasing= TRUE)
inspect(SortedRulesL[1:10]) 