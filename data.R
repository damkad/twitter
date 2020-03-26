install.packages("rtweet")
library(tidytext)
library(rtweet)
library(tidyverse)

access_token = "74837730-8OkvwiwPzjifT3Kcn4P5rMgIHUG6xzdjlPO2d5jHi"
access_secret = "3uK5fmVUMNvfJNMoa9QlGJ3QgHTOwSdPV971y2NfgZFvr"
consumer_key = "NeCzYgSV1imbiUtUA1zFgov1d"
consumer_secret = "76rUkoQpdxXLSnjAtCDWN9ZG4ABZaNYI3YESz5t3CkEYTtuDON"

create_token(
  app = "birdsSearch",
  consumer_key,
  consumer_secret,
  access_token,
  access_secret,
  set_renv = TRUE
)

Nigeria <- search_tweets("#Nigeria", n=100, include_rts = FALSE)
England <- search_tweets("#England", n=100, include_rts = FALSE)

tweets.Nigeria <- Nigeria %>% select(screen_name, text)

tweets.England <- England %>% select(screen_name, text)


#preprocessing tweets
head(tweets.Nigeria$text)
#all words to lowercase
#remove hyperlinks
#remove punctuations
#tokenize


tweets.Nigeria$stripped_text <- gsub("http\\S+", "", tweets.Nigeria$text)

tweets.England$stripped_text <- gsub("http\\S+", "", tweets.England$text)

stem.tweets.Nigeria <- tweets.Nigeria %>% select(stripped_text) %>% unnest_tokens(word, stripped_text)

stem.tweets.England <- tweets.England %>% select(stripped_text) %>% unnest_tokens(word, stripped_text)

#stemming
clean.tweets.Nigeria <- stem.tweets.Nigeria %>% anti_join(stop_words)

clean.tweets.England <- stem.tweets.England %>% anti_join(stop_words)

















