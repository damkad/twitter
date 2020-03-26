install.packages("rtweet")
install.packages("textdata")
library(tidytext)
library(rtweet)
library(tidyverse)
library(textdata)

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


#top 10 words in Nigeria

clean.tweets.Nigeria %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x=word, y=n)) + geom_col() +
  xlab(NULL) + coord_flip() + theme_classic() + labs(x= "Count", y="Unique words", title = "Unique word counts in Nigeria")



#top 10 words in England

clean.tweets.England %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x=word, y=n)) + geom_col() +
  xlab(NULL) + coord_flip() + theme_classic() + labs(x= "Count", y="Unique words", title = "Unique word counts in England")


#performing sentiment analysis using bing lexicon

x <- get_sentiments("bing") %>% filter(sentiment == "negative")

bing_Nigeria <- clean.tweets.Nigeria %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()


bing_Nigeria %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y")+
  labs(title = "Tweets containing Nigeria", y= "Contribution to Sentiment", x=NULL)+
  coord_flip()+theme_bw()


bing_England <- clean.tweets.England %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()


bing_England %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y")+
  labs(title = "Tweets containing England", y= "Contribution to Sentiment", x=NULL)+
  coord_flip()+theme_bw()




#get sentiment for each tweet
sentiment_bing = function(twt){
  twt_tbl = tibble(text = twt) %>% mutate(stripped_text = gsub("http\\s+", "", text)) %>% 
    unnest_tokens(word, stripped_text) %>% anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% 
    mutate(score = case_when(
      sentiment == "negative" ~ n*(-1),
      sentiment == "positive" ~ n*(1)
    ))
  
  sent.score = case_when(
    nrow(twt_tbl)==0 ~ 0,
    nrow(twt_tbl)>0 ~ sum(twt_tbl$score)
  )
  
  zero.type = case_when(
    nrow(twt_tbl)==0 ~ "Type 1",
    nrow(twt_tbl)>0 ~ "Type 2"
  )
  
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
} 

Nigeria_sent = lapply(Nigeria$text, function(x){sentiment_bing(x)})
England_sent = lapply(England$text, function(x){sentiment_bing(x)})
View(Nigeria_sent)





country_sentiment <- bind_rows(
  tibble(country = "#Nigeria",
         score = unlist(map(Nigeria_sent, "score")), 
         type = unlist(map(Nigeria_sent, "type"))
         ),
  tibble(country = "#England",
         score = unlist(map(England_sent, "score")), 
         type = unlist(map(England_sent, "type"))
  )
)
ggplot(country_sentiment, aes(x=score, fill=country)) + 
  geom_histogram(bins = 15, alpha=0.6)+facet_grid(~country) + theme_bw()


x <- clean.tweets.Nigeria$word %>% str_replace_all(c(".*corona.*" = "covid19", "^(19)$" ="covid19", 
                                                ".*covid.*" ="covid19"))






