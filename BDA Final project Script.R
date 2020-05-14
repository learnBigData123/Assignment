##install rtweet from CRAN
install.packages("rtweet")
# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)



#whatever name assigned to your created app
appname <- "Shaina.Twitter"
## api key (example below is not a real key)
key <- "AhrS1gAasKdESQk7pERXYHu3N"
## api secret (example below is not a real key)
secret <- "eyeyN2L5XSGxKPOHSi6sIAxmIAd3Ag43bMrRh7VCbpgH4to77B"

access_token <- "93580603-x5mQ1pJ8MTdjOubr9H3f2Ikpl0SwsR2IkFDOSzeZp"
access_secret <- "byZnxuwvdwndC0tsoBc5b3riDWCW46NLdEmUKEknyCl4b"


# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

## check to see if the token is loaded
library(rtweet)
get_token()

## search for 18000 documents (tweets) using "stock trading"
finance_tweets <- search_tweets(q = "Apple", n = 50000)
# view the first 3 rows of the dataframe
head(finance_tweets, n = 3)

## plot time series of tweets
finance_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Stock Market Twitter statuses from past 7 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")

## search for 500 tweets using the #finance hashtag
#Ivory_tweets <- search_tweets(q = "@jashainaaa",n = 500)
# view the first 3 rows of the dataframe
#head(Ivory_tweets, n = 3)


#Import text as lines
#text <- training_1600000_processed_noemoticon
#head(text)

#text <- text[,-(1:5)]
#head(text)

#text <- readLines("~/Downloads/Combined_News_DJIA.csv")
#head(text)

#writeLines(finance_tweets$text, "Finance_tweets.txt")


# remove urls tidyverse is failing here for some reason
# finance_tweets %>%
#  mutate_at(c("stripped_text"), gsub("http.*","",.))

# remove http elements manually
finance_tweets$stripped_text <- gsub("http.*","",  finance_tweets$text)
finance_tweets$stripped_text <- gsub("https.*","", finance_tweets$stripped_text)
#finance_tweets$stripped_text 

text <- finance_tweets$stripped_text
head(text)

#In order to turn it into a tidy text dataset, we first need to put it into a data frame.
library(dplyr)
text_df <- tibble(line = 1:length(text), text = text)
head(text_df)

#We can’t filter out words or count which occur most frequently, since each row is made up of multiple combined words. 
#We need to convert this so that it has one-token-per-document-per-row.
library(tidytext)
text_df <- text_df %>%
  unnest_tokens(word, text)
head(text_df)


#words that are not useful for an analysis, typically extremely common words such as “the”, “of”, “to”, 
# We can remove stop words
data(stop_words)
text_df <- text_df %>%
  anti_join(stop_words)
head(text_df)

#use dplyr’s count() to find the most common words
# plot the top 20 words -- notice any issues?
text_df %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

text_df <- text_df %>%
  add_count(word,sort = FALSE)
text_df

#Find top paired words that occur together in tweets
#install_github("dgrtwo/widyr")
#library(widyr)
# remove punctuation, convert to lowercase, add id for each tweet!
finance_tweets_paired_words <- finance_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

finance_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#Plot top paired words that occur together in tweets
#tidy dataframe before plotting
library(tidyr)
finance_tweets_separated_words <- finance_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

finance_tweets_filtered <- finance_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


finance_words_counts <- finance_tweets_filtered %>%  # newcounts:
  count(word1, word2, sort = TRUE)
head(finance_words_counts)



library(igraph)
library(ggraph)

# plot finance word network for words that occur in pairs at least 20 times
# (plotting graph edges is currently broken)
finance_words_counts %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = n, edge_width = n))
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets about finance",
       subtitle = "Text mining twitter data ",
       x = "", y = "")



#cast to document term matrix for LDA model
#100% sparse (100% of document-word pairs are zero)
text_df_dtm <- text_df %>%
  cast_dtm(line, word, n)
text_df_dtm


library(tm)
#access the terms (words) in the document with the Terms() function
terms <- Terms(text_df_dtm)
head(terms)


#create a 5-topic LDA model.A LDA_VEM topic model with 5 topics.
# set a seed so that the output of the model is predictable
text_lda <- LDA(text_df_dtm, k = 5, control = list(seed = 1))
text_lda


#rest of the analysis will involve exploring/interpreting the model using tidying functions from the tidytext package.
#tidytext package provides this method for extracting the per-topic-per-word probabilities, called  β
library(tidytext)

#term “focus” has 0.000139 probability of being generated from topic 1, 0.000130 probability of being from topic 2 
text_topics <- tidy(text_lda, matrix = "beta")
text_topics

#plot top 15 words that are most common within each topic
text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, beta)


text_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


#the per-document-per-topic probabilities, called γ “gamma”
#estimated proportion of words from that document that are generated from that topic document
#each document is a tweet in this case and there are so many tweets so many probabilities can be pretty small
#not really helpful in this case
text_documents <- tidy(text_lda, matrix = "gamma")
text_documents


####SENTIMENT ANALYSIS BASED ON TOPICS TOP TERMS (WORDS) IN EACH OF 5 TOPICS
tt <- text_top_terms$term
top_terms <- text_df %>%
  filter(word %in% tt)%>%
  count(word, sort = TRUE)
top_terms

# join sentiment classification to the tweet words
bing_word_counts <- top_terms %>%
  inner_join(get_sentiments("bing")) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment of Stock Market related Tweets during past 7 days",
       y = "Word Contribution to sentiment",
       x = NULL) +
  coord_flip()

bing_word_counts
  