#------ Calling The Library----------

library(twitteR)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(syuzhet)
library(ggplot2)

#----Assigning authentic keys in diff. objects------------

consumer_key <- "baZvJ0G7PcHnwp98opby7u6UI"
consumer_secret <- "R4bqx3DaPBwl4aHN5Q2MJuNY5mEAFLIXhJZcWYQezEsVWxDbx4"
access_token <- "2991719840-gmqDThywlcQAwPL0bsIUjSJhja6v4VZ4IkvMRmB"
access_secret <- "mdcJwnZqM29pxyqs9TLRkHuMcaIHVxs9N3r60CqPKUGft"

#---------Authorization and connection to twitter---------

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#---------Checking the connection------

searchTwitter('analytics')

#---------Getting the tweets----------

tweets <- userTimeline("@KKRiders", n=200)

#--------Cleaning the tweets----------

n.tweet <- length(tweets)

tweets.df <- twListToDF(tweets)

head(tweets.df)
View(tweets.df)

#---------Removing the https from the document-----------

tweets.df1 <- gsub("http.*","",tweets.df$text)
tweets.df1 <- gsub("http.*","",tweets.df1)
tweets.df1 <- gsub("#.*","",tweets.df1)
tweets.df1 <- gsub("@.*","",tweets.df1)

View(tweets.df1)

#----------Pasting only the text column to an object--------

tweet_text <- paste(tweets.df1, collapse = " ")
View(tweet_text)


#-----------Creating a vector space-------------

tweet_spa <- VectorSource(tweet_text)


#--------Coverting to document------------

tweet_doc <- Corpus(tweet_spa)
View(tweet_doc)

#------Cleaning and transforming the text---------

tweet_doc <- tm_map(tweet_doc, content_transformer(tolower))
tweet_doc <- tm_map(tweet_doc, removePunctuation)
tweet_doc <- tm_map(tweet_doc, removeNumbers)
tweet_doc <- tm_map(tweet_doc, stripWhitespace)
tweet_doc <- tm_map(tweet_doc, removeWords, stopwords('english'))
tweet_doc <- tm_map(tweet_doc, content_transformer(function(s)
  {
  gsub(pattern = '[^a-zA-z0-9\\s]+',
       x = s,
       replacement = " ",
       ignore.case = TRUE,
       perl = TRUE)
}))

abc <- c("overs","comes","brings","wicket","amp")
tweet_doc <- tm_map(tweet_doc,removeWords,abc)

View(tweet_doc)
stopwords()

View(tweet_doc)
head(tweet_doc)
tweet_doc

#-------Converting to document term matrix and then to a structured one---------

tweet_mat <- DocumentTermMatrix(tweet_doc)

tweet_str <- as.matrix(tweet_mat)
View(tweet_str)
tweet_str

#---------Frequency of each word in the object--------

tweet_f <- colSums(tweet_str)
tweet_f <- sort(tweet_f, decreasing = TRUE)
View(tweet_f)

#-------------Designing word cloud--------

tweet_w <- names(tweet_f)
wordcloud(tweet_w[1:300], tweet_f[1:50], random.order = FALSE, random.color = FALSE)
par(bg = "Orange")



#-----------------Sentiment Analytics-----------------------------

#--------------Getting all the sentiments from the object-----------

tweet_senti <- get_nrc_sentiment(tweet_w)
View(tweet_senti)

#--------------Giving each sentiment a value ------------

tweet_value <- get_sentiment(tweet_w)
View(tweet_value)

#------------Segregating the the most positive and negative values--------------

tweet_pos <- tweet_w[tweet_value== max(tweet_value)]

tweet_neg <- tweet_w[tweet_value==min(tweet_value)]


#--------------Categorizing the word------------

tweet_cat <- ifelse(tweet_value>0,"Positive", ifelse(tweet_value<0,"Negative","Neutral"))
head(tweet_cat)
plot(table(tweet_cat))

#--------------Calculating the no.of emotions-----------

tweet_sum <- colSums(tweet_senti)

#--------------Forming tables---------------

tweet_frame <- data.frame(count= tweet_sum, tweet_senti=names(tweet_sum))
View(tweet_frame)

#-----------Ordering the emotions-----------------

tweet_frame$tweet_senti <- factor(tweet_frame$tweet_senti, 
                                  levels = tweet_frame$tweet_senti[order(tweet_frame$count, decreasing = TRUE)])

#--------------Creating the graph------------------

x <- ggplot(tweet_frame, 
       aes(x= tweet_frame$tweet_senti,y= tweet_frame$count,
           fill= tweet_frame$tweet_senti))+
  geom_bar(stat = "Sum")+
  theme_bw()
x + coord_flip()




