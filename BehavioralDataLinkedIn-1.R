# Using Module 2 Behavioral Data to analyze the LinkedIn data

library(rtweet)
library(data.table)

library(ggplot2)
library(textcat)

library(tm)
library(textstem)

library(RColorBrewer)
library(wordcloud)

library(lda)
library(topicmodels)
library(textcat)

# read the data object 
LinkedIn = readRDS("testTWLinkedIn.RDS")

#Divide text by different languages
language=textcat(LinkedIn$text)
sort(table(language),decreasing=T)
# Select only English reviews
ENGREVIEWs=LinkedIn[language=="english",]

# Trend Analysis: find out the number of tweets over time
# Times Series Analysis
ts_plot(ENGREVIEWs,by="days")
ts_plot(ENGREVIEWs,by="hours") 
tsdata=ts_data(ENGREVIEWs,by="days")

#Are these social media messages effective? 
# use summary statistics
summary(ENGREVIEWs$favorite_count)
# use histogram
hist(ENGREVIEWs$favorite_count,nclass=20)

which (ENGREVIEWs$favorite_count == 16979)
ENGREVIEWs$full_text[c(1)]

# There are different types of social media messages
# Original tweets
# Retweets: broadcast messages from other accounts without changing the content
# Quote Tweets: Add own comments when retweeting the messages from other accounts
# Private replies to someone: do not show up in the followers' feeds
# Self replies to 3M itself: show up in the followers' feeds
temp=ENGREVIEWs
quote<-na.omit(ENGREVIEWs$is_quote_status)

#Analysis for quote tweet
summary(quote)
sum(quote)/(nrow(temp)-1)

#Analysis for retweet
temp$is_retweet <- grepl('^RT', temp$full_text)
summary(temp$is_retweet)
sum(temp$is_retweet)/nrow(temp)
RT=temp[temp$is_retweet,]

# Analysis for all replies (private replies and self replies)
temp$is_reply<-!(is.na(temp$in_reply_to_screen_name))
summary(temp$is_reply)
sum(temp$is_reply)/nrow(temp)
# Analysis of self replies
temp$t1 <- temp$in_reply_to_screen_name
temp$t1[is.na(temp$in_reply_to_screen_name)] = ""
temp$is_self_reply<-temp$t1==temp$in_reply_to_screen_name
summary(temp$is_self_reply)
SELFREPLY=temp[temp$is_self_reply,]
# Analysis on "private replies" replies that do not show in followers' news feeds
temp$is_private=temp$is_reply==1 & temp$is_self_reply==0
summary(temp$is_private)
# Analysis on LinkedIn's "own" messages, not retweets, not quote, not replies
OWNNOTREPLY=temp[temp$is_retweet==0 & temp$is_quote_status==0 & temp$is_reply==0,]
QT=temp[temp$is_quote_status,]
# Which type of messages is more liked?
summary(RT$favorite_count)
summary(QT$favorite_count)
summary(SELFREPLY$favorite_count)
summary(OWNNOTREPLY$favorite_count)

#Analysis on the topic shooting
words <- c("shooting","shooter","Louisville")
temp1<-temp[rowSums(sapply(words, grepl, temp$full_text)) > 0, , drop = FALSE]
temp1$full_text
# search the full_text column and if the text starts with RT, it is retweet. 
temp1$is_retweet <- grepl('^RT', temp1$full_text)
summary(temp1$is_retweet)
sum(temp1$is_retweet)/nrow(temp1)

#real data topic analysis further
tp=ENGREVIEWs$text
docs = VCorpus(VectorSource(tp))

# Preprocess the data:
# Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))
#remove punctuation
docs = tm_map(docs, removePunctuation)
#Strip digits
docs = tm_map(docs, removeNumbers)
#remove stopwords
docs = tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs = tm_map(docs, stripWhitespace)
# lemmatize the words 
# e.g. ran and run will both be converted to run
docs = tm_map(docs, content_transformer(lemmatize_strings))
#convert data into document term matrix format
DTM = DocumentTermMatrix(docs)

# Generate wordcloud for all the documents
# Calculate the frequency of each word
DTMMATRIX=as.matrix(DTM)
WD = sort(colSums(DTMMATRIX),decreasing=TRUE)

# Transform the data to fit with the wordcloud package
WCLOUD=data.table(words=names(WD),freq=WD)

par(mar=c(1,1,1,1))
wordcloud(words = WCLOUD$words, freq = WCLOUD$freq, 
          scale=c(5,1),
          min.freq = 2,
          max.words=200, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

words2 <- c("petition")
temp2<-temp[rowSums(sapply(words2, grepl, temp$full_text)) > 0, , drop = FALSE]
temp2$full_text

# Apply topic models to identify common themes
# Transform the data to fit with the topic model package
input = dtm2ldaformat(DTM)

# set the random seed so results are replicable
set.seed(12345)

# select model parameters to be 3 topics,  select parameters
K=3
N=1000
result = lda.collapsed.gibbs.sampler(
  input$documents,
  K,                # The number of topics.
  input$vocab,
  N, # The number of iteration
  alpha=1/K,          # The Dirichlet hyper parameter for topic proportion
  eta=0.1,            # The Dirichlet hyper parameter for topic multinomial
  compute.log.likelihood=TRUE)

plot(result$log.likelihoods[1,],type="o")

# Get the top words in each cluster. 
# Top words are the characteristics of the relevant topic.
#different topics may have same keywords
TOPIC = top.topic.words(result$topics, 10, by.score=TRUE)
TOPIC

# Count number of topic keywords for each sentence 
T1=t(result$document_sums)
# Calculate the topic assignment for each sentence
topicproportion=T1/rowSums(T1)
topicproportion[1:10,]
colMeans(topicproportion)

#sentiment score
library(sentimentr)
temp3<-ENGREVIEWs$text
MYTEXT=get_sentences(temp3)
SENTENCE.S = sentiment(MYTEXT)
SENTENCE.S = as.data.table(SENTENCE.S)
REVIEW.S=SENTENCE.S[,
                    list(
                      review.sentiment = mean(sentiment)
                    ),
                    by=element_id]
ENGREVIEW.S = cbind(ENGREVIEWs, REVIEW.S)
summary(ENGREVIEW.S$review.sentiment)
hist(ENGREVIEW.S$review.sentiment)



