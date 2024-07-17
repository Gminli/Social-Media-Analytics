ds=read.csv("Amazon Reviews_Beauty_Product_Line.csv")
# Install the sentiment analysis package
install.packages("sentimentr")
library(sentimentr)

# Install the textcat package. 
# It can be used to select English reviews
install.packages("textcat")
library(textcat)
library(data.table)
# Install the sentiment analysis package
install.packages("sentimentr")
library(sentimentr)


# Install packages for the diffusion model, it requires many other packages. So please be patient. 
install.packages("devtools")
devtools::install_github("mamut86/diffusion")

library(diffusion)
#i descriptive data
#1
ct=table(ds$asin)
sort(ct,decreasing=T)

#2
uniq_products = ds[!duplicated(ds$asin),]
nrow(uniq_products)

#ii
#1
# Select only English reviews
reviews = fread("Amazon Reviews_Beauty_Product_Line.csv")
DS=reviews[reviews$asin=='B0031NNE56',]
# Detect the language used in the reviews using textcat package
#predictive model always come with errors
language=textcat(DS$reviewText)
sort(table(language),decreasing=T)
# Select only English reviews
ENGREVIEWs=DS[language=="english",]
#2
# Set the English reviews to temp
temp=ENGREVIEWs$reviewText
temp[1]
# Decompose the reviews at the sentence level.
# The sentiment Analysis produces the sentiment scores for each sentence.
MYTEXT=get_sentences(temp)
MYTEXT[[1]]
# Generate the sentiment scores for each sentence
SENTENCE.S = sentiment(MYTEXT)
SENTENCE.S = as.data.table(SENTENCE.S)
# Create the sentiment score for each review 
# Average the sentiment scores for all sentences in this review 
REVIEW.S=SENTENCE.S[,
                    list(
                      review.sentiment = mean(sentiment)
                    ),
                    by=element_id]
# Combine the sentiment score with the original data
ENGREVIEW.S = cbind(ENGREVIEWs, REVIEW.S)
# Analyze the sentiment score 
summary(ENGREVIEW.S$review.sentiment)
hist(ENGREVIEW.S$review.sentiment)
#3
# Use correlation test to evaluate the relationship between sentiment scores and review ratings
cor.test(ENGREVIEW.S$review.sentiment, as.numeric(ENGREVIEW.S$overall))
#4
# Treat review rating as factor and plot the relationship between sentiment scores and review ratings
plot(ENGREVIEW.S$review.sentiment ~ factor(ENGREVIEW.S$overall), ylim=c(-1,1))
#5
# Categorize the sentiment scores into types
ENGREVIEW.S$senticlass=cut(ENGREVIEW.S$review.sentiment, breaks = c(-Inf,-0.05, 0.05,Inf),labels=c("negative","neutral","positive"))
summary(ENGREVIEW.S$senticlass)



