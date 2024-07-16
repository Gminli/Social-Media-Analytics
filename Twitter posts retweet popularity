#data collection on interested topic with popularity levels

install.packages("rtweet")
install.packages("httpuv")
library(rtweet)
my_bearer_token = 'AAAAAAAAAAAAAAAAAAAAAEIL%2BAAAAAAAugnp2hX6nEE5x%2BdRdeP2VVrDtSM%3DV12utjzI17UT1KOi4n2dWVHaW1DsEKXpEOSb7wM42NisSYuKJ8'
auth <- rtweet_app(my_bearer_token)
auth_as(auth)
#retrieve tweets which mentions “DisneyStudios”
TWATDisney <- search_tweets(
  "DisneyStudios", lang="en",n = 1000, include_rts = TRUE
)
View(TWATDisney)

TWATDisney$screen_name <- users_data(TWATDisney)$screen_name
TWATDisney$followers_count <- users_data(TWATDisney)$followers_count
TWATDisney$friends_count <- users_data(TWATDisney)$friends_count
TWATDisney$account_created_at <- users_data(TWATDisney)$created_at

# Save the data
saveRDS(TWATDisney, "testTWATDisney.RDS")

# install and library new packages to incorporate new functions
# install rtweet package to retrieve and analyze data from twitter
install.packages("rtweet", repos = 'https://ropensci.r-universe.dev/')
library(rtweet)

# install data.table packages for data operation
install.packages("data.table")
library(data.table)

# read the data object
PPEDisney <- readRDS("testTWATDisney.RDS")

# Number of observations
dim(PPEDisney)

# List all the variable names in the data set
names(PPEDisney)
# Use summary statistics to describe the effectiveness of Disney's Social Media Messages 
# favorite_count: number of likes of each message
summary(PPEDisney$favorite_count)
var(PPEDisney$favorite_count)
sd(PPEDisney$favorite_count)

# retweet_count: number of retweets of each message
summary(PPEDisney$retweet_count)



