# Data Analysis using Bass Model to predict new product diffusion patterns
# This dataset contains the daily count of number of tweets containing words "dove", "real", "beauty", and "sketches", during the first 21 days after the release of the ad “dove real beauty sketches”. 


library(rtweet)
library(data.table)
install.packages("ggplot2")
library(ggplot2)
install.packages("devtools")
dove_daily=read.csv("dove_daily.csv")
View(dove_daily)
library(diffusion)
# Q1 Generate the summary statistics for the daily count of number of tweets. Discuss your outputs.
#summary statistics for the daily count of number of tweets
summary(dove_daily)

#Q2 Plot the daily count of number of tweets over time. Discuss your plot. 
# Trend Analysis: find out the number of tweets over time
# Times Series Analysis
par(mar=c(3,3,3,3))
plot(dove_daily$Dove_real_beauty_sketches)

#Q3 Fit a bass model based on the first 8 days of data, plot the output, and describe the plot. 
#select first 8 days of data
temp=dove_daily[1:8, ]
View(temp)
# Apply Bass model based on the first 8 days of data
fit = diffusion(temp$Dove_real_beauty_sketches,type="bass")
# Show the output of the Bass model
fit

# Plot the predicted number of new viewers. 
pred = predict(fit,5)
par(mar=c(3,3,3,3))
plot(pred,cumulative=FALSE)
lines(temp$Dove_real_beauty_sketches)

