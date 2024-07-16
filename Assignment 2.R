library(rtweet)
library(data.table)
install.packages("ggplot2")
library(ggplot2)
install.packages("devtools")
dove_daily=read.csv("dove_daily.csv")
View(dove_daily)
library(diffusion)
# Q1
#summary statistics for the daily count of number of tweets
summary(dove_daily)

#Q2
# Trend Analysis: find out the number of tweets over time
# Times Series Analysis
par(mar=c(3,3,3,3))
plot(dove_daily$Dove_real_beauty_sketches)

#Q3
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

