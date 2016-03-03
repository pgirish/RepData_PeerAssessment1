---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
## global chunk options are set
## opts_chunk$set() can change the default global options in a document

library(knitr)
library(lubridate)
opts_chunk$set(echo = TRUE, results = 'hold', message = FALSE, warning = FALSE)

activity <- read.csv("activity.csv")
summary(activity)


## What is mean total number of steps taken per day?
total.steps.perday <- aggregate(steps~date, activity, sum, na.rm = TRUE)
mean.total.steps.perday <- mean(total.steps.perday$steps)

median.total.steps.perday <- median(total.steps.perday$steps)
steps <- total.steps.perday$steps

## Make a histogram of the total number of steps taken each day
hist(steps, main="Total Steps Per Day", xlab="Total Steps taken Per Day",border="#ffffff", col="blue", lwd=0.4)

## What is the average daily activity pattern?
steps.per.interval <- aggregate(steps~interval, activity, mean, na.rm=TRUE)
head(steps.per.interval)

library(ggplot2)
ggplot(steps.per.interval, aes(interval, steps)) + geom_line(colour="#000099") + labs(x="Interval", y ="Number of Steps")

##Which 5-minute interval, on average across all the days in the dataset, contains the ##maximum number of steps?     
interval.with.max.steps <- steps.per.interval[which.max(steps.per.interval$steps),]


##Calculate and report the total number of missing values in the dataset (i.e. the total ##number of rows with 𝙽𝙰s)
total.missing.values <- sum(is.na(activity$steps))
head(total.missing.values)
## Imputing missing values
new <- function(x, y){
    new.steps <- x$steps
    all.nas <- which(is.na(new.steps))
    for (a in all.nas){
        intrvl <- x[a,]$interval
        avg.step <- y[y$interval==intrvl,]$steps  
        new.steps[a] <- avg.step
    }
    new.steps
}

new.steps <- new(activity, steps.per.interval)
activity2 <- data.frame(steps = new.steps, date=activity$date, interval=activity$interval)

##  mean,median and  total number of steps taken per day with the new data set
new.total.steps.perday <- aggregate(steps~date, activity2, sum, na.rm = TRUE)
new.mean.total.steps.perday <- mean(new.total.steps.perday$steps)
new.median.total.steps.perday <- median(new.total.steps.perday$steps)
new.steps <- new.total.steps.perday$steps

nofill <- hist(steps, breaks = seq(-250,21750,500), plot = FALSE)
withfill <- hist(new.steps, breaks = seq(-250,21750,500), plot = FALSE)
plot(nofill, col=rgb(.1,.1,.1,alpha=0.8), xlim=c(-250,25000), ylim = c(0,12), main = "",xlab="Average Total Number of Steps per Day") 
plot(withfill, col=rgb(.2,.2,.2,alpha=0.5), xlim=c(-250,25000), ylim = c(0,12), add=T)


#The impact of imputing missing data with the average number of steps in the same 5-min interval is that there is slight difference in both the mean and the median.


##Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is ##a weekday or weekend day.
library(lubridate)
library(dplyr)
weekdata <- mutate(activity2, date = ymd(date), weekday = wday(date),day.type = ifelse(weekday != 1 & weekday != 7,"Weekday",ifelse(weekday == 1 | weekday == 7, "Weekend", NA))) 

weekdata <- mutate(weekdata, day.type = as.factor(day.type))

##panelplot
steps.interval.bydaytype <- aggregate(steps~day.type+interval, weekdata, mean, na.rm=TRUE)
ggplot(steps.interval.bydaytype, aes(x=interval, y=steps)) + 
        geom_line(aes(group=1), colour="#000099") + 
        facet_wrap(~ day.type, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of Steps") +
        theme_grey()
