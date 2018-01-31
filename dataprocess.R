##Set directory to local repository location containing data

#setwd("C:/Users/mike.mcgroddy/Desktop/Coursera/Stats with R/Reproducible Research/Week 2/Project 1/RepData_PeerAssessment1")
library(dplyr)
library(ggplot2)

##1. Loading and preprocessing the data

####Load the data into R and process to convert variables to correct class

filename <- "activity.zip"
destfile <- "activity.csv"
data <- read.csv(unz(filename, destfile))
unlink(destfile)

data$date <- as.Date(data$date)
data$steps <- as.numeric(data$steps)

##2. What is mean total number of steps taken per day?

####Group by day and sum days
day_steps <- data %>% group_by(date) %>% summarize(sums = sum(steps))

####Create histogram of total steps taken per day.  25 breaks gives a better picture
hist(day_steps$sums, breaks = 25, main = "Histogram of Total Number of Steps per Day")

####calculate and report mean and median steps taken per day
mean1 <- mean(day_steps$sums, na.rm = TRUE)
med1 <- median(day_steps$sums, na.rm = TRUE)

##3. What is the average daily activity pattern?

####Calculate average per day for each interval and plot time series
intavg <- data %>% group_by(interval) %>% summarize(avg = mean(steps, na.rm = TRUE))
ggplot(data = intavg, aes(x=interval, y= avg)) + xlab("5-min interval") + ylab("Average Steps") + ggtitle("Average Steps Across All Days for each 5-min Interval") + geom_line() 

####Determine interval with max average steps
intavg[intavg$avg == max(intavg$avg),][1]

##4. Imputing missing values

####Find total number of missing values in the dataset
sum(is.na(data))

####Impute missing values in new dataset
data2 <- data
for (i in 1:length(data2$steps)){
  if (is.na(data2$steps[i])){
    data2[i,"steps"] <- intavg[intavg$interval==data2[i,"interval"],2]
  }
}

####Make new histogram and calculate mean, median and compare to previous
day_steps2 <- data2 %>% group_by(date) %>% summarize(sums = sum(steps))

par(mfrow = c(1,2), margin(c(2,2,4,4)))
hist(day_steps$sums, breaks = 25, main = "Total Number of Steps per Day")
hist(day_steps2$sums, breaks = 25, main = "Total Number of Steps per Day with Imputed Data")

mean2 <- mean(day_steps2$sums)
med2 <- median(day_steps2$sums)

stats <- data.frame("Original" = c(mean1, med1), "Imputed" = c(mean2, med2), row.names = c("mean", "median"))


##5. Are there differences in activity patterns between weekdays and weekends

####Create new factor variable with binary "weekend/weekday" label
data3 <- mutate(data2, day = rep("NA", dim(data2)[1]))
data3[weekdays(data3$date)=="Saturday" | weekdays(data3$date) == "Sunday","day"] = "weekend"
data3[data3$day == "NA","day"] = "weekday"
data3$day <- as.factor(data3$day)

####Create panel plot of weekday and weekend trends
intavg2 <- data3 %>% group_by(interval, day) %>% summarize(avg = mean(steps))
xyplot(avg ~ interval| factor(day), data = intavg2, type = "l")

