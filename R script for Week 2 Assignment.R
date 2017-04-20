##Author: Qichen Zeng
##Date: 4/20/2017
##Assignment: Reproducible Research Week 2 Peer assignment R script


## Read in File

library(ggplot2)
df <- read.csv("C:/Users/qiche/Desktop/activity.csv",header = T, sep = ",", stringsAsFactors = F)
df$date <- as.Date(df$date, "%m/%d/%Y")
str(df)


##What is mean total number of steps taken per day?
library (dplyr)
Day <- df %>% group_by(date) %>%
  summarize(total.steps = sum(steps, na.rm = T), 
            
            mean.steps = mean(steps, na.rm = T))



## Histgram of the total number of steps taken

library(ggplot2)
g <- ggplot(Day, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),  
                                            axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")


## The mean and median of the total number of steps taken per day

summary(Day$total.steps)
summary(Day$mean.steps)




## What is the average daily activity pattern?

intervaldf <- df %>% group_by(interval) %>%
  summarize(mean.steps = mean(steps, na.rm = T))
g <- ggplot(intervaldf, aes(x = interval, y = mean.steps))
g + geom_line() + theme(axis.text = element_text(size = 12), 
                        axis.title = element_text(size = 14, face = "bold")) + 
  labs(y = "Mean number of steps") + labs(x = "Interval")


## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

## From the timeseries chart, looks like the largest amount occurs within 500 and 1000



##Imputing missing values

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

mean(is.na(df$steps))
sum(is.na(df$steps))
sum(is.na(intervaldf$mean.steps))


##fill in the NA

nona <- df
for (i in 1:nrow(nona)) {
  if (is.na(nona$steps[i])) {
    index <- nona$interval[i]
    value <- subset(intervaldf, interval==index)
    nona$steps[i] <- value$mean.steps
  }
}


##Histogram for total number of steps taken

newAvg <- nona %>% group_by(date) %>%
  summarize(total.steps = sum(steps, na.rm = T))
g <- ggplot(newAvg, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),
                                            axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")



summary (df$total.steps)

sd(df$total.steps, na.rm=T)

summary (newAvg$total.steps)

sd(newAvg$total.steps, na.rm=T)

## The mean and med stayed the same, however the the data become closer to the mean. The new data have smaller std, therefore the effect of the filled in NA with mean decreased the spread


## Are there differences in activity patterns between weekdays and weekends?


nona$day <- ifelse(weekdays(nona$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

weekends <- filter(nona, day == "weekend")
weekdays <- filter(nona, day == "weekday")

weekends <- weekends %>%
  group_by(interval) %>%
  summarize(mean.steps = mean(steps)) 
weekends$day <- "weekend"

weekdays <- weekdays %>%
  group_by(interval) %>%
  summarize(mean.steps = mean(steps)) 
weekdays$day <- "weekday"

bindeddata <- rbind(weekends, weekdays)
bindeddata$day <- as.factor(bindeddata$day)
bindeddata$day <- relevel(bindeddata$day, "weekend")


finalgraph <- ggplot (bindeddata, aes (interval, mean.steps))
finalgraph + geom_line() + facet_grid (day~.) + theme(axis.text = element_text(size = 12), 
                                                      axis.title = element_text(size = 14)) + labs(y = "Number of Steps") + labs(x = "Interval")


## Yes the patterns are different from weekdays and weekends, however, the peek intervals are still around 500-1000