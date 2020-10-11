Assignment Reproducible Research Project1
Setting global options
knitr::opts_chunk$set(echo = TRUE,warning=FALSE, fig.width=8,fig.height=4)
Loading and processing data
library(ggplot2)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
# Import data
activity <- read.csv("activity.csv")
#looking at the structure of the data
str(activity)
## 'data.frame':    17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
head(activity)
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
# Setting date format to get the weekdays of the dates
activity$date <- as.POSIXct(activity$date, "%Y%m%d")

# Getting the days of all the dates
day <- weekdays(activity$date)

# Combining the dataset with the weekday of the dates
activity <- cbind(activity, day)

# Viewing the processed data
summary(activity)
##      steps             date               interval          day           
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
##  NA's   :2304
What is the mean total number of steps taken per day?
# Calculating total steps taken on a day
totalsteps <- with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
# Changing col names
names(totalsteps) <- c("Date", "Steps")

# Converting the data set into a data frame for ggplot2
dftsteps <- data.frame(totalsteps)

# Plotting a histogram using ggplot2
ggplot(dftsteps, aes(x = Steps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "light blue", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day")


png("plot1.png")
dev.off()
## png 
##   2
The mean of the total number of steps taken per day is:
mean(totalsteps$Steps)
## [1] 9354.23
The median of the total number of steps taken per day is:
median(totalsteps$Steps)
## [1] 10395
What is the average daily activity pattern?
A time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Calculating the average number of steps taken
avgdailyactv <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
# Changing col names
names(avgdailyactv) <- c("Interval", "Mean")

# Converting the data set into a dataframe
dfavgdactv <- data.frame(avgdailyactv)

# Plotting on ggplot2
ggplot(dfavgdactv, mapping = aes(Interval, Mean)) + 
  geom_line(col = "green") +
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Per Interval") 


 png("plot2.png")
 dev.off()
## png 
##   2
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
avgdailyactv[which.max(avgdailyactv$Mean), ]$Interval
## [1] 835
Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity$steps))
## [1] 2304
Devise a strategy for filling in all of the missing values in the dataset
# Matching the mean of daily activity with the missing values
steps_impute <- avgdailyactv$Mean[match(activity$interval, avgdailyactv$Interval)]
Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Transforming steps in activity if they were missing values with the filled values from above.
actv_impute <- transform(activity, steps = ifelse(is.na(activity$steps), yes = steps_impute, no = activity$steps))

# Forming the new dataset with the imputed missing values.
totimpuactv <- aggregate(steps ~ date, actv_impute, sum)

# Changing col names
names(totimpuactv) <- c("date", "dailySteps")
Testing the new dataset to check if it still has any missing values
sum(is.na(totimpuactv$dailySteps))
## [1] 0
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
# Converting the data set into a data frame to be able to use ggplot2
dftotste_imput <- data.frame(totimpuactv)

# Plotting a histogram using ggplot2
ggplot(dftotste_imput, aes(x = dailySteps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "light blue", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day")


png("plot3.png")
The mean of the total number of steps taken per day is:
mean(totimpuactv$dailySteps)
## [1] 10766.19
The median of the total number of steps taken per day is:
median(totimpuactv$dailySteps)
## [1] 10766.19
Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# Updating format of the dates
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))

# Creating a function that distinguishes weekdays from weekends

activity$daytype <- sapply(activity$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
  {y <- "Weekend"}
  else {y <- "Weekday"}
  y
})
Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
# Creating the data set that will be plotted
activity_day <-  aggregate(steps ~ interval + daytype, activity, mean, na.rm = TRUE)

# Plotting using ggplot2
ggplot(activity_day, aes(x = interval , y = steps, color = daytype)) + 
  geom_line() + ggtitle("Average Daily Steps by Day Type") + 
  xlab("Interval") + 
  ylab("Average Number of Steps") +
  facet_wrap(~daytype, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Day Type")


png("plot4.png")
