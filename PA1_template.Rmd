Reproducible Research Assignment
==============================

First download the data

Read the data into R Studio and view the summary of it.

```{r}
setwd("C:/Users/u6027576/Documents/Coursera/reproducibleResearch")
activity <- read.csv("activity.csv")
summary(activity)
```

Remove the rows in the data frame where the number of steps is not given (NA) or just calculate the means of all.

What is mean total number of steps taken per day?
================================================
```{r}
activityNAR <- na.omit(activity)
## Sum
activitySum <- tapply(activity$steps, activity$date, sum)
df.actSum <- data.frame(date=names(activitySum), sum.steps=activitySum)
## Create histogram
library(ggplot2)
ggplot(df.actSum,aes(x=sum.steps)) +
    geom_histogram(fill = "red", alpha = 0.4)

## Mean
activityMeans <- tapply(activity$steps, activity$date, mean)
activityMin <- tapply(activity$steps, activity$date, min)
activityMax <- tapply(activity$steps, activity$date, max)
activityMedian <- tapply(activity$steps, activity$date, median)
df.actAv <- data.frame(mean.steps=activityMeans, min.steps=activityMin, max.steps=activityMax, median.steps=activityMedian)
df.actAv
```

What is the average daily activity pattern?
===========================================

```{r}
activityNAR <- na.omit(activity)
TimeSeries <- tapply(activityNAR$steps, activityNAR$interval, mean)
df.timeSeries <- data.frame(Time=as.numeric(as.character(names(TimeSeries))), mean.steps=TimeSeries)
dtt <- sprintf("%04d", df.timeSeries$Time)
df.timeSeries$Time2 <- strptime(dtt, format="%H%M")

p <- ggplot() + 
  geom_line(data = df.timeSeries, aes(x = Time2, y = mean.steps, group=1), color="red") +labs(x = "Time", y = "Mean average of steps", title = "Average steps through the day")
p

df.timeSeries$IntervalMinutes <- (df.timeSeries$Time %% 100) + (df.timeSeries$Time %/% 100)*60

q <- ggplot() + 
  geom_line(data = df.timeSeries, aes(x = IntervalMinutes, y = mean.steps, group=1), colour="#000099") +labs(x = "Time in minutes", y = "Mean average of steps", title = "Average steps through the day")
q

```


Which 5-minute interval contains the highest average for steps?

```{r}

maxStepAv <- max(df.timeSeries$mean.steps)
maxStepAvIndex <- grep(pattern=maxStepAv, df.timeSeries$mean.steps)
fiveMInterval <- df.timeSeries$Time[maxStepAvIndex]
fiveMInterval

```

Imputing missing values
================================
Calculating rows containing NA. False values on complete cases are the rows which contain NA.

```{r}

table(complete.cases(activity))

```

Filling in the blanks. Add bullet point explanation. All data is filled in now.

```{r}
intervals <- unique(activity$interval)
a <- which(is.na(activity$steps))

for (i in intervals){
  b <- which(activity$interval== i)
  empties <- intersect(a, b) 
  intervalPosition <- which(df.timeSeries$Time == i)
  intMean <- df.timeSeries$mean.steps[intervalPosition]
  activity$steps[empties] <- intMean
}

```

Complete cases is run again to show all cases are complete.

```{r}
A <- complete.cases(activity)
table(A)
```

Making the histograms with the new data

```{r}
activitySum <- tapply(activity$steps, activity$date, sum)
df.actSum <- data.frame(date=names(activitySum), sum.steps=activitySum)

##plot
library(ggplot2)
ggplot(df.actSum,aes(x=sum.steps)) +
    geom_histogram(fill = "red", alpha = 0.4)

## Mean
activityMeans <- tapply(activity$steps, activity$date, mean)
activityMin <- tapply(activity$steps, activity$date, min)
activityMax <- tapply(activity$steps, activity$date, max)
activityMedian <- tapply(activity$steps, activity$date, median)
df.actAv <- data.frame(mean.steps=activityMeans, min.steps=activityMin, max.steps=activityMax, median.steps=activityMedian)
df.actAv
```

Looking at the means and medians, the mean falls near enough the others but it is perhaps an overestimate. The approximate value for median is too high as it is 0 in all other cases.

Are there differences in activity patterns between weekdays and weekends?
========================================================================

When creating the column, Monday, Tuesday, Wednesday, Thursday, Friday are weekday and Saturday and Sunday are Weekends.

```{r}
activity$day <- weekdays(as.Date(activity$date, "%Y-%m-%d"))

weekend <- c('Saturday', 'Sunday')
activity$weekday <- factor((activity$day %in% weekend), levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))
```

To plot weekdays Vs. weekends

```{r}

DayEnd <- split(activity, activity$weekday)
weekendData <- data.frame(DayEnd[[1]])
weekdayData <- data.frame(DayEnd[[2]])
TimeSeries <- tapply(weekendData$steps, weekendData$interval, mean)
TimeSeries2 <- tapply(weekdayData$steps, weekdayData$interval, mean)

#weekend df
df.timeSeries <- data.frame(Time=as.numeric(as.character(names(TimeSeries))), mean.steps=TimeSeries, weekday="weekend")
df.timeSeries$IntervalMinutes <- (df.timeSeries$Time %% 100) + (df.timeSeries$Time %/% 100)*60

#weekday df
df.timeSeries2 <- data.frame(Time=as.numeric(as.character(names(TimeSeries2))), mean.steps=TimeSeries2, weekday="weekday")
df.timeSeries2$IntervalMinutes <- (df.timeSeries2$Time %% 100) + (df.timeSeries2$Time %/% 100)*60

df.full <- rbind(df.timeSeries2, df.timeSeries)

## q plot panel of weekend and weekday
qplot(IntervalMinutes, mean.steps, data=df.full, geom="line", color=weekday) + facet_grid(~weekday, scales='free', space='free') +labs(x = "Time in minutes", y = "Mean average of steps", title = "Average steps through the day")

```