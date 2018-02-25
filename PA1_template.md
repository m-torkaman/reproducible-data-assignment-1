---
title: "Report"
author: "Meysam Torkaman"
date : "23/02/2018"
output: html_document
keep_md : TRUE
        
editor_options: 
  chunk_output_type: inline
---

```{r global_options, include=FALSE}
knitr::opts_chunk$set(fig.width=8, fig.height=4, fig.path='figure/',
                      echo=TRUE)
```
```{r loading_packages, include=FALSE}
library(ggplot2)
library(data.table)
library(dplyr)
library(grid)
library(lubridate)
library(lattice)
```
### Loading and preprocessing the data

```{r read_data, cache=TRUE}
activityData <- read.csv("./Factivity/activity.csv", stringsAsFactors = F,
         header = TRUE, colClasses = c("integer","character", "integer"),
                 na.strings = NA)

activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
```

### What is mean total number of steps taken per day?

```{r mean_total_steps_per_day}

## Calculate the total number of steps taken per day
DailyTotalSteps <- with(activityData, tapply(steps, date, sum, na.rm=TRUE))

DTS_df <- as.data.table(DailyTotalSteps, keep.rownames = "date")
DTS_df$date <- as.Date(DTS_df$date, "%Y-%m-%d")

##  Make a histogram of the total number of steps taken each day
with(DTS_df, plot(date, DailyTotalSteps, main="Total Daily Steps Histogram", xlab="Date", ylab="Steps Count", type="h", lwd=6, col="chocolate2"))

## Calculate and report the mean and median of the total number of steps taken per day

mean_ttl_stp_org <- round(mean(DTS_df$DailyTotalSteps),2)
mdian_ttl_stp_org <- round(median(DTS_df$DailyTotalSteps),2)
```

<b><font color=blue> For the original data, the mean of total number of daily steps is <font color=red> `r format(mean_ttl_stp_org, scientific = FALSE)` </font>, while the median of total daily steps equates to <font color=red> `r format(mdian_ttl_stp_org, scientific = FALSE)` </font>.</font></b>

### What is the average daily activity pattern?

```{r activity_pattern}
## calculating and plotting average daily activity pattern
patterns <- aggregate(steps ~ interval, activityData, FUN = mean, na.rm = TRUE)

with(patterns, plot(interval, steps, type="l", col="chocolate2", lwd=3 ))

## the interval with Maximum average number of steps
maxStepsInterval <- patterns[ patterns$steps == max(patterns$steps), 1]

```

<b><font color=blue> The 5-minute interval with the maximum steps across all the days is <font color=red> `r maxStepsInterval`. </font></font></b> 

### Imputing missing values

```{r imputing_NAs}
## calculating number of missing values in the data
NA_count <- sum(is.na(activityData$steps)) 

newData <- activityData

## Imputing NAs by the mean of steps in the the same interval across all days  
for (i in 1:nrow(newData)){
      if (is.na(newData[i,1])){
          newData[i,1] <- patterns[which(newData[i,3] == patterns$interval), 2]
        }
}

```

<b><font color=blue> The total number of missing values in the Original dataset is equal to <font color=red> `r NA_count`. </font> We use the mean of steps in the same interval across all days to impute the missing values.</font></b>

```{r}
## Calculate the total number of steps taken per day for imputed data
new_DailyTotalSteps <- with(newData, tapply(steps, date, sum))

new_DTS_df <- as.data.table(new_DailyTotalSteps, keep.rownames = "date")
new_DTS_df$date <- as.Date(new_DTS_df$date, "%Y-%m-%d")

##  Make a histogram of the total number of steps taken each day for imputed data
with(new_DTS_df, plot(date, new_DailyTotalSteps, main="Total Daily Steps Histogram (Imputed)", xlab="Date", ylab="Steps Count (Imputed)", type="h", lwd=6, col="chocolate2"))

## Calculate and report the mean and median of the total number of steps taken per day for imputed data

new_mean_ttl_stp <- round(mean(new_DTS_df$new_DailyTotalSteps),2)
new_mdian_ttl_stp <- round(median(new_DTS_df$new_DailyTotalSteps),2)

```

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

<b><font color=blue> For the new imputed dataset, the mean of total number of daily steps is <font color=red> `r format(new_mean_ttl_stp, scientific = FALSE)`</font>, while the mean for the otiginal data was <font color=red> `r format(mean_ttl_stp_org, scientific = FALSE)` </font>, the absolute difference of the two means is <font color=red>`r format(abs(new_mean_ttl_stp - mean_ttl_stp_org), scientific = FALSE)`</font>.
Also,the median of total daily steps for new imputed dataset equates to <font color=red> `r format(new_mdian_ttl_stp, scientific = FALSE)` </font>, where the median for original dataset was <font color=red> `r format(mdian_ttl_stp_org, scientific = FALSE)`</font>, the absolute difference of <font color=red> `r format(abs(new_mdian_ttl_stp - mdian_ttl_stp_org), scientific = FALSE)` </font>.</font></b>

### Are there differences in activity patterns between weekdays and weekends?

```{r weekday_weekend_difference}
factor_vector <- character(c(nrow(newData)))
for (j in 1:nrow(newData)){
        if ((wday(newData$date[j], week_start = 1)) < 6)  factor_vector[j] <- c("weekday")   
        else    factor_vector[j] <- c("weekend")    
}

newData <- cbind(newData, data.frame(factor_vector))
```

```{r time_series_plot}
## time series plot of 5-minute interval and the average of steps across weekdays and weekends

new_patterns <- newData %>% group_by(factor_vector, interval) %>%
                summarise(new_interval_mean = mean(steps))

xyplot(new_interval_mean ~ interval | factor_vector, data = new_patterns, type="l", ylab="Number of Steps", layout = c(1,2), main="Average steps patterns of weekdays and weekends", col = "chocolate2", lwd = 2)

```

