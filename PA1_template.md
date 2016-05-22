# Reproducible Research: Peer Assessment 1
HT  
May 21, 2016  




## Loading and preprocessing the data

```r
activity_data <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
summary(activity_data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?

```r
total_steps_per_day <- tapply(activity_data$steps, activity_data$date, sum, na.rm = TRUE)
hist(total_steps_per_day, main = "Frequency of number of steps per day", 
     xlab = "Number of steps per day", ylab = "Frequency", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_total_number_of_steps_per_day = tapply(activity_data$steps, activity_data$date, mean, na.rm=TRUE)
mean_total_number_of_steps <- mean(total_steps_per_day, na.rm = TRUE)
mean_total_number_of_steps
```

```
## [1] 9354.23
```

```r
median_total_number_of_steps_per_day <- median(total_steps_per_day, na.rm = TRUE)
median_total_number_of_steps_per_day
```

```
## [1] 10395
```


## What is the average daily activity pattern?   

```r
mean_steps_per_interval <- tapply(activity_data$steps, activity_data$interval, mean, na.rm = TRUE)
plot(mean_steps_per_interval, type="l",main="Average Daily Activity Pattern", 
     xlab="Daily Interval", ylab="Mean Total Number of Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
max_number_steps_sequence <- seq(along = mean_steps_per_interval)[mean_steps_per_interval == max(mean_steps_per_interval)]
max_number_steps_sequence
```

```
## [1] 104
```

```r
# Get the steps data as a vector
tmp_dataTest <- as.vector(activity_data$steps)
# Set it to one where data is missing
tmp_dataTest[is.na(tmp_dataTest)] = 1
```
## Imputing missing values   

```r
## Get the mean steps per interval as a vector
tmp_mean_steps_per_interval <- as.vector(mean_steps_per_interval)

## Repeat it to be the same for each of the 61 days
tmp_mean_steps_per_interval <- rep(tmp_mean_steps_per_interval, 61)

## Set it one where there is no missing data
tmp_mean_steps_per_interval[!is.na(activity_data$steps)] = 1

## Get the steps data as a vector
tmp_dataTest <- as.vector(activity_data$steps)

## Set it to one where data is missing
tmp_dataTest[is.na(tmp_dataTest)] = 1

data_NoMissing <- activity_data
data_NoMissing$steps <- tmp_mean_steps_per_interval * tmp_dataTest
stepsTotalPerDay_NoMissing <- tapply(data_NoMissing$steps, data_NoMissing$date, 
    sum)
hist(stepsTotalPerDay_NoMissing, breaks = 6, main = "Frequency of number of steps per day", 
    xlab = "Number of steps per day", ylab = "Frequency", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Total number of rows with missing values   


```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a factor variable with two levels (weekday, weekend-day)
LT <- as.POSIXlt(activity_data$date, format = "%Y-%m-%d")
WeekDays <- LT$wday
WeekDays[WeekDays == 0] = 0
WeekDays[WeekDays == 6] = 0
WeekDays[WeekDays != 0] = 1
WeekDaysFactor <- factor(WeekDays, levels = c(0, 1))
# Add the factor variable to the data
activity_data$WD <- WeekDaysFactor
# Calculate the mean
MeanStepsPerWeekday <- tapply(activity_data$steps, list(activity_data$interval, activity_data$WD), mean, 
    na.rm = TRUE)

par(mfrow = c(2, 1))
# Display the 2 plots
with(activity_data, {
    par(mai = c(0, 1, 1, 0))
    plot(MeanStepsPerWeekday[, 1], type = "l", main = ("Steps vs. Interval"), 
        xaxt = "n", ylab = "Week-ends")
    title = ("# of Steps v.s. Interval")
    par(mai = c(1, 1, 0, 0))
    plot(MeanStepsPerWeekday[, 2], type = "l", xlab = "Interval", ylab = "Week days")

})
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
