# Reproducible Research: Peer Assessment 1
HT  
May 21, 2016 and Jul 17,2016  




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

![](PA1_template-v2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_total_number_of_steps_per_day = tapply(activity_data$steps, activity_data$date, mean, na.rm=TRUE)
mean_total_number_of_steps <- mean(total_steps_per_day, na.rm = TRUE)
median_total_number_of_steps_per_day <- median(total_steps_per_day, na.rm = TRUE)

print(paste("Mean total number of steps = ",mean_total_number_of_steps))
```

```
## [1] "Mean total number of steps =  9354.22950819672"
```

```r
print(paste("Median total number of steps per day = ", median_total_number_of_steps_per_day ))
```

```
## [1] "Median total number of steps per day =  10395"
```


## What is the average daily activity pattern?   

```r
mean_steps_per_interval <- tapply(activity_data$steps, activity_data$interval, mean, na.rm = TRUE)
plot(mean_steps_per_interval, type="l",main="Average Daily Activity Pattern", 
     xlab="Daily Interval", ylab="Mean Total Number of Steps per day")
```

![](PA1_template-v2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

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
### Total number of rows with missing values   


```r
##sum(is.na(activity_data$steps))
print(paste("Total number of rows in data set = ", nrow(activity_data)))
```

```
## [1] "Total number of rows in data set =  17568"
```

```r
print(paste("Total number of rows with complete data (i.e. no missing data in any field = )", sum(complete.cases(activity_data))))
```

```
## [1] "Total number of rows with complete data (i.e. no missing data in any field = ) 15264"
```

```r
print(paste("Total number of rows with missing values = ",nrow(activity_data) - sum(complete.cases(activity_data))))
```

```
## [1] "Total number of rows with missing values =  2304"
```

## Imputing missing values   

```r
## We test each element of 'steps'; if it is NA, we replace with the mean, otherwise we replace with the original value.
imputed_data = transform(activity_data, steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))

stepsTotalPerDay_NoMissing <- tapply(imputed_data$steps, imputed_data$date, sum)
hist(stepsTotalPerDay_NoMissing, breaks = 6, main = "Frequency of number of steps per day", xlab = "Number of steps per day", ylab = "Frequency", col = "blue")
```

![](PA1_template-v2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
## Mean of imputed data
imputedmean_total_number_of_steps_per_day = tapply(imputed_data$steps, imputed_data$date, mean, na.rm=TRUE)
imputedmean_total_number_of_steps <- mean(stepsTotalPerDay_NoMissing, na.rm = TRUE)
imputedmedian_total_number_of_steps_per_day <- median(stepsTotalPerDay_NoMissing, na.rm = TRUE)

print(paste("Mean total number of steps = ",imputedmean_total_number_of_steps))
```

```
## [1] "Mean total number of steps =  10766.1886792453"
```

```r
print(paste("Median total number of steps per day = ", imputedmedian_total_number_of_steps_per_day ))
```

```
## [1] "Median total number of steps per day =  10766.1886792453"
```

```r
if(imputedmean_total_number_of_steps == imputedmedian_total_number_of_steps_per_day) {
  print("The impact of imputing the missing steps data is that, the Mean and median of imputed data are equal, suggesting a standard normal distribution. This is confirmed by the histogram below.")
}
```

```
## [1] "The impact of imputing the missing steps data is that, the Mean and median of imputed data are equal, suggesting a standard normal distribution. This is confirmed by the histogram below."
```

```r
hist(stepsTotalPerDay_NoMissing, main = "Frequency of number of steps per day for imputed data", 
     xlab = "Number of steps per day", ylab = "Frequency", col = "blue")
```

![](PA1_template-v2_files/figure-html/unnamed-chunk-6-2.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a factor variable with two levels (weekday, weekend-day)
LT <- as.POSIXlt(imputed_data$date, format = "%Y-%m-%d")
WeekDays <- LT$wday

WeekDays[WeekDays == 0] = 0 ## Sunday
WeekDays[WeekDays == 6] = 0 ## Saturday
WeekDays[WeekDays != 0] = 1 ## Other week day

WeekDaysFactor <- factor(WeekDays, levels = c(0, 1))
# Add the factor variable to the data
imputed_data$WD <- WeekDaysFactor
# Calculate the mean
MeanStepsPerWeekday <- tapply(imputed_data$steps, list(imputed_data$interval, imputed_data$WD), mean, 
    na.rm = TRUE)

par(mfrow = c(2, 1))
# Display the 2 plots
with(imputed_data, {
    par(mai = c(0, 1, 1, 0))
    plot(MeanStepsPerWeekday[, 1], type = "l", main = ("Steps vs. Interval"), 
        xaxt = "n", ylab = "Week-ends")
    title = ("# of Steps v.s. Interval")
    par(mai = c(1, 1, 0, 0))
    plot(MeanStepsPerWeekday[, 2], type = "l", xlab = "Interval", ylab = "Week days")

})
```

![](PA1_template-v2_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
## Showing difference in the plots in terms of stats too (apart from the visual above)

```r
## These are the summary stats of week-ends
summary(MeanStepsPerWeekday[, 1])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   4.673   5.642  32.700  42.370  70.800 157.800
```

```r
## This is the Standard Deviation of the week-end data
sd(MeanStepsPerWeekday[, 1])
```

```
## [1] 38.86906
```

```r
## These are the summary stats of weekdays
summary(MeanStepsPerWeekday[, 2])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   4.984   6.907  25.760  35.610  49.940 207.900
```

```r
## This is the Standard Deviation of the weekday data
sd(MeanStepsPerWeekday[, 2])
```

```
## [1] 36.61514
```

```r
## Result: Therefore, based on the above visual plot and summary statistics presented above:
## we infer that there is some difference between activity on weekdays and week-ends
## but not a whole lot (i.e. not highly significant difference)
## However, it is noted that, the Mean number of steps in the week-end is higher than the Mean number of steps in the week days.
## This suggests, more people walk (more) in the week-ends than weekdays.
## Also, the maximum number of steps walked in the week days is higher than the maximum number of steps walked in the week-ends.
## This could possibly be because of some out-liers in data wherein, someone with an outdoors job (e.g. delivery) may be causing this increase, whereas, people may be going out on shorter walks in the week-ends, which seems to reduce the maximum number of steps walked in the week-ends.
```
