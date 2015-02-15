Reproducible Research: Peer Assessment 1
========================================

### Basic settings

```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
```

### Loading and processing the data

```r
unzip("activity.zip")
```

```
## Warning in unzip("activity.zip"): error 1 in extracting from zip file
```

```r
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
data$month <- as.numeric(format(data$date, "%m"))
```

```
## Error in data$date: object of type 'closure' is not subsettable
```

```r
noNA <- na.omit(data)
rownames(noNA) <- 1:nrow(noNA)
```

```
## Error in 1:nrow(noNA): argument of length 0
```

```r
head(noNA)
```

```
##                                                                      
## 1 function (..., list = character(), package = NULL, lib.loc = NULL, 
## 2     verbose = getOption("verbose"), envir = .GlobalEnv)            
## 3 {                                                                  
## 4     fileExt <- function(x) {                                       
## 5         db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)              
## 6         ans <- sub(".*\\\\.", "", x)
```

```r
dim(noNA)
```

```
## NULL
```

```r
library(ggplot2)
```


### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Make a histogram of the total number of steps taken each day

```r
ggplot(noNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "green", fill = "green", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

```
## Error: ggplot2 doesn't know how to deal with data of class function
```

* Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:

```r
totalSteps <- aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x
```

```
## Error in noNA$steps: object of type 'closure' is not subsettable
```

```r
mean(totalSteps)
```

```
## Error in mean(totalSteps): object 'totalSteps' not found
```
Median total number of steps taken per day:

```r
median(totalSteps)
```

```
## Error in median(totalSteps): object 'totalSteps' not found
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgSteps <- aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), FUN = "mean")
```

```
## Error in noNA$steps: object of type 'closure' is not subsettable
```

```r
names(avgSteps)[2] <- "meanOfSteps"
```

```
## Error in names(avgSteps)[2] <- "meanOfSteps": object 'avgSteps' not found
```

```r
ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "green", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

```
## Error in ggplot(avgSteps, aes(interval, meanOfSteps)): object 'avgSteps' not found
```

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```

```
## Error in eval(expr, envir, enclos): object 'avgSteps' not found
```

### Imputing missing values
* The total number of rows with NAs:


```r
sum(is.na(data))
```

```
## Warning in is.na(data): is.na() applied to non-(list or vector) of type
## 'closure'
```

```
## [1] 0
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to use the mean for that 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newData <- data 
for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}
```

```
## Error in 1:nrow(newData): argument of length 0
```

```r
head(newData)
```

```
##                                                                      
## 1 function (..., list = character(), package = NULL, lib.loc = NULL, 
## 2     verbose = getOption("verbose"), envir = .GlobalEnv)            
## 3 {                                                                  
## 4     fileExt <- function(x) {                                       
## 5         db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)              
## 6         ans <- sub(".*\\\\.", "", x)
```

```r
sum(is.na(newData))
```

```
## Warning in is.na(newData): is.na() applied to non-(list or vector) of type
## 'closure'
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "green",
                                             fill = "green",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

```
## Error: ggplot2 doesn't know how to deal with data of class function
```

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
newTotalSteps <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
```

```
## Error in newData$steps: object of type 'closure' is not subsettable
```

```r
newMean <- mean(newTotalSteps)
```

```
## Error in mean(newTotalSteps): object 'newTotalSteps' not found
```

```r
newMean
```

```
## Error in eval(expr, envir, enclos): object 'newMean' not found
```
Median total number of steps taken per day:

```r
newMedian <- median(newTotalSteps)
```

```
## Error in median(newTotalSteps): object 'newTotalSteps' not found
```

```r
newMedian
```

```
## Error in eval(expr, envir, enclos): object 'newMedian' not found
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(totalSteps)
```

```
## Error in mean(totalSteps): object 'totalSteps' not found
```

```r
oldMedian <- median(totalSteps)
```

```
## Error in median(totalSteps): object 'totalSteps' not found
```

```r
newMean - oldMean
```

```
## Error in eval(expr, envir, enclos): object 'newMean' not found
```

```r
newMedian - oldMedian
```

```
## Error in eval(expr, envir, enclos): object 'newMedian' not found
```
So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(newData)
```

```
##                                                                      
## 1 function (..., list = character(), package = NULL, lib.loc = NULL, 
## 2     verbose = getOption("verbose"), envir = .GlobalEnv)            
## 3 {                                                                  
## 4     fileExt <- function(x) {                                       
## 5         db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)              
## 6         ans <- sub(".*\\\\.", "", x)
```

```r
newData$weekdays <- factor(format(newData$date, "%A"))
```

```
## Error in newData$date: object of type 'closure' is not subsettable
```

```r
levels(newData$weekdays)
```

```
## Error in newData$weekdays: object of type 'closure' is not subsettable
```

```r
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
```

```
## Error in `*tmp*`$weekdays: object of type 'closure' is not subsettable
```

```r
levels(newData$weekdays)
```

```
## Error in newData$weekdays: object of type 'closure' is not subsettable
```

```r
table(newData$weekdays)
```

```
## Error in newData$weekdays: object of type 'closure' is not subsettable
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps <- aggregate(newData$steps, 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
```

```
## Error in newData$steps: object of type 'closure' is not subsettable
```

```r
names(avgSteps)[3] <- "meanOfSteps"
```

```
## Error in names(avgSteps)[3] <- "meanOfSteps": object 'avgSteps' not found
```

```r
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

```
## Error in eval(expr, envir, enclos): object 'avgSteps' not found
```

