# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
#loading data
data <- read.csv("activity.csv")

#looking at the loaded dataset
summary(data)
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
#summarising steps along days
meanDay2 <- with(data, tapply(steps, date, sum))

#histogramming the result
hist(meanDay2, 
        xlab = "steps", 
        ylab = "frequency", 
        main = "Mean total number of steps taken per day",
        col = "blue")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#total daily steps mean 
mean(meanDay2, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
#total daily steps median
median(meanDay2, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
#summarising steps along intervals
meanInterval2 <- with(data, tapply(steps, interval, mean, na.rm = TRUE))

#looking for maximum steps interval
forP <- data.frame(unique(data$interval), meanInterval2)
ind = 0
iMax <- max(forP[, 2]) 
for (i in seq_along(unique(data$interval)))
        if (forP[, 2][i] == iMax) ind <- forP[, 1][i]

# plotting the average dayly activity plot with maximum point
plot(forP[, 1], forP[, 2], type = "l", main = "Average daily activity plot", xlab="interval", ylab="steps")
points(ind, iMax, col = "red", pch = 10)
infS1 <- paste("Interval number", ind, sep = " ")
infS2 <- paste(ceiling(iMax), "steps", sep = " ")
text(ind, iMax, paste(infS1, infS2, sep = "\n"), cex=0.5, pos=4, col="red")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# reporting maximum
paste(infS1, infS2, sep = ", ")
```

```
## [1] "Interval number 835, 207 steps"
```

## Imputing missing values

```r
#counting and reporting missing values
paste("Total number of missing values:", sum(is.na(data$steps) == TRUE), sep = " ")
```

```
## [1] "Total number of missing values: 2304"
```

```r
#imputing missed values with day means

##replacing NAs with 0 in the day means data set
meanDay <- replace(meanDay2, is.na(meanDay2), 0)

##copying loaded dataset 
data2 <- data

##imputing NAs with  day means in the new dataset
for (i in 1:length(data2[, 2])) 
        if (is.na(data2[ , 1][i])) 
                data2[, 1][i] <- meanDay[data2[, 2][i]]
##checking for NAs
sum(is.na(data2))
```

```
## [1] 0
```

```r
#histogramming the result
meanDay3 <- with(data2, tapply(steps, date, sum))

hist(meanDay3, 
        xlab = "steps", 
        ylab = "frequency", 
        main = "Mean total number of steps taken per day\n (without NAs)",
        col = "green")
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#total daily steps mean with imputed missing values
mean(meanDay3)
```

```
## [1] 9354.23
```

```r
#total daily steps median with imputed missing values
median(meanDay3)
```

```
## [1] 10395
```

*Compare the graphs with NAs and with imputed missing values*

```r
par(mfrow=c(1,2))
hist(meanDay2,  col = "grey", main="Histogram of day means\n (with NAs)", xlab = "steps", ylab = "frequency")
hist(meanDay3, col = "blue", main="Histogram of day means\n (imputed NAs)", xlab = "steps", ylab = "frequency")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## Are there differences in activity patterns between weekdays and weekends?



```r
# adding new field and replasing day titles
data2$wdFactor <- weekdays(as.Date(data2$date), abbreviate = TRUE)
data2$wdFactor <- replace(data2$wdFactor, data2$wdFactor %in% c("Sun", "Sat"), "weekend")
data2$wdFactor <- replace(data2$wdFactor, data2$wdFactor %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday")

# converting added field to the factor 
data3 <- group_by(data2, interval, wdFactor) %>% summarise_each(funs(mean))

# plotting the result
xyplot(steps ~ interval | wdFactor, data = data3, layout = c(1, 2), type = "l", col = "blue")
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
