data <- read.csv("activity.csv", head = TRUE, stringsAsFactors = FALSE)
clean.data <- na.omit(data)
require(dplyr)
require(ggplot2)
require(lattice)
days <- group_by(clean.data, date)
total_days <- summarise(days, total = sum(steps, na.rm = TRUE))

hist(total_days$total, col = "green", xlab = "Total Steps Taken per Day",
     main = "Histogram of Total Steps in a Day", ylim = c(0,20), breaks = 20)
ggplot(total_days, aes(x = total)) + geom_histogram() 
     + labs(x = "Total Steps Taken per Day", y = "Frequency",
     title = "Histogram of Total Steps in a Day")

paste("Mean = ",mean(total_days$total), sep="")
paste("Median = ",median(total_days$total), sep="")
paste("Standard Deviation = ",sd(total_days$total),sep="")

interval <- group_by(clean.data, interval)
interval_avg <- summarise(interval, avg = mean(steps))
plot(interval_avg$interval,interval_avg$avg, type = "l", xlab = "Interval (5 min increments)",
     ylab = "Average Number of Steps", main = "Average Number of Steps per Interval over Time")
max_point <- grep((max(interval_avg$avg)), interval_avg$avg)
interval_avg[(max_point-1):(max_point+1),]
summary(data)
sum(is.na(data))

missing_data <- is.na(data$steps)
filled.data <- data
for (i in 1:nrow(filled.data)) {
  if(missing_data[i] == TRUE) {
    missing_interval = filled.data[i,3]
    avg_index <- grep(missing_interval, interval_avg[,1])
    filled.data$steps[i] = interval_avg[avg_index,2] 
  }
}
filled.days <- group_by(filled.data, date)
filled.total_days <- summarise(filled.days, total = sum(steps))
par(mfrow = c(2,1))
hist(total_days$total, col = "green", xlab = "Total Steps Taken per Day",
     main = "Histogram of Total Steps in a Day", ylim = c(0,20), breaks = 20)

hist(filled.total_days$total, col = "green", xlab = "Total Steps Taken per Day",
     main = "Histogram of Total Steps in a Day (Filled Data)", ylim = c(0,20), breaks = 20)

paste("Mean = ",mean(filled.total_days$total), sep = "")
paste("Median = ",median(filled.total_days$total), sep="")
paste("Standard Deviation = ", sd(filled.total_days$total), sep ="")

filled.data$day <- weekdays(as.Date(filled.data[,2]))
filled.data$day <- gsub("Sunday", "weekend", filled.data$day)
filled.data$day <- gsub("Saturday", "weekend", filled.data$day)
filled.data$day <- gsub("Monday", "weekday", filled.data$day)
filled.data$day <- gsub("Tuesday", "weekday", filled.data$day)
filled.data$day <- gsub("Wednesday", "weekday", filled.data$day)
filled.data$day <- gsub("Thursday", "weekday", filled.data$day)
filled.data$day <- gsub("Friday", "weekday", filled.data$day)

filled.data.weekday <- subset(filled.data, day == "weekday")
filled.data.weekend <- subset(filled.data, day == "weekend")

interval.weekday <- group_by(filled.data.weekday, interval)
interval.weekend <- group_by(filled.data.weekend, interval)
interval.weekday_avg <- summarise(interval.weekday, avg = mean(steps))
interval.weekday_avg$day <- "weekday"
interval.weekend_avg <- summarise(interval.weekend, avg = mean(steps))
interval.weekend_avg$day <- "weekend"

interval.week_avg <- rbind(interval.weekday_avg, interval.weekend_avg)
xyplot(interval.week_avg$avg ~ interval.week_avg$interval | interval.week_avg$day,
       layout = c(1,2), type = "l", xlab = "Interval (5 min increments)",
       ylab = "Average Number of Steps",
       main = "Average Number of Steps per Interval over Time")

