# load data
org.df <- read.csv("activity.csv", stringsAsFactors = FALSE)

# explore dataset
dim(org.df)
summary(org.df)
str(org.df)

# convert date from char to date and summarize
org.df$date <- as.Date(org.df$date)
agg_date.df <- setNames(aggregate(org.df$steps ~ org.df$date, data = org.df, FUN = sum), c("Date","Steps"))
head(agg_date.df)

# mean and median steps per day
mean(agg_date.df$Steps)
median(agg_date.df$Steps)

# histogram of steps per day
library(ggplot2)
ggplot(agg_date.df, aes(Steps)) +
        geom_histogram(binwidth = 2000, fill = "blue") +
        labs(title = "Histogram of Daily Steps", x = "Steps per Day", y = "Frequency")

# time series plot of average steps across intervals
agg_int.df <- setNames(aggregate(org.df$steps ~ org.df$interval, data = org.df, FUN = mean), c("Interval","Steps"))
ggplot(agg_int.df, aes(x = Interval, y = Steps)) +
        geom_line(color = "blue") +
        labs(title = "Time Series of Daily Average Steps over Intervals", x = "Intervals", y = "Number of Steps")
# 5 minute interval with the maximum number of steps
agg_int.df[which.max(agg_int.df$Steps),]

# number of missing values
sapply(org.df, function(x) sum(is.na(x)))

# imputing missing values
# copy original dataset
imp.df <- org.df
# impute missing values with the mean
imp.df$steps[is.na(imp.df$steps)] <- mean(imp.df$steps, na.rm = TRUE)
sapply(imp.df, function(x) sum(is.na(x)))
# aggregate the imputed data set to get the total number of steps taken daily
agg_date_imp.df <- setNames(aggregate(imp.df$steps ~ imp.df$date, data = imp.df, FUN = sum), c("Date","Steps"))
# histogram of 
ggplot(agg_date_imp.df, aes(Steps)) +
        geom_histogram(binwidth = 2000, fill = "blue") +
        labs(title = "Histogram of Daily Steps (w/ imputed val)", x = "Steps per Day", y = "Frequency")
# mean and median of imputed dataset
mean(agg_date_imp.df$Steps)
median(agg_date_imp.df$Steps)

# add DayType (weekend/weekday) feature into the imputed dataset
weekend <- c("Saturday","Sunday")
imp.df$DayType <- as.factor(ifelse(weekdays(imp.df$date) %in% weekend, "Weekend", "Weekday"))
# aggregate data points over intervals
agg_int_imp.df <- setNames(aggregate(imp.df$steps ~ imp.df$interval + imp.df$DayType, data = imp.df, FUN = mean), c("Interval","DayType", "Steps"))

# panel plot for Interval Mean Steps - Weekday Vs. Weekend
ggplot(agg_int_imp.df, aes(x = Interval, y = Steps, color = DayType)) +
        geom_line() +
        facet_wrap(~DayType, ncol = 1, nrow = 2) +
        labs(title = "Interval Mean Steps - Weekday Vs. Weekend", x = "Interval", y = "Mean Steps")







