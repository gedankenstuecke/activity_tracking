# load packages

library(ggplot2)
library(ggridges)
library(cowplot)
library(GGally)

# read data file 

df <- read.csv(file='fitbit_with_heart.csv',head=T)

# convert to dates, get weekdays & weekend status
df$date <- as.Date(df$date)
df$weekday <- weekdays(df$date)

df$weekday <- factor(df$weekday, levels= c("Monday", 
                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

df$dayoff <- ifelse((df$weekday == 'Saturday' |df$weekday == 'Sunday'),"Weekend", "Weekday")

# plot correlations between data types

ggpairs(subset(df,
               df$sleep.timeInBed > 0 ),
        mapping = aes(color = dayoff, alpha=0.5),
        columns = c('activities.tracker.minutesVeryActive',
                    'activities.tracker.minutesLightlyActive',
                    'activities.tracker.minutesSedentary',
                    'sleep.minutesAsleep',
                    'activities.tracker.steps',
                    'activities.tracker.elevation',
                    'restingHeartRate'),
        columnLabels = c("mins. very active",
                         "mins. lightly active",
                         "mins. sedentary",
                         "mins. asleep",
                         "Steps",
                         "Elevation (ft)",
                         'resting HR'),
        lower = list(
          continuous = "smooth"
        ))
