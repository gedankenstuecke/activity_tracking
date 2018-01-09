# required libraries. should be installed via install.packages()
library('devtools')
library("ggplot2")
library("cowplot")
library("dplyr")
# use devtools to install the fitbitr package if needed
devtools::install_github("teramonagi/fitbitr")
library("fitbitr")

# set your fitbit personal API key/secret here

FITBIT_KEY    <- "<your-fitbit-key>"
FITBIT_SECRET <- "<your-firbit-secret>"

# get token for subsequent calls

token <- fitbitr::oauth_token()

# set range of dates of interest
dates_of_interest = seq(as.Date('2017-11-10'),as.Date('2018-01-03'),'days')

### HEART RATE

# initialize empty dataframe
full_hr = data.frame(time = character(),
                     value = integer())

# get data for each date of interest, append data to full_hr
for (date in as.list(dates_of_interest)){
  print(as.character(date))
  single_date_hr = get_heart_rate_intraday_time_series(token, date=as.character(date), detail_level="5min")
  full_hr <- rbind(full_hr,single_date_hr)
  }

#moving around the dates/times
full_hr$date <- full_hr$time
# save only the time of the day
times <- strftime(full_hr$time,format="%H:%M")
full_hr$time <- as.POSIXct(times, format="%H:%M")
# save only the day
full_hr$day <- strftime(full_hr$date,format="%Y-%m-%d")

# get day of the week and eval whether workday or weekend
full_hr$weekdays <- weekdays(as.Date(full_hr$date))
full_hr$weekdays <- factor(full_hr$weekdays, levels= c("Monday",
                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
full_hr$dayoff <- ifelse((full_hr$weekdays == 'Saturday' |full_hr$weekdays == 'Sunday'),"Weekend", "Weekday")

# get mean data for each time
full_hr %>% group_by(time,dayoff) %>% summarise(value = mean(value,na.rm=TRUE)) -> hr_mean_dayoff

# do the plot
hr_plot <- ggplot(subset(full_hr,full_hr$value > 0),aes(time,value)) +
  geom_line(alpha=0.2,aes(group=day)) +
  scale_y_continuous('heart rate') +
  geom_line(data=hr_mean_dayoff,color='#7f0000') +
  scale_x_datetime(date_label = "%H:%M") +
  theme_minimal() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        strip.text.y = element_text(size = 12)) +
  facet_grid(. ~ dayoff) +
  geom_vline(xintercept = as.POSIXct(paste(full_hr$time[1],'09:00:00')),color='red',alpha=0.4) +
  geom_vline(xintercept = as.POSIXct(paste(full_hr$time[1],'17:00:00')),color='red',alpha=0.4)



### STEPS
# analogous to the Heart rate data
full_steps = data.frame(time = character(),
                        value = integer())

for (date in as.list(dates_of_interest)){
  df <- get_activity_intraday_time_series(token, 'steps',as.character(date), detail_level="5min")
  df$time <- as.POSIXct(strptime(paste0(df$dateTime, " ", df$dataset_time), "%Y-%m-%d %H:%M:%S"))
  single_steps = data.frame(time = df$time,
                            value = df$dataset_value)
  full_steps <- rbind(full_steps,single_steps)
}


full_steps$date <- full_steps$time
times <- strftime(full_steps$time,format="%H:%M")
full_steps$time <- as.POSIXct(times, format="%H:%M")
full_steps$day <- strftime(full_steps$date,format="%Y-%m-%d")

full_steps %>% group_by(time) %>% summarise(value = mean(value,na.rm=TRUE)) -> steps_mean
full_steps$weekdays <- weekdays(as.Date(full_steps$date))
full_steps$weekdays <- factor(full_steps$weekdays, levels= c("Monday",
                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
full_steps$dayoff <- ifelse((full_steps$weekdays == 'Saturday' |full_steps$weekdays == 'Sunday'),"Weekend", "Weekday")

full_steps %>% group_by(time,dayoff) %>% summarise(value = mean(value,na.rm=TRUE)) -> steps_mean_dayoff

steps_plot <- ggplot(subset(full_steps,full_steps$value > 0),aes(time,value)) +
  geom_line(alpha=0.2,aes(group=day)) +
  scale_y_continuous('steps') +
  geom_line(data=steps_mean_dayoff,color='#7f0000') +
  scale_x_datetime(date_label = "%H:%M") +
  theme_minimal() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        strip.text.y = element_text(size = 12)) +
  facet_grid(. ~ dayoff ) +
  geom_vline(xintercept = as.POSIXct(paste(full_steps$time[1],'09:00:00')),color='red',alpha=0.4) +
  geom_vline(xintercept = as.POSIXct(paste(full_steps$time[1],'17:00:00')),color='red',alpha=0.4)

# now merge the plots into a single one
p <- plot_grid(steps_plot,hr_plot,ncol = 1)
title <- ggdraw() + draw_label("Step & Heartrate variation over the day.
Gray lines give time series for individual days. Red line is the average (n=55 days).
Red vertical bars give 09:00 & 17:00.", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
ggsave('fitbit_intraday.pdf',width = 10, height = 3)
