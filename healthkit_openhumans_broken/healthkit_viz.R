library(ggplot2)
library(tidyr)
library(reshape2)
library(dplyr)

# read dataframe, convert time zones
df <- read.csv(file='healthkitdata.csv',sep=',',head=T)
df$X <- NULL
df$date <- as.POSIXct(as.character(df$date),format="%Y-%m-%dT%H:%M:%S+00:00")

# reformat dataframe to wide
df %>% dcast(date ~ variable, fun=mean) -> df_wide

# get hours 
hours <- strftime(df_wide$date,format="%H:%M")
df_wide$time <- as.POSIXct(hours, format="%H:%M")
df_wide$day <- strftime(df_wide$date,format="%Y-%m-%d")

# get mean for each time entry

df_wide %>% group_by(time) %>% summarise(HeartRate = mean(HeartRate,na.rm=TRUE)) -> hr_mean

#plot 
ggplot(subset(df_wide,df_wide$HeartRate > 0),aes(time,HeartRate)) + 
  geom_line(aes(group=day),alpha=0.3) +
  geom_line(data=hr_mean,color='#7f0000') +
  theme_minimal()
