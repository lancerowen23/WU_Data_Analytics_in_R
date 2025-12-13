ts <- read.csv('~/Desktop/WU_Data_Analytics_in_R/TS_daily.csv')

## convert date to date object 

ts = ts %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

## create features 

ts = ts %>%
  mutate(
    day = weekdays(date), 
    week = week(date), 
    month = month(date)
  )

head(ts)
View(ts)


ts = ts %>%
  mutate(
    day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", 
                                 "Thursday", "Friday", 
                                 "Saturday", "Sunday")), 
    week = factor(week), 
    month = factor(month)
  )

### working with time series, we need dataset indexed by time 

dt.ts = tsibble(ts, index = date)
View(dt.ts)

# quick visualization
dt.ts %>% autoplot(inboundc) + theme_minimal()
