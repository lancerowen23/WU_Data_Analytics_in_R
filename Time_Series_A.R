dt <- read.csv('~/Desktop/WU_Data_Analytics_in_R/traffic.csv')
dw <- read.csv('~/Desktop/WU_Data_Analytics_in_R/weather_forecast.csv')

source('~/Desktop/WU_Data_Analytics_in_R/functions_packages.R')

View(traffic)
View(weather)

head(traffic)

dt <- dt %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

dt <- dt %>% 
  mutate(day = weekdays(date),
         week = week(date),
         month = month(date))

dt %>%  
  group_by(day) %>% 
  summarise(avg_veh = mean(Vehicles))

dt %>%  
  group_by(month, day) %>% 
  summarise(avg_veh = mean(Vehicles))

dt.ts <- tsibble(dt, index = date)

dt.ts %>%  autoplot(Vehicles) +
  theme_minimal()

#### trend 
# moving average

ma.trend <- dt.ts %>% 
  mutate(ma_veh = ma(Vehicles, n = 7))

dt.ts %>%  autoplot(Vehicles) +
  autolayer(ma.trend, .vars = ma_veh, colour = "blue") +
  theme_minimal()


