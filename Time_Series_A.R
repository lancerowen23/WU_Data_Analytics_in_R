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

lm.trend <- dt.ts %>% 
  model(lin.trend = TSLM(Vehicles ~ as.numeric(date)),
        quad.trend = TSLM(Vehicles~as.numeric(date) 
                          + I(as.numeric(date)^2)))

fit.lm.trend <- lm.trend %>% 
  fitted()

lm.trend %>% glance()
lm.trend %>% coef()

dt.ts %>% autoplot(Vehicles) +
  autolayer(fit.lm.trend, .vars = .fitted) +
  autolayer(ma.trend, .vars = ma_veh, colour = "blue")  +
  theme_minimal()

dt.ts %>% 
  gg_subseries(Vehicles, period = "week")

dt.ts %>% 
  gg_lag(Vehicles, lags = 1:9, geom = "point")


### decomposition

class.dec.add <- dt.ts %>%  model(
  classical_decomposition(Vehicles, type = "additive")
) %>%  components()

class.dec.add %>% autoplot() + theme_minimal()

dt.ts %>%  model(
  classical_decomposition(Vehicles, type = 'multiplicative')
) %>% components() %>% autoplot()


dt.ts %>% model(
  STL(Vehicles~ trend(window = 7) +
        season(window = 'periodic'))
) %>% components() %>%  autoplot() + theme_minimal()
