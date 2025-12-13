rm(list=ls())

dt <- read.csv('~/Desktop/WU_Data_Analytics_in_R/traffic.csv')
dw <- read.csv('~/Desktop/WU_Data_Analytics_in_R/weather_forecast.csv')

source('~/Desktop/WU_Data_Analytics_in_R/functions_packages.R')

### visualize dt 

head(dt)


## convert date to date object 

dt = dt %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

## create features 

dt = dt %>%
  mutate(
    day = weekdays(date), 
    week = week(date), 
    month = month(date)
  )

head(dt)


## summarize values 

dt %>% 
  group_by(day) %>%
  summarise(avg_veh = mean(Vehicles))

## reorder and apply factors 

dt = dt %>%
  mutate(
    day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", 
                                 "Thursday", "Friday", 
                                 "Saturday", "Sunday")), 
    week = factor(week), 
    month = factor(month)
  )


dt %>% 
  group_by(day) %>%
  summarise(avg_veh = mean(Vehicles))


### working with time series, we need dataset indexed by time 

dt.ts = tsibble(dt, index = date)

## important if we want to summarize values we need to transform it back to normal
## table 

dt.ts %>% 
  as_tibble() %>%
  group_by(day) %>%
  summarise(avg_veh = mean(Vehicles))

## visualize timeseries

dt.ts %>% autoplot(Vehicles) + theme_minimal()


##### we can visualize the trend

## MA 

ma.trend = dt.ts %>%
  mutate(ma = ma(Vehicles, 7))


dt.ts %>% autoplot(Vehicles) +
  autolayer(ma.trend, .vars = ma, colour = "red") +
  theme_minimal()


## estimate linear and quadratic trend 

tslm.trend = dt.ts %>% model(
  lin.trend = TSLM(Vehicles ~ as.numeric(date)), 
  quad.trend = TSLM(Vehicles ~ as.numeric(date)+I(as.numeric(date)^2)), 
)


## visualize trend fit 

tslm.trend %>% glance()

## extract est. coef 

tslm.trend %>% coef()


## visualize on top of time series 

tslm.fit = tslm.trend %>% fitted()

dt.ts %>% autoplot(Vehicles) +
  autolayer(tslm.fit, .vars = .fitted) +
  theme_minimal()


## visualize seasonality and lag components 

dt.ts %>% 
  gg_subseries(y=Vehicles, period = "week") +
  theme_minimal()

dt.ts %>% 
  gg_lag(y=Vehicles, lags = 1:9, geom = "point")+
  theme_minimal()


dt.ts %>% 
  ggplot(aes(x=day, y=Vehicles))+
  geom_boxplot()+
  theme_minimal()


dt.ts %>% 
  ggplot(aes(x=day, y=Vehicles, fill = month))+
  geom_boxplot()+
  theme_minimal()


dt.ts %>% ACF(Vehicles) %>% autoplot() + theme_minimal()
dt.ts %>% PACF(Vehicles) %>% autoplot() + theme_minimal()


################################################################################
################################################################################

## decomposition 

dt.ts %>% model(
  classical_decomposition(Vehicles, type = "additive")
) %>% components() %>% autoplot() + theme_minimal()



decomps = dt.ts %>% model(
  add.class = classical_decomposition(Vehicles, type = "additive"), 
  multi.class = classical_decomposition(Vehicles, type = "multiplicative")
) %>% components()


decomps = decomps %>%
  mutate(
    rec_series = ifelse(.model == "add.class", trend+seasonal, trend*seasonal)
  )

dt.ts %>% autoplot(Vehicles) +
  autolayer(decomps, .vars = rec_series) +
  theme_minimal()


#### we add stl 

decomps.v2 = dt.ts %>% model(
  add.class = classical_decomposition(Vehicles, type = "additive"), 
  multi.class = classical_decomposition(Vehicles, type = "multiplicative"), 
  stl.dec = STL(Vehicles ~ trend(window = 7) + season(window = 7))
) %>% components()

decomps.v2 = decomps.v2 %>%
  mutate(
    rec_series = case_when(
      .model == "add.class" ~ trend+seasonal,
      .model == "multi.class" ~ trend*seasonal, 
      .model == "stl.dec" ~ trend+season_week
    )
  )

dt.ts %>% autoplot(Vehicles) +
  autolayer(decomps.v2, .vars = rec_series) +
  theme_minimal()


################################################################################
################################################################################


### we can identify the outliers using the residuals from stl 

stl.dec = decomps.v2 %>% filter(.model == "stl.dec")


ggplot(stl.dec, aes(x=remainder))+
  geom_boxplot()+
  theme_minimal()


stl.dec = stl.dec %>%
  mutate(
    q1 = quantile(remainder, probs = 0.25), 
    q3 = quantile(remainder, probs = 0.75), 
    flag = remainder<q1-1.5*(q3-q1)|remainder>q3+1.5*(q3-q1)
  )

table(stl.dec$flag)

stl.dec[stl.dec$flag, ]

################################################################################
################################################################################

## forecast next 7 days 

forecast.h = 7

### split in test and train 

dt.train = dt.ts %>% filter(date<=max(date)-forecast.h)
dt.test = dt.ts %>% filter(date>max(date)-forecast.h)


### estimate models 

mods = dt.train %>% model(
  exp.smooth = ETS(Vehicles ~ error("A") + trend("N") + season("N")), 
  holt = ETS(Vehicles ~ error("A") + trend("A") + season("N")), 
  holt_wint = ETS(Vehicles ~ error("A") + trend("A") + season("A")), 
  arima = ARIMA(Vehicles)
)


## we can inspect models

mods %>% select(holt_wint) %>% glance()
mods %>% select(holt_wint) %>% coef()

## we can inspect diagnostic 

mods %>% select(holt_wint) %>%
  gg_tsresiduals() +
  theme_minimal()


### we can inspect fit visually 

mods.fit = mods %>% fitted()

dt.train %>% autoplot(Vehicles) +
  autolayer(mods.fit, .vars = .fitted) +
  theme_minimal()


## and compute accuracy measures 
# on train
accuracy(mods)


## and on test 

pred = mods %>% forecast(h=forecast.h)

pred.ci95 = pred %>% hilo(level = 0.95)

accuracy(pred, dt.test)


### we refit using all observations with the best model 

mod.refit = mods %>% refit(dt.ts) 

## get forecast with holt winters 

pred.holt = mod.refit %>% select(holt_wint) %>%
  forecast(h=forecast.h)


pred.holt %>%
  autoplot(level = 95) + theme_minimal()


## adding exogenous variables 

dt.weather = read.csv(paste0(data.dir, "weather_forecast.csv"))

head(dt.weather)

dt.weather = dt.weather %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"), 
         weather.for = as.factor(weather.for))

dt.ts.full = full_join(dt.ts, dt.weather, by = c("date", "Junction"))


mods.v2 = dt.ts.full %>%
  model(
    arima = ARIMA(Vehicles), 
    arimax = ARIMA(Vehicles ~ weather.for)
  )

mods.v2 %>% select(arima) %>% report()

mods.v2 %>% coef()

