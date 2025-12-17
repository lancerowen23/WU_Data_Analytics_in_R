## load packages and functions 

source("C:\\Users\\aless\\OneDrive\\Desktop\\Vienna\\Upload\\functions_packages v2.R")

## load dataset
dt.patient = read.xlsx("C:\\Users\\aless\\OneDrive\\Desktop\\Vienna\\Upload\\Monte Carlo\\Afternoon\\PullPill_Input_red.xlsx", 
                       sheet = "Patients t0")

## plot the histogram of variable
ggplot(dt.patient, aes(x=Patients.estimated, y=after_stat(density)))+
  geom_histogram()+
  theme_minimal()

## compute sample mean and sample standard deviation
sample.mean = mean(dt.patient$Patients.estimated)
sample.sd = sd(dt.patient$Patients.estimated)

## plot histogram of the variable and 
## normal distribution with mu=sample mean and 
## sd=sample sd
ggplot(dt.patient, aes(x=Patients.estimated))+
  geom_histogram(aes(y=after_stat(density)))+
  stat_function(fun=dnorm, args = list(mean = sample.mean, sd = sample.sd))+
  theme_minimal()

## test whether the sample comes from a normal 
## distribution with mu = to the sample mean
## and sigma = to the sample sd 
ks.test(x=dt.patient$Patients.estimated, "pnorm", sample.mean, sample.sd)
