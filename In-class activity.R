#load packages
library(brms) #for assessing model in bayesian framework
library(MASS) #for negative binomial
library(marginaleffects) #marginal effects plot
library(modelr) 
library(bayesplot) #
library(performance) #for mae

#Question 1. Does sound from boats influence beluga whale song duration and frequency (total calls/hr)? 
#Question 2. Do boats have other direct impacts on whale calling behavior, outside of their noise impacts?

#load data and check data
record_dat <- read.csv("/Users/hannahrudd/Documents/BSU/Spring 2025 Courses - BSU/EEB 622/R/In-class Activity/recordings.csv")
View(record_dat)
head(record_dat)

sensor_dat <- read.csv("/Users/hannahrudd/Documents/BSU/Spring 2025 Courses - BSU/EEB 622/R/In-class Activity/sensorinfo.csv")
View(sensor_dat)
head(sensor_dat)

beluga_dat <- merge(record_dat,sensor_dat,by="sensorid", all = TRUE)
View(beluga_dat)

#combine datasets for analyzing
beluga_dat$sensorid <- as.factor(beluga_dat$sensorid)
head(beluga_dat) #check new dataset


#glmm in bayesian framework using negative binomial distribution for total song count
totsongs_mod <- brm(
  totsongs ~ boatactivity + distshore + waterdepth + (1 | sensorid) + (1|dayid),
  data = beluga_dat,
  family = negbinomial()
)

#check model fit
summary(totsongs_mod) #good, chains completed, rhat = 1
pp_check(totsongs_mod) + xlim(c(0,50)) #real data seems to match predicted values from posterior
plot(totsongs_mod) #good, chains converged

bayes_R2(totsongs_mod) #Our model explains 58% of the variation in song counts (boatactivity + distshore + waterdepth, sensorid and dayid included as interceptes)
performance_mae(totsongs_mod) #Total song count is off by 24 units, this is low because total song counts range from 0 to 777
min(record_dat$totsongs) # min = 0
max(record_dat$totsongs) # min = 777
pp_check(totsongs_mod) + theme_bw() + xlim(c(0,50)) #real data matches predicted posterior values well

#interpret results from this model
plot_predictions(totsongs_mod, condition = "boatactivity")
mcmc_plot(totsongs_mod)
conditional_effects(totsongs_mod) #same as plot predictions

#glmm in bayesian framework using gamma distribution for song length
songlength_mod <- brm(
  songlength ~ boatactivity + distshore + waterdepth + (1 | sensorid) + (1|dayid),
  data = beluga_dat,
  family = "Gamma"(link=log)
)

#check model fit
summary(songlength_mod) #good, chains completed, rhat = 1
pp_check(songlength_mod) + xlim(c(0,50)) #real data seems to match predicted values from posterior
plot(songlength_mod) #good, chains converged

bayes_R2(songlength_mod) #Our model explains 34% of the variation in song length (songlength~boatactivity + distshore + waterdepth, sensorid and dayid included as varying interceptes)
performance_mae(songlength_mod) #Song length is off by 9.93 units, this is low because total song counts range from 0 to 167
min(record_dat$songlength) # min = 0
max(record_dat$songlength) # min = 167
pp_check(songlength_mod) + theme_bw() + xlim(c(0,50)) #real data matches predicted posterior values well

#interpret results from this model
plot_predictions(songlength_mod, condition = "boatactivity")
mcmc_plot(songlength_mod)
conditional_effects(songlength_mod)

print(predictions(songlength_mod, newdata = datagrid(boatactivity = seq_range(beluga_dat$boatactivity, 19)),
            conf_level=.95))
#song length decreased from 58.18 when boat activity was 1, to a song length of 2.03 when boat activity was 19.

predictions(totsongs_mod, newdata = datagrid(boatactivity = seq_range(beluga_dat$boatactivity, 19)),
            conf_level=.95)
#total song count decreased from ~287 when boat activity was 1 to ~0 when boat activity was 19.

#can also use this function if want to specify value
print(predictions(songlength_mod, 
                  newdata = datagrid(boatactivity = 19),
                  conf_level = 0.95))
print(predictions(songlength_mod, 
                  newdata = datagrid(boatactivity = 1),
                  conf_level = 0.95))


#glmm in bayesian framework using gamma distribution, effect of noise on whale call length and count
#for whale song length
length_noisemod <- brm(
  songlength ~ boatnoise + watertemp + (1 | sensorid) + (1|dayid),
  data = beluga_dat,
  family = "Gamma"(link=log)
)

summary(length_noisemod)

#for whale song count
count_noisemod <- brm(
  totsongs ~ boatnoise + watertemp + (1 | sensorid)+(1|dayid),
  data = beluga_dat,
  family = negbinomial()
)

summary(count_noisemod)











