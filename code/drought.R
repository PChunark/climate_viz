source("code/local_weather.R")
library(slider)
# Update environment in r
# renv::snapshot()



local_weather %>%
  select(date, prcp) %>% 
  mutate(prcp = if_else(is.na(prcp), 0, prcp)) %>% 
  mutate(one_day_lag = lag(prcp), # Add a lag data to the dataframe by one day
         two_day_lag = lag(prcp, n = 2),
         one_day_lead = lead(prcp), # Add a lag data to the dataframe by one day
         two_day_lead = lead(prcp, n = 2)
)

# How to use slider! It is a function to window (snapshot) a data
x <- 1:10
slider(x, 
       ~.x,
       .before = 1)