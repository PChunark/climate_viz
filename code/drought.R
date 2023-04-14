source("code/local_weather.R")

local_weather %>%
  select(date, prcp) %>% 
  mutate(prcp = if_else(is.na(prcp), 0, prcp)) %>% 
  mutate(one_day_lag = lag(prcp), # Add a lag data to the dataframe by one day
         two_day_lag = lag(prcp, n = 2),
         one_day_lead = lead(prcp), # Add a lag data to the dataframe by one day
         two_day_lead = lead(prcp, n = 2)
)
