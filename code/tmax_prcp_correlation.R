source("code/local_weather.R")

tmax_prcp <- local_weather %>% 
  mutate(year = year(date)) %>%
  # drop_na(tmax,prcp) %>% #It will remove NA for all rows. But some columns might have a data. This approach is not effective
  group_by(year) %>% 
  summarize(tmax = mean(tmax, na.rm = TRUE), #Remove NA before calculation
            prcp = sum(prcp,na.rm = TRUE))
