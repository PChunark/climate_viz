source("code/local_weather.R")

local_weather %>% 
  select(date, prcp) %>%
  drop_na(prcp) %>% #drop NA value in prcp
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  filter(!(month == 2 & day == 29)) %>% #Remove February 29th. Remove leap day
  group_by(year) %>% #Calculate cumulative prcp by year
  arrange(date) %>%  
  mutate(cum_prcp = cumsum(prcp)) %>%  #Calculate cumulative prcp by year
  ungroup()
