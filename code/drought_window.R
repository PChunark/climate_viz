source("code/local_weather.R")

threshold <- 0 

local_weather %>% 
  select(date, prcp) %>% 
  mutate(prcp = replace_na(prcp, 0)) %>% # other option to replace NA
  # replace_na(list(prcp = 0)) %>%  # replace NA argument should be a list
  filter(prcp > threshold)
