source("code/local_weather.R")

threshold <- 0 

drought_by_year<-
  local_weather %>% 
  select(date, prcp) %>% 
  mutate(prcp = replace_na(prcp, 0)) %>% # other option to replace NA
  # replace_na(list(prcp = 0)) %>%  # replace NA argument should be a list
  filter(prcp > threshold) %>% 
  mutate(prev_date = lag(date, n = 1)) %>% 
  drop_na() %>% 
  mutate(drought_length = as.numeric(date-prev_date),#find a drought length
         year = year(date)) %>% 
  select(year, length = drought_length)