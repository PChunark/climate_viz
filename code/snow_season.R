source("code/local_weather.R")

local_weather %>% 
  select(date, snow) %>% 
  drop_na(snow) %>% 
  mutate(cal_year = year(date),
         month = month(date)
         ) %>% 
  filter(!cal_year == 1939)  
  
