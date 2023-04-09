source("code/local_weather.R")

local_weather %>% 
  select(date, snow) %>% 
  drop_na(snow) %>% 
  mutate(calendar_year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue("{calendar_year}-07-01")), #Snow year in USA or in Canada is in July - June
                                        calendar_year - 1,
                                        calendar_year)
         ) %>% 
  filter(!calendar_year == 1939)  
  
