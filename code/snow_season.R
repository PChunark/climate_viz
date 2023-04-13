source("code/local_weather.R")

snow_data <-
  local_weather %>% 
  select(date, snow) %>% 
  drop_na(snow) %>% 
  mutate(calendar_year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue("{calendar_year}-07-01")), #Snow year in USA or in Canada is in July - June
                                        calendar_year - 1,
                                        calendar_year)
         ) %>% 
  filter(!calendar_year == 1939)  %>% 
  select(month, snow_year, snow) %>% 
  filter(snow_year != 1939) 

#Plotting cumulative snowfall by year 
snow_data %>% 
  group_by(snow_year) %>% 
  summarize(total_snow = sum(snow)) %>% 
  ggplot(aes(x = snow_year, y = total_snow)) + 
  geom_line()
