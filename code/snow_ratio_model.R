source("code/local_weather.R")

#Create annual precipitation and snow
prcp_snow_annual <- local_weather %>% 
  drop_na() %>% 
  filter(snow > 0) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(prcp = sum(prcp),
            snow = sum(snow)) %>% 
  filter(year != 2023)

prcp_snow_annual %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value))+
  geom_line() +
  facet_wrap(~name, 
             ncol = 1,
             scales = "free_y")

prcp_snow_annual %>% 
  ggplot(aes(x = prcp, y = snow, color = year))+
  geom_point()

cor.test(prcp_snow_annual$prcp, prcp_snow_annual$snow)

#Create an daily precipitation and snow
prcp_snow_daily <- local_weather %>% 
  drop_na() %>% 
  filter(snow > 0) %>% 
  mutate(year = year(date)) 
  # filter(year != 2023) 
  
  
prcp_snow_daily%>% 
  ggplot(aes(x = prcp, y = snow, color = tmax)) +
  geom_point()

cor.test(prcp_snow_daily$prcp, prcp_snow_daily$snow)
