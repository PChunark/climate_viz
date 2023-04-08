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
  # filter(!snow >800 & !prcp >60) %>% 
  ggplot(aes(x = prcp, y = snow, color = tmax)) +
  geom_point() +
  geom_smooth(                         #by default it is a polynomial function. 
              formula = "y~x+0", #We do a linear regression. Assumed that 0 snow is 0 precipitation. We add an intercept = 0.
              method = "lm", #this is linear regression method.
              se = FALSE
              ) +
  geom_abline(intercept = 0, slope = 10, size = 1) #This is the line that we can define an intercept and slope

cor.test(prcp_snow_daily$prcp, prcp_snow_daily$snow)
