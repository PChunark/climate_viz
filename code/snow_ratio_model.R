source("code/local_weather.R")

prcp_snow <- local_weather %>% 
  drop_na() %>% 
  filter(snow > 0) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(prcp = sum(prcp),
            snow = sum(snow)) %>% 
  filter(year != 2023)

prcp_snow %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value))+
  geom_line() +
  facet_wrap(~name, 
             ncol = 1,
             scales = "free_y")

prcp_snow %>% 
  ggplot(aes(x = prcp, y = snow, color = year))+
  geom_point()

cor.test(prcp_snow$prcp, prcp_snow$snow)
