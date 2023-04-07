source("code/local_weather.R")

tmax_prcp <- local_weather %>% 
  mutate(year = year(date)) %>% # Filter out the present data because the data is not complete.
  # drop_na(tmax,prcp) %>% #It will remove NA for all rows. But some columns might have a data. This approach is not effective
  filter(year != year(today())) %>% 
  group_by(year) %>% 
  summarize(tmax = mean(tmax, na.rm = TRUE), #Remove NA before calculation
            prcp = sum(prcp,na.rm = TRUE))

tmax_prcp %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~name, 
             ncol = 1,
             scales = "free_y" #Free the y scale axis
             ) +
  geom_smooth(se = FALSE)
