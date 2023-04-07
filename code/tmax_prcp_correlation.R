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

#Try to make a duel y axis
#Scale the tmax between 0 and 1. It called a Normalization of the data.

scaled_tmax_prcp <- tmax_prcp %>% 
  mutate(
         tmax_tr = (tmax - min(tmax))/(max(tmax)-min(tmax)),
         tmax_min = min(tmax),
         tmax_max = max(tmax), #%>%
  # summarize(min = min(tmax_tr), max = max(tmax_tr)) #Check the maximun and the minimum
         prcp_tr = (prcp - min(prcp))/(max(prcp)-min(prcp)),
         prcp_min = min(prcp),
         prcp_max = max(prcp))



tmax_plot <- scaled_tmax_prcp %>% 
  ggplot(aes(x = year, y = tmax_tr)) +
  geom_line(color = "blue")

tmax_plot + 
  geom_line(aes(y = prcp_tr), color = "red")
