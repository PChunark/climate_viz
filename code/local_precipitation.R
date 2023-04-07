source("code/local_weather.R")

this_year <- year(today()) #Create an automate year

local_weather %>% 
  select(date, prcp) %>%
  drop_na(prcp) %>% #drop NA value in prcp
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         is_this_year = year == this_year) %>% #Check whether "year" is equal to "this_year". It return TRUE & FALSE value. It is a conditional command.
  filter(!(month == 2 & day == 29)) %>% #Remove February 29th. Remove leap day
  group_by(year) %>% #Calculate cumulative prcp by year
  arrange(date) %>%  
  mutate(cum_prcp = cumsum(prcp)) %>%  #Calculate cumulative prcp by year
  ungroup() %>% 
  mutate(new_date = ymd(glue("2022-{month}-{day}"))) %>%  #Create Pseudo date???
  ggplot(aes(x = new_date, y = cum_prcp, group = year, 
             color = is_this_year,
             size = is_this_year))+
  geom_step(show.legend = FALSE) + #Remove legend
  scale_color_manual(breaks = c(F,T), #Give the color of lines
                     values = c("lightgrey", "dodgerblue")) + 
  scale_size_manual(breaks = c(F,T),
                    values = c(0.3,0.5)) + #Adjust the size of lines
  scale_x_date(date_labels = "%B", #Give a custom month label
               date_breaks = "2 months") + #Break months into 2 months
  scale_y_continuous(breaks = seq(0,2200,200), #Break the y scale
                     labels = seq(0,220,20), #Label the y axis into centimeters
                     limits = c(0, 2200), #Give the limit to an axis
                     expand = c(0,0)) + #Remove the space between an axis
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line()
       ) +
  labs(
    x = NULL,
    y = "Cumulative precipitation (cm)"
  )
  
ggsave("figures/cumulative_prcp.png", width = 6, height = 5)
