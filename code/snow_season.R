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

# Create dummy dataframe for NA value
dummy_df <-

    crossing(snow_year = 1940:2022,
             month = 1:12) %>%
    mutate(dummy = 0)

#Take the total snow from snow_year
total_snow <-
  snow_data %>% 
  group_by(snow_year) %>% 
  summarize(total_snow = sum(snow)) %>% 
  filter(snow_year == year(today())-1) %>% 
  mutate(total_snow = total_snow/10) %>% 
  pull(total_snow)

# Plotting snowfall by year and month
snow_data %>%
  right_join(., dummy_df, by = c("snow_year", "month")) %>%
  mutate(snow = if_else(is.na(snow), dummy, snow)) %>% 
  group_by(snow_year, month) %>% 
  summarize(snow = sum(snow), .groups = "drop") %>% 
  mutate(month = factor(month, c(8:12,1:7)),
         is_this_year = year(today())-1 == snow_year) %>%
  ggplot(aes(x = month, y = snow, group = snow_year, color = is_this_year)) +
  geom_line(show.legend = FALSE) + 
  scale_color_manual(name = NULL,
                     breaks = c(TRUE,FALSE),
                     values = c("dodgerblue", "grey")
                     ) +
  scale_x_discrete(breaks = c(9, 11, 1, 3, 5), #Add month label as an abbreviation
                   labels = month.abb[c(9, 11, 1, 3, 5)],
                   expand = c(0,0)) + # Remove the space between grid 
  scale_y_continuous(breaks = seq(0,2000,500), #Label the y axis from mm to cm
                     labels = seq(0,200,50),
                     limits = c(0,2000,500)) + 
  labs(x = NULL,
       y = "Total monthly snowfall (cm)") + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line())

ggsave("figures/snow_by_snow_year.png", width = 6, height = 4)
