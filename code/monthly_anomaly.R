library(tidyverse)

month_anom <- 
  read_table(file = "data/merra2_seas_anom.txt", skip = 3) %>% 
  select(month = Month, seas_anom) %>% 
  mutate(month = as.numeric(month),
         month = month.abb[month])

t_data <- read_csv(file = "data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na() %>%  
  inner_join(., month_anom, by = "month") %>%  #Join 2 dataframes by months
  mutate(month = factor(month, levels = month.abb),
         month_anom = t_diff + seas_anom - 0.7) %>%  #Calculate month anom to generate a line plot
  group_by(year) %>% 
  mutate(ave = mean(month_anom)) %>% #get a temperature to map a color to, an average temperature is calculated
  ungroup() %>% 
  mutate(ave = if_else(year == 2022, max(abs(ave)),ave))
  
annotation <-
  t_data %>% 
  slice_tail(n = 1)
  
  
t_data %>% 
  ggplot(aes(x = month,
             y = month_anom,
             group = year,
             color = ave)) + 
  geom_line() + 
  geom_point(data = annotation, 
             aes(x = month, y = month_anom)) +#Adding in last point data to a plot. We need additional dataframe
  geom_text(data = annotation, 
            aes(x = 11.7, y = month_anom), 
            label = "December 2022",
            hjust = 1)+
  scale_color_gradient2(low = "darkblue",
                        mid = "white",
                        high = "darkred",
                        midpoint = 0,
                        guide = "none") +
  scale_y_continuous(breaks = seq(-3,2,1)) +
  scale_x_discrete(expand = c(0,0.1))+ # Remove space between a line plot and axises
  labs(
    x = NULL,
    y = NULL,
    title = "Temperature Anomaly (\u00B0 C)",
    subtitle = "(Difference from 1980-2015 annual mean)"
  )+
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray",
                                      linetype = "dotted",
                                      size = 0.25),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray", 
                                 size = 10),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10) #Give a margin to a plot
  )

ggsave("figures/monthly_anomaly.png", width = 6, height = 4, units = "in")
