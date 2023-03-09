library(tidyverse)


t_data <- read.csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na()

t_data %>% 
  filter(month == "Apr" | month == "Oct") %>% 
  pivot_wider(names_from = "month", values_from = "t_diff") %>%  # to draw a geom_segment, we need to draw 4 lines.
  mutate(ave_t = (Oct + Apr) / 2) %>%  # Make an average temperature to provide a color to a line
  ggplot(aes(x = -4 - Oct, xend = Apr, y = year, yend = year, color = ave_t)) + 
  geom_vline(xintercept = c(-5, -4, 0, 1), color = "gold") + #Creating vertical lines
  geom_segment() + 
  scale_color_gradient2(low = "darkblue" , 
                        mid = "white" , 
                        high = "darkred",
                        midpoint = 0,
                        guide = "none") 
  
  