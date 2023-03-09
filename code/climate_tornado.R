library(tidyverse)


t_data <- read.csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na()

t_data %>% 
  filter(month == "Apr" | month == "Oct") %>% 
  pivot_wider(names_from = "month", values_from = "t_diff") # to draw a geom_segment, we need to draw 4 lines.
