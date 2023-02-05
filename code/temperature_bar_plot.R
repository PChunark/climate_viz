library(tidyverse)

read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>%
  drop_na() %>% # Remove any rows that have missing data
  ggplot(aes(x = year, y = t_diff))+ # Provide x and y parameter
  geom_col() #geom_bar generates a bar plot with summary build-in function, we dont need!!
