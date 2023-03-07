library(tidyverse)

#Make dataframe to get radian in circle

tibble(theta = 2 * pi * seq(0, 10, 0.05),
       radius = seq(0, 1, length.out = length(theta))) %>% #Number of radius = number of theta
  mutate(x = radius * sin(theta),
         y = radius * cos(theta)) %>% 
  ggplot(aes(x = x, y = y))+
#  geom_line() #geom_line connects the dot between plots in the order of the x-axis
  geom_path()
