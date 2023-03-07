library(tidyverse)

#Make dataframe to get radian in circle

tibble(theta = 2 * pi * seq(0, 1, 0.05),
       radius = 1) %>% 
  ggplot(aes(x = theta, y = radius))+
  geom_line()
