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
  
  
  
  
t_data %>% 
  ggplot(aes(x = month,
             y = month_anom,
             group = year,
             color = ave)) + 
  geom_line() + 
  scale_color_gradient2(low = "darkblue",
                        mid = "white",
                        high = "darkred",
                        midpoint = 0,
                        guide = "none")
