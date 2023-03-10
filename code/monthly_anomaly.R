library(tidyverse)

month_anom <- 
  read_table(file = "data/merra2_seas_anom.txt", skip = 3) %>% 
  select(month = Month, seas_anom) %>% 
  mutate(month = as.numeric(month),
         month = month.abb[month])
