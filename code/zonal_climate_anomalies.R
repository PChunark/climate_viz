library(tidyverse)

#The latitude zones are 90N-64N, 64N-44N, 44N-24N, 24N-EQU, EQU-24S, 24S-44S, 44S-64S, 64S-90S.
bands <- c("64N-90N", "44N-64N", "24N-44N", "EQU-24N", 
          "24S-EQU", "44S-24S", "64S-44S", "90S-64S")

# the original plot is from NASA: https://svs.gsfc.nasa.gov/4978
url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"

zone_data <- read_csv(url) %>% 
  select(year = Year,
         all_of(bands)) %>% 
  pivot_longer(-year, names_to = "zone", values_to = "t_diff") %>% 
  mutate(zone = factor(zone, levels = bands ))

zone_data %>% 
  ggplot(aes(x = t_diff, 
             y = zone)) + 
  geom_point()
