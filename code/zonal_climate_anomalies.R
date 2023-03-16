library(tidyverse)

#The latitude zones are 90N-64N, 64N-44N, 44N-24N, 24N-EQU, EQU-24S, 24S-44S, 44S-64S, 64S-90S.
bands <- rev(c("64N-90N", "44N-64N", "24N-44N", "EQU-24N", 
          "24S-EQU", "44S-24S", "64S-44S", "90S-64S")) #Rev argument is to reverse the vector

# the original plot is from NASA: https://svs.gsfc.nasa.gov/4978
url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"

zone_data <- read_csv(url) %>% 
  select(year = Year,
         all_of(bands)) %>% 
  pivot_longer(-year, names_to = "zone", values_to = "t_diff") %>% 
  mutate(zone = factor(zone, levels = bands ),
         zone_position = as.numeric(zone))

#Add data for current year
current_year <-
  zone_data %>% 
  filter(year == 2021)

zone_data %>% 
  ggplot(aes(x = t_diff, xend = t_diff, 
             y = zone_position - 0.25, yend = zone_position+0.25)) + 
  geom_segment(color = "white", 
               alpha = 0.25)+ #Adjust the transparency
  geom_segment(data = current_year, #Add a current year to the plot with color
               aes(color = t_diff),
               size = 2,
               lineend = "round") + #Round the line end
  scale_y_continuous(breaks = 1:8, # Break y axis into 8 parts 
                     labels = bands) + #Give the labels to y axis equal to bands
  scale_x_continuous(breaks = seq(-3 ,4, 1),
                     labels = seq(-3 ,4, 1),
                     limits = c(-3, 4)) +
  scale_color_gradient2(low = "darkblue", #Set color for a current year data
                       mid = "white",
                       high = "darkred",
                       midpoint = 0,
                       guide = "none") + #remove legend
  labs(x = "Temperature anomaly (\u00B0C)",
       y = NULL,
       title = "Variation in annual temperature anomaly by\nlatitude (1880-2021)",
       subtitle = "Bars for 2021 are colored by the size of the anomaly") +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white", face = "bold"),
    plot.title.position = "plot", #give a title on y axis
    plot.subtitle = element_text(color = "grey", size = 8),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    panel.grid.major.x = element_line(color = "gray", size = 0.25),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("figures/latitude_anomaly.png", width = 4, height = 3, units = "in")
