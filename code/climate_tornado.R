library(tidyverse)

#Provide grid labels

grid_labels <- tibble(
  x = c(-5, -4, 0, 1),
  y = 2030,
  labels = c("+1 \u00B0C", "0 \u00B0C", "0 \u00B0C", "+1 \u00B0C")
  )

#Make year labels
year_labels <- tibble(
  x = -2,
  y = c(seq(1880, 2000, by = 20), 2021)
  )

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
  geom_label(data = grid_labels,
             aes(x = x,
                 y = y,
                 label = labels),
             inherit.aes = FALSE,
             fill = "black", # fill background color
             color = "gold", # text color
             label.size = 0, #border edge size
             size = 3)+ #give font size
  geom_segment(size = 0.9, lineend = "round") + # Increase the thickness of the line. Round the end of the line
  geom_text(data = year_labels, 
            aes(x = x, 
                y = y,
                label = y),
            inherit.aes = FALSE,
            color = "gold",
            size = 3,
            fontface = "bold") +
  scale_color_gradient2(low = "darkblue" , 
                        mid = "white" , 
                        high = "darkred",
                        midpoint = 0,
                        guide = "none") +
  scale_y_continuous(limits = c(NA, 2030), expand = c(0,0)) + # limiting the vertical lines at bottom data level
  labs(x = NULL,
       y = NULL,
       title = NULL) + 
  coord_cartesian(clip = "off") + #turn off the clip that exist over the plot
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("figures/climate_tornado.png", width = 4.5, height = 3.5, units = "in")  
  