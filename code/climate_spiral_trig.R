library(tidyverse)
library(gganimate)



# Plot: There are data in preceding year and next year.
# Need to create 3 separated dataframes

t_diff <- 
  read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% #load CSV file, skip 1st rows
  select(year = Year, month.abb) %>% #month.abb is constant R build-in. which is the same as our datatype
  # Tidy dataframe by converting month in 1 column accept for year
  #Pivot dataframe for all columns except for year column, Give a name of month column
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>%
  drop_na() #Ignore NA value
  

# Create 3 Separated dataframe
# Create dataframe for preceding December
# last_dec <-
#   t_diff %>%
#   filter(month == "Dec") %>% #Filter December
#   mutate(year = year + 1,   #Calculate preceding year by using the original data
#          month = "last_Dec") #Rename Dec to last_Dec

# Create dataframe for next Jan
# next_jan <-
#   t_diff %>%
#   filter(month == "Jan") %>% #Filter January
#   mutate(year = year - 1,    # Add extra year
#          month = "next_Jan") # Rename it

# Create dummy variable for radius
radius_bumb <- 1.5

t_data <- 
  t_diff %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")), #Order months into numerical order rather than alphabet order, define month as a factor
         month_number = as.numeric(month)) %>% #Remove previous December to remove zero from dataframe
  arrange(year, month) %>%  # arrange year and month       
  mutate(step_number = 1:nrow(.), #Add row number. 1:nrow(.) means count from 1 to the end of row in this dataframe. need to count step over the row
         radius = t_diff + radius_bumb,         theta = 2 * pi * (month_number-1) / 12, # Convert an angle to radian. Theta starts from zero degree January will be zero and divided by 12 to give angle to months. 
         x = radius * sin(theta),
         y = radius * cos(theta))
  
  annotation <-
    t_data %>% #create new dataframe from 2022
    slice_max(year) %>%
    slice_max(month_number)
 
temp_line <- #Create text at specific position
  tibble(
    x = 0,
    y = c(1, 0, -1) + radius_bumb,
    labels = c("+1\u00B0 C","0\u00B0 C", "-1.0\u00B0 C")
  )

#Create dataframe for tangential month to the ciricle
month_label <-
  tibble(
    theta = 2 * pi * (1:12 - 1)/12,
    radius = 1.5 + radius_bumb,
    labels = toupper(month.abb), #Convert month label to capital letters
    x = radius * sin(theta),
    y = radius * cos(theta)
    )

#Make dataframe for grid line and used in geom_segment
# gridlines <- 
#   tibble(
#     x = c(1.2, 1.3, 1.6),
#     xend = c(12.8, 12.7, 12.4),
#     y = c(1, 0, -1),
#     yend = y
#   )
 
# Adding gridlines to climate spiral 
gridlines <-
  tibble(theta = 2 * pi * rep(seq(0, 1, 0.01), each = 3), #Repeat theta
       radius = rep(c(1, 0, -1) + radius_bumb, length.out = length(theta)),
       line = rep(c("a", "b", "c"), length.out = length(theta)),
       x = radius * cos(theta),
       y = radius * sin(theta))
 

#a <-  
  t_data %>% ggplot(aes(x = x, 
             y = y, 
             color = t_diff)) + #Let's coloring by temperature change. Previous version was colored by year
  geom_label(aes(x = 0, y = 0, label = year),  #geom_label it provides a text with background
               fill = "black",
               label.size = 0,
               size = 6) +
  geom_path()+  
  geom_path(data = gridlines %>% filter(radius != radius_bumb), # radius should not be equal to dummy radius bumb 
               aes(x = x, y = y,
                   group = line), 
               color = "yellow",
               inherit.aes = FALSE) +
  geom_path(data = gridlines %>% filter(radius == radius_bumb), # radius should not be equal to dummy radius bumb 
              aes(x = x, y = y,
                  group = line), 
              color = "green",
              inherit.aes = FALSE) +   
  geom_text(data = temp_line, aes(x = x, y = y, label = labels), #Add label and coloring
               color = c("yellow", "green", "yellow"), size = 2, fontface = "bold",
               inherit.aes = FALSE)+
  geom_text(data = month_label, aes(x = x, y = y, label = labels), # Add month label back to the position
              color = "yellow",
              inherit.aes = FALSE) +
  scale_y_continuous(limits = c(-4, 4),
                     expand = c(0,-0.3) # expand grid. First is an addition. Second is an multiplication.
                     ) + #Rescale y axis
  scale_x_continuous(limits = c(-4, 4),
                     expand = c(0,-0.3) # expand grid. First is an addition. Second is an multiplication.
    ) + #Rescale x axis
  coord_fixed()+ # Make a proportion of an x and y fixed  
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, #Change the color to continuous scale. Re-scale the legend
                        guide = "none") + #Remove legend 
  # coord_polar(start = 0)+ #Set polar 5 minutes off. "start" measures thing in radius 
  labs(x = NULL, # Add label
      y = NULL,
      title = NULL) + 
  theme(
    panel.background = element_rect(fill = "black"), #Add black color and white border. Increase border line size
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid = element_blank(), # Remove grid line color
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank()
  )#+
   #transition_manual(frames = year, cumulative = TRUE)  #Add data and keep the old data in gganimate only
   
ggsave("figures/climate_spiral_trig.png", width = 4.155, height = 4.5, unit = "in", dpi = 300)
 
# animate(a, width = 4.155, height = 4.5, unit = "in", res = 300)
# anim_save("figures/climate_spiral_trig.gif")

# animate(a, width = 4.155, height = 4.5, unit = "in", res = 300,
#         renderer = av_renderer("figures/climate_trig.mp4")
# )
