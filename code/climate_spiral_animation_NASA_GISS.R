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
next_jan <-
  t_diff %>%
  filter(month == "Jan") %>% #Filter January
  mutate(year = year - 1,    # Add extra year
         month = "next_Jan") # Rename it

#Combine 3 dataframes
t_data <- 
  bind_rows(t_diff,next_jan) %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")), #Order months into numerical order rather than alphabet order, define month as a factor
         month_number = as.numeric(month)) %>% #Remove previous December to remove zero from dataframe
  arrange(year, month) %>% # arrange year and month       
  filter(year != 1879) %>% # we dont need 1879
  mutate(step_number = 1:nrow(.)) #Add row number. 1:nrow(.) means count from 1 to the end of row in this dataframe. need to count step over the row
  
  
  annotation <-
    t_data %>% #create new dataframe from 2022
    slice_max(year) %>%
    slice_max(month_number)
 
temp_line <- #Create text at specific position
  tibble(
    x = 1,
    y = c(1, 0, -1),
    labels = c("+1\u00B0C","0\u00B0C", "-1.0\u00B0C")
  )

#Create dataframe for tangential month to the ciricle
month_label <-
  tibble(
    x = 1:12,
    labels = toupper(month.abb), #Convert month label to capital letters
    y = 1.5
  )

#Make dataframe for grid line and used in geom_segment
gridlines <- 
  tibble(
    x = 1.5,
    xend = 12.5,
    y = c(1, 0, -1),
    yend = y
  )
 
# a <-  
  t_data %>% ggplot(aes(x = month_number, 
             y = t_diff, 
             group = year, 
             color = t_diff), #Let's coloring by temperature change. Previous version was colored by year
             inherit.aes = FALSE) +
    # geom_rect(aes(xmin = 1, xmax = 13, ymin = -2, ymax = 2.4), # create black circle background
    #           color = "black", fill = "black",
    #           inherit.aes = FALSE)+ 
    geom_label(aes(x = 1, y = -1.3, label = year),  #geom_label it provides a background
               fill = "black",
               label.size = 0,
               size = 6) +
  geom_line()+
  geom_segment(data = gridlines, # Add yellow line at -1,0,1 y intercept, use geom segment and make new dataframe
               aes(x = x, y = y,
                   xend = xend, yend = yend), 
               color = c("yellow", "green", "yellow"),
               inherit.aes = FALSE) + 
  geom_text(data = temp_line, aes(x = x, y = y, label = labels), #Add label and coloring
               color = c("yellow", "green", "yellow"),
               inherit.aes = FALSE)+
  geom_text(data = month_label, aes(x = x, y = y, label = labels), # Add month label back to the position
              color = "yellow",
              inherit.aes = FALSE#,
              #angle = seq(360-360/12, 0, length.out = 12) #Assign angle to the month label
    ) +
  # scale_x_continuous(breaks = 1:12, #Set x axis and its coordinator
  #                    labels = month.abb,
  #                    expand = c(0,0),
  #                    sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(limits = c(-1.5, 1.5),
                     expand = c(0,-0.7) # expand grid. First is an addition. Second is an multiplication.
                     ) + #Rescale y axis
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, #Change the color to continuous scale. Re-scale the legend
                        guide = "none") + #Remove legend 
  coord_polar(start = 0)+ #Set polar 5 minutes off. "start" measures thing in radius 
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
  # transition_manual(frames = year, cumulative = TRUE)  #Add data and keep the old data in gganimate only
   
ggsave("figures/climate_spiral_nasa.png", width = 4.155, height = 4.5, unit = "in", dpi = 300)
 
# animate(a, width = 4.155, height = 4.5, unit = "in", res = 300
#         # nframes = nrow(t_data),
#         # fps = nrow(t_data)/12/60/60
#         )
# anim_save("figures/climate_spiral.gif")
# 
# animate(a, width = 4.155, height = 4.5, unit = "in", res = 300,
#         renderer = av_renderer("figures/climate_spiral.mp4")
# )
