library(tidyverse)
library(gganimate)

# 1. First plot
t_diff <- 
  read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% #load CSV file, skip 1st rows, na value in the data is "***"
  select(year = Year, month.abb) %>% #month.abb is constant R build-in. which is the same as our datatype
  # Tidy dataframe by converting month in 1 column accept for year
  #Pivot dataframe for all columns except for year column, Give a name of month column
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>%
  drop_na() %>% #Ignore NA value
  mutate(month = factor(month, levels = month.abb)) #Order months into numerical order rather than alphabit order, define month as a factor
  
  
t_diff %>%
  ggplot(aes(x = month, y = t_diff, group = year, color = year)) +
  geom_line()

# 2. Second plot: There are data in preceding year and next year.
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
    x = 12,
    y = c(1.5, 2.0),
    labels = c("1.5\u00B0C", "2.0\u00B0C")
  )

#Create dataframe for tangential month to the ciricle
month_label <-
  tibble(
    x = 1:12,
    labels = month.abb,
    y = 2.7
  )

 
a <-  t_data %>% ggplot(aes(x = month_number, 
             y = t_diff, 
             group = year, 
             color = year), inherit.aes = FALSE) +
    geom_rect(aes(xmin = 1, xmax = 13, ymin = -2, ymax = 2.4), # create black circle background
              color = "black", fill = "black",
              inherit.aes = FALSE)+ 
   geom_hline(yintercept = c(1.5, 2.0), color = "red") + # Add white line at 1.5 and 2.0 y intercept
  geom_label(data = temp_line, aes(x = x, y = y, label = labels), #Add label and coloring
             color = "red", fill = "black", label.size = 0,
             inherit.aes = FALSE)+
  geom_text(data = month_label, aes(x = x, y = y, label = labels), # Add month label back to the position
            color = "white",
            inherit.aes = FALSE,
            angle = seq(360-360/12, 0, length.out = 12)) + #Assign angle to the month label
  geom_label(aes(x = 1, y = -1.3, label = year), 
               color = "white", fill = "black",
               label.padding = unit(50, "pt"), label.size = 0,
               size = 6) +
  geom_line()+

  scale_x_continuous(breaks = 1:12, #Set x axis and its coordinator
                     labels = month.abb,
                     expand = c(0,0),
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(breaks = seq(-2,2,0.2),
                     limits = c(-2,2.7),
                     expand = c(0,-0.7), # expand grid. First is an addition. Second is an multiplication.
                     sec.axis = dup_axis(name = NULL, labels = NULL)) + #Rescale y axis
   scale_color_viridis_c(breaks = seq(1880,2020,20), #Change the color to continuous scale. Re-scale the legend
                        guide = "none") + #Remove legend 
  coord_polar(start = 2*pi/12)+ #Set polar 5 minutes off. "start" measures thing in radius 
  labs(x = NULL, # Add label
      y = NULL,
      title = "Global temperature change (1880-2022)") + 
  theme(
    panel.background = element_rect(fill = "#444444", size = 1), #Add black color and white border. Increase border line size
    plot.background = element_rect(fill = "#444444", color = "#444444"),
    panel.grid = element_blank(), # Remove grid line color
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color = "white", size = 13),
    plot.title = element_text(color = "white", hjust = 0.5, size = 15)
  )+
  transition_reveal(along = step_number)  #Add data and keep the old data in gganimate only
    
animate(a, width = 4.155, height = 4.5, unit = "in", res = 300,
        nframes = nrow(t_data),
        fps = nrow(t_data)/12/60/60)
anim_save("figures/climate_spiral.gif")
