library(tidyverse)


# read_csv is a function in tidyverse. read.csv is a base r function
read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>% # Change colums name
  ggplot(aes(x = year, y = t_diff)) + 
  geom_line(color = "grey", size = 0.5)+ # Add line color and size
  geom_point(fill = "white", color = "grey", shape =21)+ # Add point fill color, border color, shape of points
  geom_smooth(se = FALSE, color = "black", size = 0.5, span = 0.15)+ # smooth the data, remove standard errors, change line color and size, give smoothness using 'span' 
  scale_x_continuous(breaks = seq(1880, 2020,20), expand = c(0,0))+ #Adjust the x axis, remove space between x and y axis
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0,0))+ #Adjust the y axis
  labs(
    x = "YEAR", # X axis label
    y = "Temperature anomaly (C)", # Y axis label
    title = "GLOBAL LAND-OCEAN TEMPERATURE INDEX", #Title label
    subtitle = "Data source: NASA's Goddard Institute for Space Studies (GISS). \nCredit: NASA/GISS", #Subtitle label
  )+
  theme_light()+# Add white background
  theme(
    axis.ticks = element_blank(), # Remove tick marks from axis
    plot.title.position = "plot", # Move title and subtitle to the margin
    plot.title = element_text(margin = margin(b=10), color = "red", face = "bold"), # Add the space between title and subtitle (Margin), change text color and thichness
    plot.subtitle = element_text(size = 10, margin = margin(b=10)) #Change font size, Add space after subtitle
    )
  
read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>% # Change colums name
  ggplot(aes(x = year, y = t_diff)) + 
  geom_line(aes(color = "1"), size = 0.5)+ # Add line color and size, since color is static the dummy variable is created 'aes(color = "1")'
  geom_point(fill = "white", aes(color = "1"), shape =21, show.legend = FALSE)+ # Add point fill color, border color, shape of points, since color is static the dummy variable is created 'aes(color = "1")', remove point from legend
  geom_smooth(se = FALSE, aes(color = "2"), size = 0.5, span = 0.15)+ # smooth the data, remove standard errors, change line color and size, give smoothness using 'span', since color is static the dummy variable is created 'aes(color = "2")' 
  scale_x_continuous(breaks = seq(1880, 2020,20), expand = c(0,0))+ #Adjust the x axis, remove space between x and y axis
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0,0))+ #Adjust the y axis
  scale_color_manual(name = NULL,             # When we added the dummy 'aes(color = "1")', we need to specify the color to dummies.
                     breaks = c(1,2),         # There are 2 dummies 1,2
                     values = c("grey", "black"),   # assign color value
                     labels = c("Annual mean", "Lowess smoothing"))+ #Assign label
  labs(
    x = "YEAR", # X axis label
    y = "Temperature anomaly (C)", # Y axis label
    title = "GLOBAL LAND-OCEAN TEMPERATURE INDEX", #Title label
    subtitle = "Data source: NASA's Goddard Institute for Space Studies (GISS). \nCredit: NASA/GISS", #Subtitle label
  )+
  theme_light()+# Add white background
  theme(
    axis.ticks = element_blank(), # Remove tick marks from axis
    plot.title.position = "plot", # Move title and subtitle to the margin
    plot.title = element_text(margin = margin(b=10), color = "red", face = "bold"), # Add the space between title and subtitle (Margin), change text color and thichness
    plot.subtitle = element_text(size = 10, margin = margin(b=10)), #Change font size, Add space after subtitle
    legend.position = c(0.15, 0.9),   #Give position as a vector. it is a relative position in the x and y axis  
    legend.title = element_text(size = 0) # Remove the legend title
    )

  
ggsave("figures/temperature_index_plot.png", width = 6, height = 4) # save the figure to 'figures folder'
