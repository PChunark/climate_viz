library(tidyverse) #Package for data manipulation, visualisation https://www.tidyverse.org/packages/
library(scales) # Package for rescale the axis, legend
library(glue) # Package for string literal

# Initial plot
read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>%
  drop_na() %>% # Remove any rows that have missing data
  ggplot(aes(x = year, y = t_diff))+ # Provide x and y parameter
  geom_col() + #geom_bar generates a bar plot with summary build-in function, we dont need!!
  theme_void() #Remove all theme, color from a plot

# Labeling the start and final years. Code is need to modified.

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
          select(year = Year, t_diff = `J-D`) %>%
          drop_na()

# We need first and last year. So it needs to do programmatically
annotation <- 
  t_data %>%
  arrange(year) %>% #Arrange the data in order before selecting the data below
  slice(1, n()) %>% #Slice gives rows. We need to number. The code gives us first and last row of the dataframe
  mutate(t_diff = 0, #Modify t_diff value to zero. We dont want year at the t_diff position value. We need it at zero position at the axis
           # Create new column to bump the 1880 and 2022 to the left and right
         x = year + c(-5,5) #Add year coloumn a vector
         )

#Add max t diff for template literal
max_t_diff <- format(round(max(t_data$t_diff), 1), nsmall = 1)

t_data %>% 
  ggplot(aes(x = year, y = t_diff, fill = t_diff))+ # Provide x and y parameter, fill bars color
  geom_col(show.legend = FALSE) + #geom_bar generates a bar plot with summary build-in function, we dont need!!, remove legend from the plot
  geom_text(data = annotation, aes(x = x, label = year), color = "white")+ #Add annotation, white font color
  geom_text(x = 1880, y = 1, hjust = 0, #Justify the axis
            label = glue("Global temperatures have increased by over {max_t_diff}\u00B0C since {min(t_data$year)}"), #Add title texts, Add template literal by glue function
            color = "white") + #Fill white text color
  # scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", #Fill gradient color by specify the color
 #                     midpoint = 0, # Specify the midpoint to be at zero
 #                     limits = c(-0.5, 1.5) # Provide the scale limit
 #                     )+
 # Compare how the function related to each others  
  # scale_fill_gradientn(colors = c("darkblue", "white", "darkred"), # Give color as a vector value
  #                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))), #Re-scale values from 
  #                      limits = c(min(t_data$t_diff), max(t_data$t_diff)) # Give what color go on each end
  #                      )+
  #The color is now in step not a gradient anymore
  scale_fill_stepsn(colors = c("darkblue", "white", "darkred"), # Give color as a vector value
                       values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))), #Re-scale values from 
                       limits = c(min(t_data$t_diff), max(t_data$t_diff)), # Give what color go on each end
                                  n.breaks = 9
                    ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"), #Fill in the background color
    legend.text = element_text(color = "white") # Fill in text color
  )


ggsave("figures/temperature_bar_plot.png", width = 7, height = 4)
