library(tidyverse)

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

t_data %>% 
  ggplot(aes(x = year, y = t_diff, fill = t_diff))+ # Provide x and y parameter, fill bars color
  geom_col() + #geom_bar generates a bar plot with summary build-in function, we dont need!!
  geom_text(data = annotation, aes(x = x, label = year), color = "white")+ #Add annotation, white font color
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", #Fill gradient color by specify the color
                      midpoint = 0 # Specify the midpoint to be at zero
                      )+
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white")
  )


ggsave("figures/temperature_bar_plot.png", width = 7, height = 4)
