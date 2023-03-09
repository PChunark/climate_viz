library(tidyverse)
library(plotly)
library(glue) #library to do a template literal
library(htmlwidgets) #library to save a plot to html

#The data shows the temperature different between each month and the month in 1980
t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na() %>%  # Drop NA value # look the tail of the dataframe%>% slice_tail(n=12)
  mutate(month = factor(month, levels = month.abb)) %>%  #Convert month character to factor
  arrange(year, month) %>%  #Sort years and month
  mutate(month_number = as.numeric(month),#Create  x, y, z coordinates
         radius = t_diff + 1.5, #We dont need t_diff to be a negative value. So we added 1.5
         theta = 2 * pi * (month_number-1)/12,
         x = radius * sin(theta),
         y = radius * cos(theta),
         z = year,
         labels = glue("{month} {year} \n{t_diff}\u00B0 C"))#Create labels in the dataframe

#Check a first plot
# t_data %>% ggplot(aes(x = x, y = y, color = z))+
#   geom_path()

axx <- list(
  title = "",
  showgrid = F, # turn off grid lines
  zeroline = F, # turn off grid at zero axis
  showticklabels = F #turn off label 
  )

axy <- list(
  title = "",
  showgrid = F, # turn off grid lines
  zeroline = F, # turn off grid at zero axis
  showticklabels = F #turn off label
  )

axz <- list(
  title = ""
)

p <- plot_ly(t_data, 
        x = ~x, y = ~y, z = ~z,
        text = ~labels,
        hoverinfo = "text",
        type = 'scatter3d', 
        mode = 'lines',
        line = list(width = 10, color = ~t_diff,
                    cmid = 0, #Added mid point to color the point 0.5 color
                    #cmin = min(t_data$t_diff),
                    #cmax = max(t_data$t_diff),
                     colorscale = list(c(0,'#0000FF'), 
                                       c(0.5, "#FFFFFF"),
                                       c(1,'#FF0000')))) %>% 
        layout(scene = list(xaxis=axx,
                            yaxis=axy,
                            zaxis=axz))

saveWidget(p, "figures/climate_spiral_plotly.html")
