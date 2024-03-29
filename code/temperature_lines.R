library(tidyverse)

# NASA GISS data: https://data.giss.nasa.gov/gistemp/
# For this data we select Global-mean monthly, seasonal, and annual mean:
# https://data.giss.nasa.gov/gistemp/tabledata_v4/T_AIRS/GLB.Ts+dSST.csv

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
last_dec <-
  t_diff %>%
  filter(month == "Dec") %>% #Filter December
  mutate(year = year + 1,   #Calculate preceding year by using the original data
         month = "last_Dec") #Rename Dec to last_Dec

#Create dataframe for next Jan
next_jan <-
  t_diff %>%
  filter(month == "Jan") %>% #Filter January
  mutate(year = year - 1,    # Add extra year
         month = "next_Jan") # Rename it

#Combine 3 dataframes
t_data <- 
  bind_rows(last_dec,t_diff,next_jan) %>%
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")), #Order months into numerical order rather than alphabet order, define month as a factor
         month_number = as.numeric(month)-1, #Change the scale in x axis to number from 0-13. Factor is a vector of character. It is stored as an order. We use as.numeric to return factor to number
         this_year = year == 2022) # Create new column for data pointer
  
  annotation <-
    t_data %>% #create new dataframe from 2022
    filter(year == 2022) %>%
    slice_max(month_number)
  
  t_data %>% ggplot(aes(x = month_number, 
             y = t_diff, 
             group = year, color = year, 
             size = this_year)) + #Add size data
  geom_hline(yintercept = 0, color = "white") + # Add white line at 0 y intercept
  geom_line()+
  geom_text(data = annotation, # Add text to a plot
            aes(x = month_number, 
                y = t_diff, 
                label = year, 
                color = year), 
            inherit.aes = FALSE,
            hjust = 0, size = 5,
            nudge_x = 0.1, fontface = "bold") + 
  scale_x_continuous(breaks = 1:12, #Set x axis and its coordinator
                     labels = month.abb,
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(breaks = seq(-2,2,0.2), 
                     sec.axis = dup_axis(name = NULL, labels = NULL)) + #Rescale y axis
  scale_size_manual(breaks = c(FALSE,TRUE),
                    values = c(0.25, 1), # Add size for this_year column
                    guide = "none") + #Remove legend
  scale_color_viridis_c(breaks = seq(1880,2020,20), #Change the color to continuous scale. Re-scale the legend
                        guide = guide_colorbar(frame.colour = "white",
                                               frame.linewidth = 1)) + #Add white border around the legend 
  coord_cartesian(xlim = c(1,12)) +
  labs(x = NULL, # Add label
      y = "Temperature change since pre-industrial time [\u00B0C]",
      title = "Global temperature change since 1880 by month") + 
  theme(
    panel.background = element_rect(fill = "black", color = "white", size = 1), #Add black color and white border. Increase border line size
    plot.background = element_rect(fill = "#444444"),
    panel.grid = element_blank(), # Remove grid line color
    axis.text = element_text(color = "white", size = 13), # Add color text
    axis.ticks = element_line(color = "white"), # Change tick mark color
    axis.ticks.length = unit(-5, "pt"), #Put tick mark into the plot by 5 points
    axis.title = element_text(color = "white", size = 13),
    plot.title = element_text(color = "white", hjust = 0.5, size = 15),
    legend.title = element_blank(), # Remove legend title
    legend.background = element_rect(fill = NA),
    legend.text = element_text(color = "white"),
    legend.key.height = unit(55, "pt") #Adjust the legend height
  )

ggsave("figures/temperature_lines.png", width = 8, height = 4.5)
