library(tidyverse)

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
  mutate(year = year - 1,   #Calculate preceding year by using the original data
         month = "last_Dec") #Rename Dec to last_Dec

#Create dataframe for next Jan
next_jan <-
  t_diff %>%
  filter(month == "Jan") %>% #Filter January
  mutate(year = year + 1,    # Add extra year
         month = "next_Jan") # Rename it

#Combine 3 dataframes
bind_rows(last_dec,t_diff,next_jan) %>%
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")), #Order months into numerical order rather than alphabet order, define month as a factor
         month_number = as.numeric(month)-1) %>% #Change the scale in x axis to number from 0-13. Factor is a vector of character. It is stored as an order. We use as.numeric to return factor to number
  ggplot(aes(x = month_number, y = t_diff, group = year, color = year)) +
  geom_line()+
  scale_x_continuous(breaks = 1:12, #Set x axis and its coordinator
                     labels = month.abb) +
  scale_color_viridis_c()+ #Change the color to continuous scale
  coord_cartesian(xlim = c(1,12)) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "#444444"),
    panel.grid = element_blank() # Remove grid line color
  )
