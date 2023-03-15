library(tidyverse)
library(R.utils) #For unzip the file
library(ncdf4) ## package for netcdf manipulation
library(data.table)
library(lubridate) #package for data and time


# The original plot is from NASA website: https://www.climate-lab-book.ac.uk/?s=mapping+global+temperature
#Download a file from the NASA website
url <- "https://data.giss.nasa.gov/pub/gistemp/gistemp250_GHCNv4.nc.gz"
download.file(url, destfile = "gistemp250_GHCNv4.nc.gz") #It is a gZip file
gunzip("gistemp250_GHCNv4.nc.gz") #It is a NetCDF file

# Function for extract NC file

nc_data <- nc_open('gistemp250_GHCNv4.nc')
# Save the print(nc) dump to a text file
{
  sink('gistemp250_GHCNv4.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

# Read in the data from the tempanomaly variable and verify the dimensions of the array
t_anomaly.array <- ncvar_get(nc_data, "tempanomaly") # store the data in a 3-dimensional array
dim(t_anomaly.array) 

#Fill in missing data
fillvalue <- ncatt_get(nc_data, "tempanomaly", "_FillValue")

#Working on data, fill NA value
t_anomaly.array[t_anomaly.array == fillvalue$value] <- NA

#Now we have 3 dimensional array. We need to get it in tidy format. Use "data.table" package
t_data <- as.data.table(t_anomaly.array) %>%  # it automatically removes the NA value
  as.tibble() %>% 
  select(longtitude = V1, latitude = V2, time = V3, t_diff = value) %>% 
  mutate(longtitude = lon[longtitude], #Take lon vector that we defined up above and take the value from the longtitude column and plug them to a lon[] vector and return a value from the lon vector
         latitude = lat[latitude],
         time = t[time] + as.Date("1800-01-01"),   # Convert time from 57388 to date. 57388 is the number of day since 1st January 1800 (the dataset told us).
         year = year(time)) %>%
           # tail() #See recent time point
         group_by(year, longtitude, latitude) %>% #Calculate average t_diff
  summarize(t_diff = mean(t_diff), .groups = "drop") %>%  #Calculate average t_diff and drop a group we built above
#This part checks the sampling number
  # count(year) %>% #Count how many gridpoint we have in each year
  # ggplot(aes(x = year, y = n)) + 
  # geom_line() #See sampling we have by a year
######
  filter(year >= 1950 & year < 2022) %>% 
  mutate(decade = 10 * floor(year / 10),  #Add decade labels to a plot by using "floor" argument. "Floor" argument neglects all decimal point.
         single = year %% 10) #mutate a remainder of the division
  
t_data %>%
  mutate(t_diff = case_when(t_diff < -4 ~ -4,
                            t_diff > 4 ~ 4,
                            TRUE ~ t_diff)) %>% 
  # filter(year == 2000) %>% 
  ggplot(aes(x = longtitude,
             y = latitude,
             fill = t_diff)) + # geom_raster take a color from "fill" argument not a "color" argument
  geom_raster() +
  scale_fill_gradient2(name = "Temperature anomaly \u00B0C", #give name to the legend
                       low = "darkblue",
                       mid = "white",
                       high = "darkred",
                       midpoint = 0,
                       limits = c(-5, 5), #give limit to gradient color legend
                       breaks = c(-4, -2, 0, 2, 4)) + # Assign the break of the legend
  facet_grid(decade~single,
             switch = "y") + #Switch year label from the right to the left side of the plot
  coord_fixed(expand = FALSE) + #Ensure the scale that are 1 degree of longtitude = 1 degree of latitude. "Expand = FALSE" is a argument to remove a margin in a plot
  labs(x = NULL,
       y = NULL,
       title = "Global annual temperature anomalies between 1950 and 2021") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        plot.title = element_text(color = "white", face = "bold"),
        panel.grid = element_blank(),
        strip.text.x = element_blank(), #Facet grid --> remove above label on facet grid
        strip.text.y.left = element_text(angle = 0, #Facet grid --> rotate strip text
                                         color = "white"),
        strip.background = element_blank(), #Facet grid --> Remove strip background
        # legend.position = "bottom", #Move legend to bottom
        legend.position = c(0.75, 0.03), # Give a position in x y coordinate
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white", size = 5),
        legend.title = element_text(color = "white", size = 6)) +
  guides(fill = guide_colorbar(title.position = "top", #Guides function is a argument for legend
                               title.hjust = 0.5)) # Middle justification  
        
ggsave("figures/global_anomaly.png", width = 10, height = 4.5, units = "in")
