library(tidyverse)
library(R.utils) #For unzip the file
library(ncdf4) ## package for netcdf manipulation
library(data.table)
library(lubridate) #package for data and time
library(ggridges) # Package for creating a ridgeline plot

# The original plot is from NASA website: https://svs.gsfc.nasa.gov/4891
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
  filter(year >= 1950 & year < 2022) %>%  #The sampling number steeply increases since 1950
  group_by(year) %>% 
  mutate(t_ave = mean(t_diff))


  
t_data %>% 
  # filter(year %in% c(1950, 1980, 2000, 2020)) %>% #Select few year to see data
  ggplot(aes(x = t_diff, y = factor(year), fill = t_ave)) + #year is a continues variable. But Geom_ridge need a factor in y axis. factor(y) is used.
  # geom_density(alpha = 0.3) #See the density of the data
  geom_density_ridges(bandwidth = 0.3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "darkred",
                       midpoint = 0)
