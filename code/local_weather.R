library(tidyverse) #data manipulation package
library(glue) #template literal package
library(lubridate) # date time package

# NOAA climate data: https://www.ncei.noaa.gov/cdo-web/datasets
# Select Daily summary: https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00861/html
# Select NCEI HTTPS Server (Download): https://www.ncei.noaa.gov/pub/data/ghcn/daily/


inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <-read_table(inventory_url,
                       col_names = c("station", "lat", "lon", "variable", "start", "end"))
# inventory

#Find the location that we are living in. 
# The location given in google map is in degree unit. We need to convert it to radian.
# Location: EGAT T100 Building

my_lat <- 13.811979809223814 * 2 * pi/360
my_lon <- 100.5052562120295 * 2 * pi/360

#Calculate the distance between current position and weather stations
# Use this reference for calculation: https://www.geeksforgeeks.org/program-distance-two-points-earth/

#Distance, d = 3963.0 * arccos[(sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(long2 – long1)]
# The obtained distance, d, is in miles. If you want your value to be in units of kilometers, multiple d by 1.609344.
# d in kilometers = 1.609344 * d in miles


my_station <- inventory %>%
              mutate(lat_radian = lat * 2 * pi/360,
                     lon_radian = lon * 2 * pi/360,
                     d_km = 1.609344 * 3963 * acos((sin(lat_radian) * sin(my_lat)) + cos(lat_radian) * cos(my_lat) * cos(my_lon - lon_radian))
              ) %>%
              filter(start < 1960 & end > 2020) %>% # Capture station name
              top_n(n = -1, d_km) %>% #find the lowest distance
              distinct(station) %>%  #get the weather station
              pull(station) #%>% # Get station name as a variable
              #arrange(d_km) #arrange d_km from the lowest to the highest value (Ascending order)

#Tidy local weather station data
station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

#Search for readme station data
local_weather <- read_csv(station_daily,
                          col_names = c("station", "date", "variable", "value", "a", "b", "c", "d")) %>% 
                 select(date, variable, value) %>% 
                 pivot_wider(names_from = "variable", values_from = "value", values_fill = 0) %>%  # convert to wider dataframe
                 select(date, TMAX, TMIN, PRCP) %>% 
                 mutate(date = ymd(date)) #convert date format
