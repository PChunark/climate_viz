library(tidyverse)
library(glue)

# NOAA climate data: https://www.ncei.noaa.gov/cdo-web/datasets
# Select Daily summary: https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00861/html
# Select NCEI HTTPS Server (Download): https://www.ncei.noaa.gov/pub/data/ghcn/daily/


inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <-read_table(inventory_url,
                       col_names = c("station", "lat", "lon", "variable", "start", "end"))
inventory

#Find the location that we are living in. The location given in google map is in degree unit. We need to convert it to radian.
my_lat <- 13.811979809223814 * 2 * pi/360
my_lon <- 100.5052562120295 * 2 * pi/360

#Calculate the distance between current position and weather stations
# Use this reference for calculation: https://www.geeksforgeeks.org/program-distance-two-points-earth/

#Distance, d = 3963.0 * arccos[(sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(long2 – long1)]
# The obtained distance, d, is in miles. If you want your value to be in units of kilometers, multiple d by 1.609344.
# d in kilometers = 1.609344 * d in miles

inventory %>%
  mutate(lat_radian = lat * 2 * pi/360,
         lon_radian = lon * 2 * pi/360,
         d = 3963 * acos((sin(lat_radian) * sin(my_lat)) + cos(lat_radian) * cos(my_lat) * cos(my_lon - lon_radian)))
