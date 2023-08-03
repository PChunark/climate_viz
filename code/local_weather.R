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

# Location: Nam Ngum Hydro power plant
my_lat <- 18.792502525314656 * 2 * pi/360 #Nam Ngum Hydro power plant lat
my_lon <- 102.48927742108829 * 2 * pi/360 #Nam Ngum Hydro power plant lon

# Location: Ninomiya house
# my_lat <-36.07140157738488 * 2 * pi/360
# my_house_lon <-140.1152927073172 * 2 * pi/360

#Location: Mt. Sapporo, 
# my_lat <-42.90197963307666 * 2 * pi/360
# my_lon <-141.201075241656 * 2 * pi/360

#Location: Calgary tower, Canada
# my_lat <-51.04532663382111 * 2 * pi/360
# my_lon <--114.06326788013058 * 2 * pi/360

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
              filter(start < 1960 & end > 2020) %>%# Capture station name
              top_n(n = -1, d_km) %>% #find the lowest distance
              distinct(station) %>%  #get the weather station
              pull(station) #%>% # Get station name as a variable
              #arrange(d_km) #arrange d_km from the lowest to the highest value (Ascending order)

#Tidy local weather station data
station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

#Search for readme station data
local_weather <- read_csv(station_daily,
                          col_names = c("station", "date", "variable", "value", "a", "b", "c", "d")) %>% 
                 select(station, date, variable, value) %>% 
                 pivot_wider(names_from = "variable", values_from = "value") %>%  # convert to wider dataframe
                 # select(date, TMAX, TMIN, PRCP) %>% 
                 mutate(date = ymd(date), #convert date format
                        TMAX = TMAX / 10, # convert tenths of degree C to degree C 
                        TMIN = TMIN / 10, # convert tenths of degree C to degree C
                        PRCP = PRCP / 10) %>%  # convert tenths of mm to mm
                  rename_all(tolower) %>%  # rename all column name to lower case
                 # filter(tmax > 1) # This approach will remove other variable. Select 1 because tmax is zero. It is not possible.
                  # mutate(tmax = if_else(tmax > 1, tmax,NA_real_)) %>%  # This approch will not remove other variable. Show how to identify weird data
                         # date = if_else(date == "1951-01-01", as.Date(NA_character_), date)) %>% #Give NA value to a specific value
                  drop_na() #Remove NA value in tmax column
                   
#Identify problematic data with line plots in tmax
local_weather %>% 
  ggplot(aes(x = date, y = tmax)) +
  geom_line()

local_weather %>% 
  slice_max(n=6, tmax)
local_weather %>% 
  slice_min(n=6, tmax)
#Identify problematic data with histogram in tmax
local_weather %>% 
  ggplot(aes(x = tmax)) +
  geom_histogram(binwidth = 1.2) # give 1 degree binwidth

#Identify problematic data with histogram in prcp
local_weather %>% 
  ggplot(aes(x = date, y = prcp)) +
  geom_line()

local_weather %>% 
  slice_max(n=6, prcp)
local_weather %>% 
  slice_min(n=6, prcp)
#Identify problematic data with histogram in prcp
local_weather %>% 
  ggplot(aes(x = prcp)) +
  geom_histogram(binwidth = 8) #+ # give 1 degree binwidth
  # scale_y_continuous(limits = c(0,100))