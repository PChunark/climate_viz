# Call the code and data from other script rather than using "setwd"
# We dont need to call packages on top of the script
source("code/local_weather.R")

this_year <- year(today())

local_weather %>% 
  select(date, tmax) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 1951 & year != this_year) %>% 
  group_by(year) %>% # calculate temperature for each year
  summarize()


