source("code/local_weather.R")
library(magrittr)

# USe BASE R to filter out NA and Zero Values ####
no_na_no_zero <-
  local_weather[(!is.na(local_weather$prcp) &
                !is.na(local_weather$snow)) &
                local_weather$snow > 0,]

# Calculate correlation 
cor.test(~prcp+snow, data = no_na_no_zero)

# Use dplyr function without pipe operator ####
no_nas <- drop_na(local_weather)
no_nas_no_zero <- filter(no_nas, snow > 0)
cor.test(~prcp+snow, data = no_nas_no_zero)

# Use BASE R Pipe ####
# this wont work for continuous data analysis
no_nas_no_zero2<-
  local_weather |>
  drop_na() |>
  filter(snow > 0) 
  
cor.test(~prcp + snow, data = no_nas_no_zero2)

# The magrittr pipe ####

local_weather %>% 
  drop_na() %>% 
  filter(snow > 0) %>% 
  cor.test(~prcp + snow, data = .)

# The exposition pipe
local_weather %>% 
  drop_na() %>% 
  filter(snow > 0) %$% #exposition pipe must load library(magrittr)
  cor.test(~prcp + snow, data = .)