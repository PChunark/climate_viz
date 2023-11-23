source("code/local_weather.R")

# USe BASE R to filter out NA and Zero Values ####
no_na_no_zero <-
  local_weather[(!is.na(local_weather$prcp) &
                !is.na(local_weather$snow)) &
                local_weather$snow > 0,]

# Calculate correlation 
cor.test(~prcp+snow, data = no_na_no_zero)
