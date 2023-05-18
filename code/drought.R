source("code/local_weather.R")
library(slider)
# Update environment in r
# renv::snapshot()



local_weather %>%
  select(date, prcp) %>% 
  mutate(prcp = if_else(is.na(prcp), 0, prcp)) %>% 
  mutate(one_day_lag = lag(prcp), # Add a lag data to the dataframe by one day
         two_day_lag = lag(prcp, n = 2),
         one_day_lead = lead(prcp), # Add a lag data to the dataframe by one day
         two_day_lead = lead(prcp, n = 2)
)

# How to use slider! It is a function to window (snapshot) a data
x <- 1:10

# Create lead approach
# The output of the slide function is a list!!!!!!!!!!!!!!
slide(x, 
       ~.x, # The value of the first slot of x vector
       .before = 1) # slide it using the value before the current value by 1 position

# Create lag approach
# The output of the slide function is a list!!!!!!!!!!!!!!
slide(x, 
      ~.x, # The value of the first slot of x vector
      .after = 2) # slide it using the value before the current value by 1 position

# Need only the complete set of value
# The output of the slide function is a list!!!!!!!!!!!!!!
slide(x, 
      ~.x, # The value of the first slot of x vector
      .after = 2,  # slide it using the value before the current value by 1 position
      .complete = TRUE) # last 2 set are null because it is not a complete set

# Calculate the sum of value in a complete set
# The output of the slide function is a list!!!!!!!!!!!!!!
slide(x, 
      ~sum(.x), # The value of the first slot of x vector
      .before = 2, # slide it using the value before the current value by 1 position
      .complete = TRUE)

#Extract the output from a list to a vector
slide_dbl(x, 
      ~sum(.x), # The value of the first slot of x vector
      .before = 2, # slide it using the value before the current value by 1 position
      .complete = TRUE)

# Test a slide function with a simple data frame
tibble(x = 1:10) %>% 
  mutate(total = slide_dbl(x, 
                           ~sum(.x), # The value of the first slot of x vector
                           .before = 2, # slide it using the value before the current value by 1 position
                           .complete = TRUE))