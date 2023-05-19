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

drought_data <-
  local_weather %>%
  select(date, prcp) %>% 
  mutate(prcp = if_else(is.na(prcp), 0, prcp)) %>% 
  arrange(date) %>%  #Arrange the date in order
  mutate(window_prcp = slide_dbl(prcp, 
                                 ~sum(.x), 
                                 .before = 99, # 100 days windows, it is the current day plus a previous 99 days
                                 .complete = TRUE)) %>%  
  drop_na(window_prcp) %>% 
  mutate(start = date - 29) %>% #Calculate start date of the calculation
  select(start, end = date, window_prcp) %>%  # Select and rename column names
  mutate(end_month = month(end), # Analyse whether the month in the slideing window is drier or wetter than we might expect in overall years in the data  
         end_day = day(end),
         end_year = year(end)) %>% 
  group_by(end_month, end_day) %>% 
  mutate(threshold = quantile(window_prcp, prob = 0.05)) %>%  # define a droughty day as being below the 5th percentile for that day of the year 
  ungroup() #%>% 
  # filter(window_prcp < threshold)   #Analyse the drought
  # filter(end_year == 2022) %>% 
  # print(n = Inf) # Print all results to the console screen

 # A threshold column has repeated data in month and day. We removed the repeated data by using a "distinct" function.

drought_line <-
  drought_data %>% 
  select(end_month, end_day, threshold) %>% 
  distinct() %>% # Remove repeated month and day
  mutate(fake_date = ymd(glue("2020-{end_month}-{end_day}"))) # Add fake date
  
drought_data %>%
  mutate(fake_date = ymd(glue("2020-{end_month}-{end_day}"))) %>% #create a fake_date to create an x axis. So we take a leap year
  select(-start, -end) %>% 
  ggplot(aes(x = fake_date,
             y = window_prcp,
             group = end_year)) + 
  geom_line() + 
  geom_line(data = drought_line, 
            aes(x = fake_date,
                y = threshold),
            inherit.aes = FALSE, # Dont use the mapping aes from the above geom_line command
            color = "red")
