source("code/local_weather.R")

local_weather %>% 
  select(date, prcp) %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date)) %>% 
  drop_na(prcp) %>% 
  group_by(month,day) %>% 
  summarize(prob_prcp = mean(prcp > 0), #Calculte the probability of prcp by using true (1) and false (0) values
            mean_prcp = mean(prcp), # Calculate the mean precipitation for each month and day. It includes zero value.
            mean_event = mean(prcp[prcp>0]), #Cutting out all zero value. If there is a rain, what the mean that day is.
            .groups = "drop"
            ) %>% print(n=100)
  mutate(date = ymd(glue("2020-{month}-{day}")))#Create for scale x date only. We will not see "2020" in the plot.
