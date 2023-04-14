source("code/local_weather.R")

snow_data <-
  local_weather %>% 
  select(date, snow) %>% 
  drop_na(snow) %>% 
  mutate(calendar_year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue("{calendar_year}-07-01")), #Snow year in USA or in Canada is in July - June
                                        calendar_year - 1,
                                        calendar_year)
         ) %>% 
  filter(!calendar_year == 1939)  %>% 
  select(month, snow_year, snow) %>% 
  filter(snow_year != 1939) 

#Plotting cumulative snowfall by year 
snow_data %>% 
  group_by(snow_year) %>% 
  summarize(total_snow = sum(snow)) %>% 
  ggplot(aes(x = snow_year, y = total_snow)) + 
  geom_line()

# Create dummy dataframe for NA value
dummy_df <-

    crossing(snow_year = 1940:2022,
             month = 1:12) %>%
    mutate(dummy = 0)

# Plotting snowfall by year and month
snow_data %>%
  right_join(., dummy_df, by = c("snow_year", "month")) %>%
  mutate(snow = if_else(is.na(snow), dummy, snow)) %>% 
  group_by(snow_year, month) %>% 
  summarize(snow = sum(snow), .groups = "drop") %>% 
  mutate(month = factor(month, c(7:12,1:6))) %>% 
  ggplot(aes(x = month, y = snow, group = snow_year, color = snow_year)) +
  geom_line()
