# Call the code and data from other script rather than using "setwd"
# We dont need to call packages on top of the script
source("code/local_weather.R")

this_year <- year(today())

local_weather %>% 
  select(date, tmax) %>%
  drop_na(tmax) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 1891 & year != this_year) %>%
  group_by(year) %>% # calculate temperature for each year
  summarize(tmax = mean(tmax)) %>%
  mutate(normalize_range = (year >= 1951 & year <= 1980), #Add normalize range because we gonna compare tmax with 1951 and 1980)
         normalize_mean = sum(tmax * normalize_range)/sum(normalize_range), #Calculate mean
         t_diff = tmax - normalize_mean) %>% 
         ggplot(aes(x = year, y = t_diff)) +
  geom_line() + 
  geom_smooth()

#Another group by usage
local_weather %>% 
  select(date, tmax) %>% 
  mutate(year = year(date),
         month = month(date))%>% 
  filter(year != 1891) %>%
  group_by(year, month) %>% 
  summarize(tmax = mean(tmax), 
            .groups = "drop") %>%  #Always use!! Command for dropping group by function
  ggplot(aes(x = month, y = tmax, group = year, color = year)) + 
  geom_line()

#Using group_by without a summarize function
local_weather %>% 
  select(date, tmax) %>%
  drop_na(tmax) %>% 
  mutate(year = year(date),
         month = month(date))%>% 
  filter(year != 1891) %>%
  group_by(year, month) %>% 
  summarize(tmax = mean(tmax), 
            .groups = "drop") %>%  #Always use!! Command for dropping group by function
  group_by(month) %>% 
  mutate(normalized_range = year >= 1951 & year <= 1980,
         normalized_temp = sum(tmax * normalized_range)/sum(normalized_range),
         t_diff = tmax - normalized_temp, #Average temperature for each year 
         is_this_year = year == this_year) %>% 
  ungroup() %>%  #Ungroup the group by function
  # filter(month == 1) #Check the normalized temp whether it is the same for each year
  # ggplot(aes(x = month, y = normalized_temp)) + geom_line() #Check average temperature
  ggplot(aes(x = month, y = t_diff, group = year, color = is_this_year)) + 
  geom_line() +
  scale_color_manual(breaks = c(F,T),
                     values = c("lightgray", "dodgerblue"),
                     guide = "none") +
  theme_classic()
