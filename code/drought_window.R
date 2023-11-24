source("code/local_weather.R")
library(ggtext) # for element_textbox_simple(size = 18) function

threshold <- 5 

drought_by_year<-
  local_weather %>% 
  select(date, prcp) %>% 
  mutate(prcp = replace_na(prcp, 0)) %>% # other option to replace NA
  # replace_na(list(prcp = 0)) %>%  # replace NA argument should be a list
  filter(prcp > threshold) %>% 
  mutate(prev_date = lag(date, n = 1)) %>% 
  drop_na() %>% 
  mutate(drought_length = as.numeric(date-prev_date)-1,#find a drought length and exclude 1 day drought length 
         year = year(date)) %>% 
  select(year, length = drought_length)

drought_by_year %>% 
  filter(year == 1976) %>% 
  ggplot(aes(x = length)) +
  geom_histogram()


drought_by_year %>% 
  filter(year != 1939) %>% 
  group_by(year) %>% 
  summarize(n = n(),
            median = median(length),
            mean = mean(length),
            max = max(length),
            upperquartile = quantile(length, probs = 0.75)) %>% 
  ggplot(aes(x = year, y = mean))+
  geom_line()+
  geom_smooth(se = F)+ # Add a smooth line
  labs(x = "Year",
       y = "Average number of days\nbetween rain events",
       title = "The length of drought has been <span style = 'color:blue'>increasing</span> over 20 years") +
  scale_x_continuous(breaks = seq(1900, year(today()), 20)) +
  theme_classic()+
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(size = 18, margin = margin(b = 10)))
  

ggsave("figures/drought_lengths.png", width = 6, height = 4)