library(ggtext) #Package for using in HTML and MARKDOWN

source("code/local_weather.R")

snow_data <-
  local_weather %>% 
  select(date, snow) %>% 
  drop_na(snow) %>% 
  mutate(calendar_year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue("{calendar_year}-08-01")), #Snow year in USA or in Canada is in July - June
                                        calendar_year - 1,
                                        calendar_year)
         ) %>% 
  filter(!calendar_year == 1939)  %>% 
  select(month, snow_year, snow) %>% 
  filter(snow_year != 1939) %>% 
  mutate(snow_year = factor(snow_year, levels = 1939:year(today())-1),
         month = factor(month, c(8:12,1:7)))

#Plotting cumulative snowfall by year 
snow_data %>% 
  group_by(snow_year, .drop = FALSE) %>% 
  summarize(total_snow = sum(snow)) %>%
  mutate(snow_year = as.numeric(levels(snow_year))) %>% 
  ggplot(aes(x = snow_year, y = total_snow)) + 
  geom_line()

# Count the day that have snow data
snow_data %>% 
  filter(snow > 0) %>% 
  count(snow_year, .drop = FALSE) %>%
  mutate(snow_year = as.numeric(levels(snow_year))) %>% 
  ggplot(aes(x = snow_year, y = n))+
  geom_line()


#Take the total snow from snow_year
total_snow <-
  snow_data %>% 
  group_by(snow_year) %>% 
  summarize(total_snow = sum(snow)) %>% 
  filter(snow_year == year(today())-1) %>% 
  mutate(total_snow = total_snow/10) %>% 
  pull(total_snow)

# Plotting snowfall by year and month
snow_data %>%
  group_by(snow_year, month, .drop = FALSE) %>% 
  summarize(snow = sum(snow), .groups = "drop") %>% 
  mutate(is_this_year = year(today())-1 == snow_year) %>%
  ggplot(aes(x = month, y = snow, group = snow_year, color = is_this_year)) +
  geom_line(show.legend = FALSE) + 
  scale_color_manual(name = NULL,
                     breaks = c(TRUE,FALSE),
                     values = c("dodgerblue", "grey")
                     ) +
  scale_x_discrete(breaks = c(9, 11, 1, 3, 5), #Add month label as an abbreviation
                   labels = month.abb[c(9, 11, 1, 3, 5)],
                   expand = c(0,0)) + # Remove the space between grid 
  scale_y_continuous(breaks = seq(0,2000,500), #Label the y axis from mm to cm
                     labels = seq(0,200,50),
                     limits = c(0,2000,500)) + 
  labs(x = NULL,
       y = "Total monthly snowfall (cm)",
       title = glue("The <span style = 'color:dodgerblue'> snow year {year(today())-1}</span> had a total snow {total_snow} cm of snow.")) + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        plot.title.position = "plot",
        plot.title = element_markdown()) #Render html and markdown into a title

ggsave("figures/snow_by_snow_year.png", width = 6, height = 4)
