source("code/local_weather.R")

#Create labels as a vector to use in facet wrap
pretty_labels <- c("prob_prcp" = "Probability of precipitation",
                   "mean_prcp" = "Average amount of\nprecipitation by day (mm)",
                   "mean_event" = "Average amount of\nprecipitation by event (mm)" )

#Create variables for a vertical line
today_month <- month(today())
today_day <- day(today())
today_date <- ymd(glue("2023-{today_month}={today_day}"))

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
            ) %>% 
  mutate(date = ymd(glue("2020-{month}-{day}"))) %>% #Create for scale x date only. We will not see "2020" in the plot.
  select(-month, -day) %>% #Deselect month and day column
  pivot_longer(cols = c(prob_prcp, mean_prcp, mean_event)) %>% #Pivot the specific column
  mutate(name = factor(name, levels = c("prob_prcp", "mean_prcp", "mean_event"))) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_hline(yintercept = 0) + #Add a zero line intercept for facet plot
  geom_vline(xintercept = today_date, color = "red", size = 1) +#Add a today vertical line
  geom_smooth(se = FALSE) +
  facet_wrap(~name, ncol = 1, scales = "free_y",
             strip.position = "left",
             labeller = labeller(name = pretty_labels)) + #Rename the strip from labeller
  scale_y_continuous(limits = c(0, NA), expand = c(0,0)) + #Let the scale determine what to go up to.
  scale_x_date(date_breaks = "2 months",
               date_labels = "%B") +
  coord_cartesian(clip = "off") + #Clip anything that outside the plot
  labs(
        x = NULL,
        y = NULL
      ) +
  theme(
        strip.placement = "outside",
        strip.background = element_blank(), #Remove strip background
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line()
        )
 
ggsave("figures/prcp_prob_amount.png", width = 5, height = 7)
