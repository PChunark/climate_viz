library(tidyverse)
library(writexl)
library(readxl)


ThemeLine <- 
  theme_bw() +
  theme(
    panel.border=element_rect(fill=NA),
    panel.grid.minor = element_line(color = NA), 
    #    axis.title=element_text(size=5),
    #    axis.text.x = element_text(hjust=1,size = 10, angle = 0),
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major.x=element_line(linetype="dashed",colour="grey",linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    # panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.ticks.length=unit(-0.15,"cm")
  )
linepalette1 <- c("#4DAF4A","#FF7F00","#377EB8","#E41A1C","#984EA3","#F781BF","#8DD3C7","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","#7f878f","#A65628","#FFFF33")
linepalette2 <- c("#E41A1C","#FF7F00","#377EB8","#B3DE69","#4DAF4A","#984EA3","#F781BF","#8DD3C7","#FB8072","#80B1D3","#FDB462","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","#7f878f","#A65628","#FFFF33")


namngumdc<-tibble(year = c(2005:2022),
       gwh = c(489.691, 582.239, 726.888, 768.825, 1075.544759, 1032.342035, 681.728782, 1135.00193, 948.692, 1216.850402, 1436.613, 569.594438,
               358.625926, 246.750295, 1295.121452, 1361.7043, 1259.801441, 829.622987))
           
prcp_namngum_yearly <-
  local_weather %>% 
  mutate(date = as.Date(date, format = "%d-%b-%Y"),
         year = year(date),
         month = month(date),
         weekday = weekdays(date),
         yday = yday(date),
         day = day(date)) %>% 
  filter(year >=2005 & year < 2023) %>% 
  group_by(year) %>% 
  summarize(prcp_year = sum(prcp),
            .groups = "drop")


prcp_dcgwh <- 
          full_join(namngumdc,
          prcp_namngum_yearly,
          by = c("year" = "year")) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "value")



prcp_dcgwh %>%
  ggplot()+
  geom_line(aes(x = year, y =value, color = variable))+
  scale_x_continuous(breaks = seq(2005, 2022,1))+
  scale_color_manual(name = NULL, #Remove legend title
                     breaks = c("gwh", "prcp_year"), #rearrange the legend
                     labels = c("Energy purchased (GWh)", "Precipitation (mm)"), #Rename the legend
                     values = c("darkorange", "dodgerblue"))+
  ThemeLine+
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "darkgray", 
                                     size = 10),
        plot.margin = margin(t = 10, r = 15, b = 10, l = 10))+
  labs(x = NULL,
       y = "Amount (GWh or mm)",
       title = "Nam Ngum hydro power plant",
       subtitle = "Energy purchased (GWh) and precitation (mm)")
ggsave("figures/energy and precipitation.png", width = 7, height = 5)  


# Plot yearly precipitation in Lao
prcp_namngum_yearly%>% 
  ggplot(aes(x = year, y = prcp_year))+
  geom_line(aes(color = "salmon"), show.legend = FALSE)+
  geom_point(shape = 22)+
  geom_smooth(se = FALSE)+
  scale_x_continuous(breaks = seq(2005, 2023,1))+
  scale_y_continuous(breaks = seq(0, 1200, 100),
                     limits = c(0,1200))+
  ThemeLine+
  labs(x = NULL,
       y = "Precipitation amount (mm)")

# Plot monthly precipitation in Lao
local_weather %>% 
  mutate(date = as.Date(date, format = "%d-%b-%Y"),
         year = year(date),
         month = month(date),
         weekday = weekdays(date),
         yday = yday(date),
         day = day(date)) %>% 
  filter(year ==2015) %>% 
  group_by(year, month) %>% 
  summarize(prcp_year = sum(prcp),
            .groups = "drop") %>% 
  ggplot(aes(x = month, y = prcp_year))+
  geom_boxplot(aes(group = month, color = "salmon"), show.legend = FALSE)+
  geom_point()+
  ThemeLine+
  labs(x = NULL,
       y = "Precipitation (mm)")







# Export to excel
writexl::write_xlsx(local_weather %>% 
                      mutate(date = as.Date(date, format = "%d-%b-%Y"),
                             year = year(date),
                             month = month(date),
                             weekday = weekdays(date),
                             yday = yday(date),
                             day = day(date)) %>% 
                      filter(year >=2016) %>% 
                      group_by(year) %>% 
                      summarize(prcp_year = sum(prcp),
                                .groups = "drop"), "data/Nam Ngum precipitation_yearly.xlsx")

writexl::write_xlsx(local_weather %>% 
                     mutate(date = as.Date(date, format = "%d-%b-%Y"),
                            year = year(date),
                            month = month(date),
                            weekday = weekdays(date),
                            yday = yday(date),
                            day = day(date)) %>% 
                     filter(year >=2016) %>% 
                     group_by(year, month) %>% 
                     summarize(prcp_year = sum(prcp),
                               .groups = "drop"), "data/Nam Ngum precipitation_monthly.xlsx")
                               