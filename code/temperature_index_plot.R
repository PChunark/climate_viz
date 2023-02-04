library(tidyverse)


# read_csv is a function in tidyverse. read.csv is a base r function
read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>% # Change colums name
  ggplot(aes(x = year, y = t_diff)) +
  geom_line(color = "grey", size = 0.5)+
  geom_point(fill = "white", color = "grey", shape =21)+
  geom_smooth(se = FALSE, color = "black", size = 0.5, span = 0.5)+
  theme_light()
  
data(economics, package="ggplot2")
economics$index <- 1:nrow(economics)
economics <- economics[1:80, ]
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span

# get smoothed output
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 

# Plot it
plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")
