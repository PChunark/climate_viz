library(plotly)

count <- 3000

x <- c()
y <- c()
z <- c()
c <- c()

for (i in 1:count) {
  r <- i * (count - i)
  x <- c(x, r * cos(i / 30))
  y <- c(y, r * sin(i / 30))
  z <- c(z, i)
  c <- c(c, i)
}

data <- data.frame(x, y, z, c)

fig <- plot_ly(data, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
               line = list(width = 4, color = ~c, colorscale = list(c(0,'#BA52ED'), c(1,'#FCB040'))))

fig
