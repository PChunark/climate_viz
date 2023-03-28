library(tidyverse)
library(glue)

# NOAA climate data: https://www.ncei.noaa.gov/cdo-web/datasets
# Select Daily summary: https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00861/html
# Select NCEI HTTPS Server (Download): https://www.ncei.noaa.gov/pub/data/ghcn/daily/


inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <-read_table(inventory_url,
                       col_names = c("station", "lat", "lon", "variable", "start", "end"))
inventory
