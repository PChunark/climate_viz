library(tidyverse)
library(R.utils) #For unzip the file
library(ncdf4) ## package for netcdf manipulation

#Download a file from the NASA website
url <- "https://data.giss.nasa.gov/pub/gistemp/gistemp250_GHCNv4.nc.gz"
download.file(url, destfile = "gistemp250_GHCNv4.nc.gz") #It is a gZip file
gunzip("gistemp250_GHCNv4.nc.gz") #It is a NetCDF file

# Function for extract NC file

nc_data <- nc_open('gistemp250_GHCNv4.nc')
# Save the print(nc) dump to a text file
{
  sink('gistemp250_GHCNv4.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

# Read in the data from the tempanomaly variable and verify the dimensions of the array
t_anomaly.array <- ncvar_get(nc_data, "tempanomaly") # store the data in a 3-dimensional array
dim(t_anomaly.array) 

#Fill in missing data
fillvalue <- ncatt_get(nc_data, "tempanomaly", "_FillValue")

#Working on data, fill NA value
t_anomaly.array[t_anomaly.array == fillvalue$value] <- NA
