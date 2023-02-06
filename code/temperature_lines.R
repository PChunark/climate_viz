library(tidyverse)

read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% #load CSV file, skip 1st rows
  select(month.abb) #month.abb is constant R build-in. which is the same as our datatype
