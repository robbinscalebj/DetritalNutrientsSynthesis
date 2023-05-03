# Script takes coordinates from detrital nutrients data set and matches them to MAT and MAP grid

library(tidyverse)
library(here)
library(rgeos)
#read in mat and map data
mat_df <- read_csv(here("Tidy Data and Summarize/ClimateData/MAT_0.5x0.5.csv"))|>
  rename(lat.5 = lat, lon.5 = lon)
#read-in detrital nutrients data  filter to 'field' studies only

det_df <- read_csv(here("Tidy Data and Summarize/DetNutSynth_Database_18Mar2023.csv"))|>
  filter(Setting == "field")

#snap DetNut data to grid values

blah <- det_df|>
  separate(Lat_Long, into = c("lat", "long"), sep = "_")|>
  mutate(lat = as.numeric(lat), long = as.numeric(long))|>
  mutate(lat.5 = round(lat*2)/2,
         lon.5 = round(long*2)/2)|>
  relocate(lat.5, lon.5, lat, long)


blah2 <- blah|>
  left_join(mat_df, by = c("lat.5", "lon.5"))|>
  relocate(mean_annual_temp_C)
