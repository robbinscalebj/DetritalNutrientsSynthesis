# Script takes coordinates from detrital nutrients data set and matches them to MAT and MAP grid

library(tidyverse)
library(here)
library(geosphere)
source(here("Tidy Data and Summarize/ClimateData/gd_get_biomes_spdf.R"))

#read in mat and map data
mat_df <- read_csv(here("Tidy Data and Summarize/ClimateData/MAT_0.5x0.5.csv"))
map_df <- read_csv(here("Tidy Data and Summarize/ClimateData/MAP_0.5x0.5.csv"))

mat_map <- full_join(mat_df, map_df, by = c("lon", "lat"))|>
  rename(lat.5 = lat, long.5 = lon) #rename for convenience in haversine dist calculation


#read-in detrital nutrients data  filter to 'field' studies only

det_df <- read_csv(here("Tidy Data and Summarize/DetNutSynth_Database_18Mar2023.csv"))|>
  filter(Setting != "lab")

length(unique(det_df$Lat_Long)) #179
det_latlongs <- unique(det_df$Lat_Long)
#find nearest grid values

grid_points <- det_df|>
  separate(Lat_Long, into = c("lat", "long"), sep = "_", remove= FALSE)|>
  #mutate(lat = as.numeric(lat), long = as.numeric(long))|>
  relocate(lat, long)|>
  distinct(Lat_Long, lat, long)|>
  ungroup()|>#still 158 here
  expand_grid(mat_map|>select(long.5, lat.5))
   #doesn't matter that we're using MAT here - MAP are same points


nearest <- grid_points  %>%  mutate(dist = distHaversine(cbind(as.numeric(long), as.numeric(lat)), 
                                                         cbind(as.numeric(long.5), as.numeric(lat.5))))|>
  group_by(Lat_Long)|>
  filter(dist == min(dist))|>
  left_join(mat_map, by = c("lat.5", "long.5"))|>
  filter(!(long.5 == -112.25 & lat.5 == 41.25))|> #this UTAH site (Lat_Long = 41.2_-112.0), we want 41.25,-111.75 - not up in mountains- could just delete this duplicate
  select(-dist, -lat.5, -long.5, -lat, -long)


mean_annual_precip_mm <- nearest$mean_annual_precip_mm
mean_annual_temp_C <- nearest$mean_annual_temp_C

clim_point <- sp::SpatialPoints(data.frame(x = mean_annual_precip_mm, y = mean_annual_temp_C))

biome <- sp::over(
  clim_point, gd_get_biomes_spdf(merge_deserts = FALSE)
)

# STEP 4
# Append biome and return the data frame
# 4.1 Append biome to data
biomes <- tibble(biome)|>
  bind_cols(nearest|>select(Lat_Long))|>
  filter(!(Lat_Long == "37.150000_-2.000000" & biome == "Subtropical desert"))|> #issue with duplicating a lat long for jesus-casa publication
  rename(Biome = "biome")


biomes|>write_csv(here("Tidy Data and Summarize/ClimateData/biome_by_latlong.csv"))



#double check join function works
blah <- det_df|>
  select(Lat_Long, First_Author)|>
  left_join(biomes, by = "Lat_Long")|>
  relocate(Lat_Long, biome)
