# script agglomerates 1x1 climate data into mean annual temp and precip for 1x1 global grids

library(tidyverse)
library(lubridate)
library(ncdf4)
library(here)
library(furrr)

here::i_am("Tidy Data and Summarize/ClimateData/climate_data_to_df.R")


# MAP
get_annual_precip <- function(ncd_file){
  clim_obj <- nc_open(ncd_file)
  lon <- ncvar_get(clim_obj, "lon")
  lat <- ncvar_get(clim_obj, "lat")
  time <- ncvar_get(clim_obj, "time")|>as.vector()|>
    days()|>as_date(origin = "1901-01-01")
  
  
  pr_array <- ncvar_get(clim_obj, "pr") #dim: 360,180,12 = lon, lat,time
  pr_vec <- as.vector(pr_array)
  
  lonlattime <- as.matrix(expand.grid(lon,lat,time))
  clim_df <- data.frame(cbind(lonlattime, pr_vec))
  colnames(clim_df) <- c("lon","lat","time", "pr")
  
  clim_df|>
    drop_na()|>
    mutate(lat = as.character(lat), lon = as.character(lon), 
           pr = as.numeric(pr), year = year(as_date(time)))|>
    group_by(year, lat, lon)|>
    summarize(annual_precip_mm = sum(pr))
}

future::plan(multicore)
annual_precips <- future_map_dfr(list.files(here("Tidy Data and Summarize/ClimateData/CruPrecip/"), full.names = TRUE), get_annual_precip)|>
  group_by(lat, lon)|>
  summarize(mean_annual_precip_mm = mean(annual_precip_mm))


annual_precips|>write_csv(here("Tidy Data and Summarize/ClimateData/MAP_0.5x0.5.csv"))

# MAT
get_annual_temp <- function(ncd_file){
  clim_obj <- nc_open(ncd_file)
  lon <- ncvar_get(clim_obj, "lon")
  lat <- ncvar_get(clim_obj, "lat")
  time <- ncvar_get(clim_obj, "time")|>as.vector()|>
    days()|>as_date(origin = "1901-01-01")
  
  
  tas_array <- ncvar_get(clim_obj, "tas") #dim: 360,180,12 = lon, lat,time
  tas_vec <- as.vector(tas_array)
  
  lonlattime <- as.matrix(expand.grid(lon,lat,time))
  clim_df <- data.frame(cbind(lonlattime, tas_vec))
  colnames(clim_df) <- c("lon","lat","time", "tas")
  
  clim_df|>
    drop_na()|>
    mutate(lat = as.character(lat), lon = as.character(lon), 
           tas = as.numeric(tas), year = year(as_date(time)))|>
    group_by(year, lat, lon)|>
    summarize(annual_temp_C = mean(tas))
}

future::plan(multicore)
annual_temps <- future_map_dfr(list.files(here("Tidy Data and Summarize/ClimateData/CruTemp/"), full.names = TRUE), get_annual_temp)|>
  group_by(lat, lon)|>
  summarize(mean_annual_temp_C = mean(annual_temp_C))

#blah<-annual_temps|>mutate(lat = as.numeric(lat), lon = as.numeric(lon))|>filter(lat < 32 & lat > 31) #test found Waco and looks right ~20 C

annual_temps|>write_csv(here("Tidy Data and Summarize/ClimateData/MAT_0.5x0.5.csv"))


#### 
clim_obj2 <- nc_open(here("Tidy Data and Summarize/ClimateData/CruTemp/CRU_mean_temperature_mon_0.5x0.5_global_2016_v4.03.nc"))

ncatt_get(clim_obj, "pr")
ncatt_get(clim_obj, "lat")
ncatt_get(clim_obj, "lon")
ncatt_get(clim_obj, "time")


vars<-c("pr","lat","long","time") 



