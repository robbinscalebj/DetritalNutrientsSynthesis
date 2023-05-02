# script agglomerates 1x1 climate data into mean annual temp and precip for 1x1 global grids

library(tidyverse)
library(lubridate)
library(ncdf4)
library(here)

here::i_am("Tidy Data and Summarize/ClimateData/transform_climate_data.R")


clim_obj <- nc_open(here("Tidy Data and Summarize/ClimateData/CruPrecip/CRU_total_precipitation_mon_1x1_global_2016_v4.03.nc"))
ncatt_get(clim_obj, "pr")
ncatt_get(clim_obj, "lat")
ncatt_get(clim_obj, "lon")
ncatt_get(clim_obj, "time")


vars<-c("pr","lat","long","time") 

pull_ens_vars <- function(vars, clim_obj){
  
  df <- map(vars, ~ncvar_get(clim_obj, .x))|>
    reduce(bind_cols)
  names(df) <- vars
}

blah <- pull_ens_vars(vars = vars, clim_obj = clim_obj)

lon <- ncvar_get(clim_obj, "lon")
lat <- ncvar_get(clim_obj, "lat")
time <- ncvar_get(clim_obj, "time")|>as.vector()|>
  days()|>as_date(origin = "1900-01-01")



pr_array <- ncvar_get(clim_obj, "pr") #dim: 360,180,12 = lon, lat,time
pr_vec <- as.vector(pr_array)
length(pr_vec)
lonlattime <- as.matrix(expand.grid(lon,lat,time))

clim_df <- data.frame(cbind(lonlattime, pr_vec))

blah <- clim_df|>filter(!is.na(pr_vec))
range(blah$Var1) #-100.5,179.5
range(blah$Var2) # -10.5,83.5
