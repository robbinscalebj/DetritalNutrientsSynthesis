#This script generates citations used in the Detrital nutrients data set


#Read-in libraries
library(tidyverse)
library(janitor)
library(gratia)
library(mgcv)
library(tidymv)
library(RColorBrewer)
library(cowplot)
library(ggsci)
library(here)

here::i_am("Stream DetNuts Paper/Stream_DetNuts_Citations.R")
load(here("Tidy Data and Summarize/DetNut_Data_for_Analysis.Rdata"))

cit<-det_loc|>
  group_by(First_Author, Publication_Title, Publication_Year, Journal, DOI)|>
  slice(1)|>ungroup()|>slice(21:40)

coords <- unique(det$Lat_Long)
