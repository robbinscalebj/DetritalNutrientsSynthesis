
#The purpose of this script is to tidy the data used for analysis in the Detrital Nutrients ms
#This script created using R 4.1.2



#Read-in libraries
library(tidyverse)
library(janitor)
library(gratia)
library(mgcv)
library(tidymv)
library(RColorBrewer)
library(here)

#Set up plot themes
plot_theme <- theme(panel.grid = element_blank(), 
                    axis.text = element_text(size = 14, face = "bold"),
                    axis.title = element_text(size = 16, face = "bold"),
                    legend.title = element_text(size = 16, face = "bold"),
                    legend.text = element_text(size = 15),
                    legend.position = c(0.87,0.7))

deriv_plot_theme <- theme(axis.text = element_text(size = 14, face = "bold"),
                          axis.title = element_text(size = 16, face = "bold"),
                          legend.title = element_text(size = 16, face = "bold"),
                          legend.text = element_text(size = 15),
                          strip.text.x = element_text(size = 14, face = "bold"),
                          strip.background = element_rect(fill = "#F4F6F6"),
                          legend.position = c(0.87,0.7))

#Read raw dataset

here::i_am("Tidy Data and Summarize/Transform_DetNutData_for_Analysis.R")
det_raw <- read_csv(here("Tidy Data and Summarize/DetNutSynth_Database_30Aug2022.csv"))

#Variables for analysis
det <- det_raw%>%
  group_by(First_Author, Publication_Title, Time_Series_ID)%>%
  mutate(series_index = group_indices())%>%#creates a unique index for each time series
  ungroup()%>%
  group_by(series_index)%>% #functionally, used to confine any window functions like first() to a time series rather than the first observation of whole dataframe
 #pull initial CNP and ratios
  mutate(initial_C = first(C_per),
         initial_N = first(N_per),
         initial_P = first(P_per),
         initial_CN = first(CN_ratio),
         initial_CP = first(CP_ratio),
         initial_NP = first(NP_ratio))%>%
  #calculate normalized versions of CNP variables
  mutate(C_mass_norm = ((C_per/100)*Mass_per_remaining)/(first(C_per/100)*first(Mass_per_remaining)),
         N_mass_norm = ((N_per/100)*(Mass_per_remaining/100))/((first(N_per)/100)*first(Mass_per_remaining/100)),
         P_mass_norm = ((P_per/100)*Mass_per_remaining)/(first(P_per/100)*first(Mass_per_remaining)))%>%
  mutate(C_mass = (C_per/100)*Mass_per_remaining,
         N_mass = (N_per/100)*Mass_per_remaining,
         P_mass = (P_per/100)*Mass_per_remaining)%>%


  #approximate percent mass remaining (if not provided) at each measurement day from k value
  mutate(Mass_per_remaining = case_when(is.na(Mass_per_remaining) & Remain_Mass_Category == "afdm" ~ exp(-k*Meas_Day)*100,
                                        is.na(Mass_per_remaining) & Remain_Mass_Category == "dm" ~ exp(-k*Meas_Day)*100,
                                        TRUE ~ Mass_per_remaining))%>%
#fill in water DIN values, when available
  mutate(Water_DIN_ug.L = case_when(is.na(Water_DIN_ug.L) ~ Water_NO3_ug.L + Water_NH4_ug.L,
         TRUE ~ Water_DIN_ug.L))%>%
  #approximate water N and P availability through multiplicative factor - empirical log-log relationships developed from Glorich database
  #slopes of relationships used as average ratio between specific analytes and total nutrients
  mutate(Temperature_C_avg = case_when(n_distinct(Temperature_C) >1 ~ mean(Temperature_C, na.rm = TRUE), 
                                               TRUE ~ Temperature_C),
         TN_approx = case_when(is.na(Water_TN_ug.L) & is.na(Water_DIN_ug.L) & !is.na(Water_NH4_ug.L) ~ exp(5.466+0.4722*log(Water_NH4_ug.L)),
                               is.na(Water_TN_ug.L) & is.na(Water_DIN_ug.L) & !is.na(Water_NO3_ug.L) ~ exp(4.541+0.4748*log(Water_NO3_ug.L)),
                               is.na(Water_TN_ug.L) & !is.na(Water_DIN_ug.L) ~ exp(2.11+0.785*log(Water_DIN_ug.L)),
                               TRUE ~ Water_TN_ug.L),
         TP_approx = case_when(is.na(Water_TP_ug.L) & !is.na(Water_SRP_ug.L) ~ exp(2.25+log(Water_SRP_ug.L*0.674)),
                               TRUE ~ Water_TP_ug.L),
         no3_avg = case_when(n_distinct(Water_NO3_ug.L) >1 ~ mean(Water_NO3_ug.L, na.rm = TRUE), 
                             TRUE ~ Water_NO3_ug.L),
         srp_avg = case_when(n_distinct(Water_SRP_ug.L) >1 ~ mean(Water_SRP_ug.L, na.rm = TRUE), 
                             TRUE ~ Water_SRP_ug.L))%>%

  #remove text from variable if not coarse fine or open
  
  mutate(mesh_cat = case_when(Mesh_Size_Category == "coarse" | Mesh_Size_Category == "fine" | Mesh_Size_Category == "open" ~ Mesh_Size_Category,
                              Mesh_Size_Category < 1 ~ "fine",
                              Mesh_Size_Category >= 1 ~ "coarse"))%>%
  #collapse system categories to lotic or lentic
  mutate(Lotic_Lentic = case_when(System == "stream"|System == "Stream"|System == "Channel"|System == "river" ~ "Lotic",
                                  System == "wetland"|System == "Wetland"|System == "wetlands"|System == "marsh"|System == "lake"|System == "Lake"|System == "pond"|System == "reservoir" ~ "Lentic",
                                  TRUE ~ System),
         Lotic_Lentic = as_factor(Lotic_Lentic))%>%
#convert a few variables for ease of use
  mutate(series_index = as_factor(series_index),
         Mass_per_loss = 100-(Mass_per_remaining),
         mesh_cat = as_factor(mesh_cat),
         Remain_Mass_Category = as_factor(Remain_Mass_Category))


#Constrain dataset to focal litter types and conditions
det_loc<-det%>%
  #very few data points below 80% mass loss
  filter(Mass_per_remaining<=100,
         Mass_per_remaining >=20)%>%
  #limit dataset to analyzing coarse decomposing senesced leaves in flowing systems - 
  #by far largest and most consistent data grouping available for analysis within larger dataset
  filter(mesh_cat == "coarse",
         Lotic_Lentic == "Lotic",
         Remain_Mass_Category == "afdm",
         Detritus_Condition == "senesced",
         Detritus_Type == "leaves")%>%
  mutate(Remain_Mass_Category = fct_drop(Remain_Mass_Category, only = c("dm_postleach", "afdm_postleach", "organic_carbon")))%>%
  mutate(mesh_cat = fct_drop(mesh_cat, "open"))%>%
  ungroup()

#can also create a dataset of 'fine' mesh, but there are relatively few time series available here
det_lof<-det%>%
  filter(Mass_per_remaining<=100,
         Mass_per_remaining >=20)%>%
  filter(mesh_cat == "fine",
         Lotic_Lentic == "Lotic",
         Remain_Mass_Category == "afdm",
         Detritus_Condition == "senesced",
         Detritus_Type == "leaves")%>%
  mutate(Remain_Mass_Category = fct_drop(Remain_Mass_Category, only = c("dm_postleach", "afdm_postleach", "organic_carbon")))%>%
  mutate(mesh_cat = fct_drop(mesh_cat, "open"))%>%
  ungroup()

#define sub-dataset for C:P,N:P analysis - not knowing whether a ratio is molar or mass-based is a large error for ratios where the molar masses
#of each element are fairly different (only 14/12 for C:N, whereas nearly 2x error for C:P at 31/12)
det_loc_cnp <- det_loc%>%filter(CNP_Ratio_Type == "molar")





save.image(here("Tidy Data and Summarize/DetNut_Data_for_Analysis.Rdata"))


