#R version 4.2.1
#this script tests the GAM structure of the base models for the Detrital Nutrients stream analysis
#Are the GAMs coincidentally picking up on noise unrelated to initial N? 
#can the GAM models reproduce simulated data?
#how do '1's at the beginning of each time series influence the ability of the GAMs to model data? 

library(tidyverse)
library(mgcv)
library(gratia)
library(tidymv)
library(cowplot)
library(here)

here::i_am("Stream DetNuts Paper/DetNut_simulation_GAMs.R")
load(here("Tidy Data and Summarize/DetNut_Data_for_Analysis.Rdata"))

#Randomize initial N to test how GAMs are sensitive to natural differences that could be driven
#by the fact that N changes in N-poor litters are highly sensitive on a proportional scale

Ni_ran <- det_loc|>group_by(series_index)|>nest()|>
  mutate(initial_N = map_dbl(data, ~first(.$initial_N)))|>
  ungroup()|>
  select(initial_N)|>pull()

det_loc_ran <- det_loc|>
  filter(!is.na(initial_N))|>
  group_by(series_index)|>
  mutate(initial_N_ran = sample(Ni_ran,1))|>
  relocate(initial_N, initial_N_ran,series_index)%>%
  mutate(initial_N = initial_N_ran)


Ni_5_ran <- gam(data = det_loc_ran, N_mass_norm ~  
              te(Mass_per_loss, initial_N, m = 2)+
              s(series_index, Mass_per_loss, bs = "re"), 
            method = "REML", family = Gamma(link = "inverse"),select = TRUE) 


#plot predictions
Nbase_loc.pred<-predict_gam(type = "link", Ni_5_ran, length_out = 5000,  exclude_terms = c("s(series_index)", "s(series_index, Mass_per_loss)"), values = list(series_index = NULL, initial_N = c(3,2,1.5,1.0,0.6,0.3)))%>%
  mutate(initial_N = as_factor(initial_N))


Nbase_loc.p<- ggplot()+
  geom_point(data = det_loc%>%filter(!is.na(N_mass_norm)), aes(Mass_per_loss, N_mass_norm), alpha = 0.3)+
  geom_smooth(data = Nbase_loc.pred, se = FALSE, aes(x = Mass_per_loss, y = 1/fit, color = initial_N))+
  geom_abline(slope = -0.01, intercept = 1)+
  ylab("Normalized N Mass")+
  xlab("Mass Loss (%)")+
  scale_color_brewer(palette = "Dark2")+
  guides(color=guide_legend(title="Initial N (%)"))+
  theme_bw()+plot_theme+
  NULL
Nbase_loc.p

#randomization generally results in no discernible pattern
## TRY WITH LOOPING
gam_list.names <- c("preds")
gam_list <- vector("list", length(gam_list.names))
names(gam_list) <- gam_list.names
n_iters <- 100
for (i in 1:n_iters) {
  Ni_ran <- det_loc|>group_by(series_index)|>nest()|>
    mutate(initial_N = map_dbl(data, ~first(.$initial_N)))|>
    ungroup()|>
    select(initial_N)|>pull()
  
  det_loc_ran <- det_loc|>
    filter(!is.na(initial_N))|>
    group_by(series_index)|>
    mutate(initial_N_ran = sample(Ni_ran,1))|>
    relocate(initial_N, initial_N_ran,series_index)%>%
    mutate(initial_N = initial_N_ran)

  
  gam.i_preds <- gam(data = det_loc_ran, N_mass_norm ~  
                    te(Mass_per_loss, initial_N, m = 2)+
                    s(series_index, Mass_per_loss, bs = "re"), 
                  method = "REML", family = Gamma(link = "inverse"),select = TRUE) 
  
  
  gam_list$preds[[i]] <-predict_gam(type = "link", gam.i_preds, length_out = 1000,  exclude_terms = c("s(series_index)", "s(series_index, Mass_per_loss)"), 
                                    values = list(series_index = NULL, initial_N = c(3,2,1.5,1.0,0.6,0.3)))%>%
    mutate(initial_N = as_factor(initial_N))|>
    select(-series_index)|>mutate(index = as_factor(i))
  
  print(paste("Completed iteration", i), sep = " ")
}
Ni_ransims_combined <- map_dfr(gam_list$preds, bind_rows)

Ni_ransims.p<- ggplot()+
  geom_line(data = Ni_ransims_combined, aes(x = Mass_per_loss, group = index, y = 1/fit, color = initial_N), alpha = 0.2)+
  geom_abline(slope = -0.01, intercept = 1)+
  ylab("Normalized N Mass")+
  xlab("Mass Loss (%)")+
  facet_wrap(.~initial_N)+
  scale_color_brewer(palette = "Dark2")+
  guides(color=guide_legend(title="Initial N (%)"))+
  theme_bw()+
  NULL
Ni_ransims.p
# There's no pattern here distinguishing the different levels
##

#### How do '1's at the beginning of each time series influence the ability of the GAMs to model data?

det_loc_z <- det_loc |>
  group_by(series_index)%>%
  slice(-1)

Ni_5_z <- gam(data = det_loc_z, N_mass_norm ~  
                  te(Mass_per_loss, initial_N, m = 2)+ 
                s(series_index, Mass_per_loss, bs = "re"), 
                method = "REML", family = Gamma(link = "inverse"),select = TRUE) 


#plot predictions
Nbase_loc.pred<-predict_gam(type = "link", Ni_5_z, length_out = 5000,  exclude_terms = c("s(series_index)", "s(series_index, Mass_per_loss)"), values = list(series_index = NULL, initial_N = c(3,2,1.5,1.0,0.6,0.3)))%>%
  mutate(initial_N = as_factor(initial_N))


Nbase_loc.p<- ggplot()+
  geom_point(data = det_loc%>%filter(!is.na(N_mass_norm)), aes(Mass_per_loss, N_mass_norm), alpha = 0.3)+
  geom_smooth(data = Nbase_loc.pred, se = FALSE, aes(x = Mass_per_loss, y = 1/fit, color = initial_N))+
  geom_abline(slope = -0.01, intercept = 1)+
  ylab("Normalized N Mass")+
  xlab("Mass Loss (%)")+
  scale_color_brewer(palette = "Dark2")+
  guides(color=guide_legend(title="Initial N (%)"))+
  theme_bw()+plot_theme+
  NULL
Nbase_loc.p
  

# Simulate hypothesized relationship and fit data using GAMM 
#Equation from Parton et al 2007, Science, "Global-scale similarities in nitrogen release patterns during long-term decomposition"

#create data grid
data_grid <- expand_grid(Ni= c(0.4,0.6,0.8,1.2,1.4,1.8), 
                         mass_remain = seq(10,100, by = 2),
                         ctl_factor = 0.7)

N_normalized <- function (mass_remain, ctl_factor, Ni) {
  peak_val <- 98*(1-exp(-1.56*Ni))
  (mass_remain/peak_val)/sqrt((2*ctl_factor*mass_remain/peak_val)^2+(1-(mass_remain/peak_val)^2)^2)
}

data_grid2 <- data_grid %>% 
  mutate(Nmass_norm = N_normalized(mass_remain, ctl_factor, Ni)/N_normalized(100,ctl_factor,Ni))

data_grid_null <- data_grid %>%
  mutate(Nmass_norm = mass_remain/100)


ggplot(data_grid2|>mutate(Ni = as_factor(Ni)), aes(x = mass_remain, y = Nmass_norm, color = Ni))+ 
  geom_line()+
  scale_x_continuous(trans = "reverse")

ggplot(data_grid_null|>mutate(Ni = as_factor(Ni)), aes(x = mass_remain, y = Nmass_norm, color = Ni))+ 
  geom_line()+
  scale_x_continuous(trans = "reverse")

data_grid_mm <- data_grid %>%
  mutate(Nmass_norm = 1+0.5/Ni*(1-exp(-Ni*5*(100-mass_remain)/100)))
#treating mass remain as mass loss on this one
ggplot(data_grid_mm|>mutate(Ni = as_factor(Ni)), aes(x = mass_remain, y = Nmass_norm, color = Ni))+ 
  geom_line()+
  scale_x_continuous(trans= "reverse")+
  NULL

#what we want to do is randomly sample from a normal distribution that has a mean with N_normalized ~mass_remain

series_number <- 100
mass_series_end <- 10
mass_series_interval <- 2
data_grid_big <- expand_grid(Ni= runif(series_number, min = 0.4, max = 2.0), 
                         mass_remain = seq(mass_series_end, 100, by = mass_series_interval),
                         ctl_factor = 0.7) |>
  mutate(series_index = rep(1:series_number, each = length(seq(mass_series_end, 100, by = mass_series_interval))),
         series_index = as_factor(series_index))

#generate  N_mass_norm values
data_grid_stoch <- data_grid_big %>% 
  mutate(Nmass_norm = rnorm(n(), 
                            N_normalized(mass_remain, ctl_factor, Ni)/N_normalized(100,ctl_factor,Ni), 
                            sd = (5/Ni)/mass_remain))|>
  group_by(series_index)|>
  arrange(desc(mass_remain))|>
  mutate(Nmass_norm = ifelse(row_number()==1, 1, Nmass_norm))



           
data_grid_stoch_mm <- data_grid_big %>% 
  mutate(Nmass_norm = rnorm(n(), 
                            1+0.5/Ni*(1-exp(-Ni*5*(100-mass_remain)/100)), 
                            sd = (5/Ni)/mass_remain))|>
  group_by(series_index)|>
  arrange(desc(mass_remain))|>
  mutate(Nmass_norm = ifelse(row_number()==1, 1, Nmass_norm))
  
  


#randomly pull values from series to simulate inconsistent Mass loss intervals of cohorts
data_sampled <- data_grid_stoch |>
  group_by(series_index)|>
  slice_sample(n = 5)

data_sampled_mm <- data_grid_stoch_mm |>
  group_by(series_index)|>
  slice_sample(n = 10)
  #model
  
  data_sampled2 <- data_sampled|>
    mutate(Mass_per_loss = 100-mass_remain)|>
    rename(initial_N = "Ni")%>%
    filter(Nmass_norm>0)

  
  data_sampled2_mm <- data_sampled_mm|>
    mutate(Mass_per_loss = 100-mass_remain)|>
    rename(initial_N = "Ni")%>%
    filter(Nmass_norm>0)
  
  ggplot(data_sampled2,aes(x = Mass_per_loss, y = Nmass_norm))+ 
    geom_point()+
    scale_x_continuous(trans = "reverse")
  
  ## Parton simulation
  Ni_sim <- gam(data = data_sampled2, Nmass_norm ~  
                  te(Mass_per_loss, initial_N, k = 9)+  
                  s(series_index, Mass_per_loss, bs = "re"),
                method = "REML", family = Gamma(link = "inverse"),select = TRUE) 
  
  
  #plot predictions
  Nbase_loc.pred<-predict_gam(type = "link", Ni_sim, length_out = 5000,  
                              exclude_terms = c("s(series_index)", "s(series_index, Mass_per_loss)"), 
                              values = list(series_index = NULL, initial_N = c(2,1.5,1.0,0.6,0.4)))%>%
    mutate(initial_N = as_factor(initial_N))
  
  
  Nbase_loc.p<- ggplot()+
    geom_point(data = data_sampled2%>%filter(!is.na(Nmass_norm)), aes(Mass_per_loss, Nmass_norm), alpha = 0.3)+
    geom_smooth(data = Nbase_loc.pred, se = FALSE, aes(x = Mass_per_loss, y = 1/fit, color = initial_N))+
    geom_abline(slope = -0.01, intercept = 1)+
    ylab("Normalized N Mass")+
    xlab("Mass Loss (%)")+
    scale_color_brewer(palette = "Dark2")+
    guides(color=guide_legend(title="Initial N (%)"))+
    theme_bw()+
    NULL
  Nbase_loc.p
  
  
  Nbase_loc.p2<- ggplot()+
    geom_point(data = data_sampled2%>%filter(!is.na(Nmass_norm)), aes(Mass_per_loss, Nmass_norm), alpha = 0.3)+
    geom_smooth(data = Nbase_loc.pred, se = FALSE, linetype = 2,aes(x = Mass_per_loss, y = 1/fit, color = initial_N))+
    geom_ribbon(data = Nbase_loc.pred, alpha = 0.3, aes(x = Mass_per_loss, 
                                                        ymin = 1/(fit-2*se.fit),
                                                        ymax = 1/(fit+2*se.fit),
                                                        color = initial_N))+
    geom_abline(slope = -0.01, intercept = 1)+
    ylab("Normalized N Mass")+
    xlab("Mass Loss (%)")+
    scale_color_brewer(palette = "Dark2")+
    guides(color=guide_legend(title="Initial N (%)"))+
    theme_bw()+
    theme(axis.title.x = element_blank(),axis.title.y = element_text(size = 16))+
    NULL
  Nbase_loc.p2
  
  
  #null simulation
  

  data_grid_stoch_null <- data_grid_big %>% 
    mutate(Nmass_norm = rnorm(n(), 
                              mass_remain/100, 
                              #sd = 0.1*(1-mass_remain/100),
                              sd = 0.1/Ni*(1-exp(-Ni*5*(100-mass_remain)/100))))|>
    group_by(series_index)|>
    arrange(desc(mass_remain))|>
    mutate(Nmass_norm = ifelse(row_number()==1, 1, Nmass_norm))
  
  data_sampled_null <- data_grid_stoch_null |>
    group_by(series_index)|>
    slice_sample(n = 5)|>
    mutate(Mass_per_loss = 100-mass_remain)|>
    rename(initial_N = "Ni")%>%
    filter(Nmass_norm>0)
  
  Ni_sim_null <- gam(data = data_sampled_null, Nmass_norm ~  
                  te(Mass_per_loss, initial_N, k = 9)+  
                  s(series_index, Mass_per_loss, bs = "re"),
                method = "REML", family = gaussian(link = "identity"),select = TRUE) 
  
  
  #plot predictions
  Nbase_loc.pred_null<-predict_gam(type = "link", Ni_sim_null, length_out = 5000,  
                              exclude_terms = c("s(series_index)", "s(series_index, Mass_per_loss)"), 
                              values = list(series_index = NULL, initial_N = c(2,1.5,1.0,0.6,0.4)))%>%
    mutate(initial_N = as_factor(initial_N))

  
  Nbase_loc_null.p2<- ggplot()+
    geom_point(data = data_sampled_null%>%filter(!is.na(Nmass_norm)), aes(Mass_per_loss, Nmass_norm), alpha = 0.3)+
    geom_smooth(data = Nbase_loc.pred_null, se = FALSE, linetype = 2,aes(x = Mass_per_loss, y = fit, color = initial_N))+
    geom_ribbon(data = Nbase_loc.pred_null, alpha = 0.3, aes(x = Mass_per_loss, 
                                                        ymin = (fit-2*se.fit),
                                                        ymax = (fit+2*se.fit),
                                                        color = initial_N))+
    geom_abline(slope = -0.01, intercept = 1)+
    ylab("Normalized N Mass")+
    xlab("Mass Loss (%)")+
    scale_color_brewer(palette = "Dark2")+
    guides(color=guide_legend(title="Initial N (%)"))+
    theme_bw()+
    theme(axis.title.x = element_blank(),axis.title.y = element_text(size = 16))+
    NULL
  Nbase_loc_null.p2
  
  #Michaelis-menten simulation
  
  Ni_sim_mm <- gam(data = data_sampled2_mm, Nmass_norm ~  
                       te(Mass_per_loss, initial_N, k = 9)+  
                       s(series_index, Mass_per_loss, bs = "re"),
                     method = "REML", family = Gamma(link = "inverse"),select = TRUE) 
  
  
  #plot predictions
  Nbase_loc.pred_mm<-predict_gam(type = "link", Ni_sim_mm, length_out = 5000,  
                                   exclude_terms = c("s(series_index)", "s(series_index, Mass_per_loss)"), 
                                   values = list(series_index = NULL, initial_N = c(2,1.5,1.0,0.6,0.4)))%>%
    mutate(initial_N = as_factor(initial_N))
  
  
  Nbase_loc_mm.p2<- ggplot()+
    geom_point(data = data_sampled2_mm%>%filter(!is.na(Nmass_norm)), aes(Mass_per_loss, Nmass_norm), alpha = 0.3)+
    geom_smooth(data = Nbase_loc.pred_mm, se = FALSE, linetype = 2,aes(x = Mass_per_loss, y = 1/fit, color = initial_N))+
    geom_ribbon(data = Nbase_loc.pred_mm, alpha = 0.3, aes(x = Mass_per_loss, 
                                                             ymin = 1/(fit-2*se.fit),
                                                             ymax = 1/(fit+2*se.fit),
                                                             color = initial_N))+
    geom_abline(slope = -0.01, intercept = 1)+
    ylab("Normalized N Mass")+
    xlab("Mass Loss (%)")+
    scale_color_brewer(palette = "Dark2")+
    guides(color=guide_legend(title="Initial N (%)"))+
    theme_bw()+
    theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))+
    NULL
  Nbase_loc_mm.p2
  
  grid_sims.p <- plot_grid(Nbase_loc.p2,Nbase_loc_null.p2, Nbase_loc_mm.p2, nrow = 3)+
    theme(plot.margin = margin(l = 1, t = 0, b = 0, r = 0, "cm"))+
    annotate("text",x=-0.02,y=0.955,size=7,label="A)")+
    annotate("text",x=-0.02,y=0.62,size=7,label="B)")+
    annotate("text",x=-0.02,y=0.29,size=7,label="C)")
grid_sims.p  
  

#ggsave(plot = grid_sims.p, file ="GAMM_sims.jpeg", height = 10, width = 12, units = "in")
  