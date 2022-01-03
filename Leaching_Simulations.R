
#leached N must always be <= total mass leached 
#- opposite observations might indicate adsorption?

leach <- expand_grid(
  leached_N_per = c(1,2,5,10,15,20),
  initial_N_per = c(0.2,0.6,0.8,1.0,1.2,1.6,2.0),
  mass_leached = c(0.05,0.1,0.2,0.3)
)

#calculate the final N percent, the final N mass, and the normalized final N mass
leach<-leach%>%
  mutate(initial_N_mass = initial_N_per/100,
         leached_N_mass = initial_N_mass*(leached_N_per/100),
         final_N_mass = initial_N_mass - leached_N_mass,
         final_total_mass = 1-mass_leached,
         final_N_per = (final_N_mass/final_total_mass)*100,
         delta_N_per = final_N_per-initial_N_per,  #usually N per goes up
         N_mass_norm = final_N_mass/initial_N_mass)

#if microbial growth max of x ug C per whatever, assume certain C:N ratio to get to mass of N per unit time.