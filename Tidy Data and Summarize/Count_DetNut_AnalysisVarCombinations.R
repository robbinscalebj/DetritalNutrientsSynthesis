dupe <- det_loc_cnp|>filter(!is.na(CN_ratio)&!is.na(initial_CN))
dupe2 <- det_loc_cnp|>filter(!is.na(NP_ratio)&!is.na(initial_NP)&!is.na(Velocity_m.s))
dupe3 <- det_loc_cnp|>filter(!is.na(CN_ratio)&!is.na(initial_CN)&!is.na(Temperature_C)&!is.na(Mass_per_loss))|>distinct(series_index)


ggplot(det|>filter(Lotic_Lentic == "Lotic")|>mutate(Mesh_Size_Category = str_remove(Mesh_Size_Category, "[^(-?(\\d*\\.)?\\d+)]"))|>
         mutate(Mesh_Size_Category = as.numeric(Mesh_Size_Category)), aes(x = Mesh_Size_Category))+
  geom_histogram()+
  scale_x_continuous(breaks = c(0,0.5,1,2,3,5,10))




ddupe <-det_loc|>filter(!is.na(C_per) & !is.na(N_per) & is.na(CNP_Ratio_Type))|>mutate(CN2=(C_per/N_per)*(14/12))|>
  relocate(C_per,N_per,CN_ratio,CN2,CNP_Ratio_Type)

mutate(CN_ratio = case_when(!is.na(C_per) & !is.na(N_per) & is.na(CNP_Ratio_Type) ~ ((C_per/N_per)*(14/12)),
                            TRUE ~ CN_ratio),
       CP_ratio = case_when(!is.na(C_per) & !is.na(P_per) & is.na(CNP_Ratio_Type) ~ ((C_per/P_per)*(31/12)),
                            TRUE ~ CP_ratio),
       NP_ratio = case_when(!is.na(N_per) & !is.na(P_per) & is.na(CNP_Ratio_Type) ~ ((N_per/P_per)*(31/14)),
                            TRUE ~ NP_ratio))



