#Estadistica circular
#vamos a transformar los valores que tenemos en grados sobre 360 grados del circulo
#para eso usamos annual_metrics con los valores transformados a grados


#representacion angular de los datos circulares

sos.halep.2 <- annual_metrics %>% 
  filter(ID == 2 & var=="eos") 
  

sos.halep.circ <- circular::circular(sos.halep.2$Value_grados, type = "angles",
                                     units = "degrees")
summary(sos.halep.circ)#para calcular las metricas angulares

#o tambien
sos.halep.circ.sum <- Directional::circ.summary(sos.halep.2$Value_grados, plot = F)

sos.halep.circ.sum$mesos
sos.halep.circ.sum$circstd





## Regresiones lineales-circulares mediante circlin (Directional)
## vamos a trabajar con angulos o con radianes (preferiblemente)

yx <- pheno_metr_p_clima %>% 
  dplyr::select(ID, Sp.x, sos_mesos, eos_mesos, pop_mesos, pot_mesos, Arid_Dm_i) %>% 
  mutate(sos_mesos_rad=(2*pi*yx$sos_mesos)/360,
         eos_mesos_rad=(2*pi*yx$eos_mesos)/360,
         pop_mesos_rad=(2*pi*yx$pop_mesos)/360,
         pot_mesos_rad=(2*pi*yx$pot_mesos)/360)

#sativa
sativa <- yx %>% 
  filter(Sp.x=="Castanea sativa")

circ_sativa_sos_arid <- circlin.cor(sativa$sos_mesos_rad, sativa$Arid_Dm_i) %>% 
  print()
circ_sativa_eos_arid <- circlin.cor(sativa$eos_mesos_rad, sativa$Arid_Dm_i) %>% 
  print()
circ_sativa_pop_arid <- circlin.cor(sativa$pop_mesos_rad, sativa$Arid_Dm_i) %>% 
  print()
circ_sativa_pot_arid <- circlin.cor(sativa$pot_mesos_rad, sativa$Arid_Dm_i) %>% 
  print()

sativa <- rbind(circ_sativa_sos_arid, circ_sativa_eos_arid,
                circ_sativa_pop_arid, circ_sativa_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Castanea sativa", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))

#europaea
europaea <- yx %>% 
  filter(Sp.x=="Olea europaea")

circ_europaea_sos_arid <- circlin.cor(europaea$sos_mesos_rad, europaea$Arid_Dm_i) %>% 
  print()
circ_europaea_eos_arid <- circlin.cor(europaea$eos_mesos_rad, europaea$Arid_Dm_i) %>% 
  print()
circ_europaea_pop_arid <- circlin.cor(europaea$pop_mesos_rad, europaea$Arid_Dm_i) %>% 
  print()
circ_europaea_pot_arid <- circlin.cor(europaea$pot_mesos_rad, europaea$Arid_Dm_i) %>% 
  print()

europaea <- rbind(circ_europaea_sos_arid, circ_europaea_eos_arid,
                circ_europaea_pop_arid, circ_europaea_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Olea europaea", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))

#camaldulensis
camaldu <- yx %>% 
  filter(Sp.x=="Eucalyptus camaldulensis")

circ_camaldu_sos_arid <- circlin.cor(camaldu$sos_mesos_rad, camaldu$Arid_Dm_i) %>% 
  print()
circ_camaldu_eos_arid <- circlin.cor(camaldu$eos_mesos_rad, camaldu$Arid_Dm_i) %>% 
  print()
circ_camaldu_pop_arid <- circlin.cor(camaldu$pop_mesos_rad, camaldu$Arid_Dm_i) %>% 
  print()
circ_camaldu_pot_arid <- circlin.cor(camaldu$pot_mesos_rad, camaldu$Arid_Dm_i) %>% 
  print()

camaldu <- rbind(circ_camaldu_sos_arid, circ_camaldu_eos_arid,
                  circ_camaldu_pop_arid, circ_camaldu_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Eucalyptus camaldulensis", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))


#halepensis
halep <- yx %>% 
  filter(Sp.x=="Pinus halepensis")

circ_halep_sos_arid <- circlin.cor(halep$sos_mesos_rad, halep$Arid_Dm_i) %>% 
  print()
circ_halep_eos_arid <- circlin.cor(halep$eos_mesos_rad, halep$Arid_Dm_i) %>% 
  print()
circ_halep_pop_arid <- circlin.cor(halep$pop_mesos_rad, halep$Arid_Dm_i) %>% 
  print()
circ_halep_pot_arid <- circlin.cor(halep$pot_mesos_rad, halep$Arid_Dm_i) %>% 
  print()

halep <- rbind(circ_halep_sos_arid, circ_halep_eos_arid,
                 circ_halep_pop_arid, circ_halep_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Pinus halepensis", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))


#pinaster
pinaster <- yx %>% 
  filter(Sp.x=="Pinus pinaster")

circ_pinaster_sos_arid <- circlin.cor(pinaster$sos_mesos_rad, pinaster$Arid_Dm_i) %>% 
  print()
circ_pinaster_eos_arid <- circlin.cor(pinaster$eos_mesos_rad, pinaster$Arid_Dm_i) %>% 
  print()
circ_pinaster_pop_arid <- circlin.cor(pinaster$pop_mesos_rad, pinaster$Arid_Dm_i) %>% 
  print()
circ_pinaster_pot_arid <- circlin.cor(pinaster$pot_mesos_rad, pinaster$Arid_Dm_i) %>% 
  print()

pinaster <- rbind(circ_pinaster_sos_arid, circ_pinaster_eos_arid,
               circ_pinaster_pop_arid, circ_pinaster_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Pinus pinaster", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))

#pinea
pinea <- yx %>% 
  filter(Sp.x=="Pinus pinea")

circ_pinea_sos_arid <- circlin.cor(pinea$sos_mesos_rad, pinea$Arid_Dm_i) %>% 
  print()
circ_pinea_eos_arid <- circlin.cor(pinea$eos_mesos_rad, pinea$Arid_Dm_i) %>% 
  print()
circ_pinea_pop_arid <- circlin.cor(pinea$pop_mesos_rad, pinea$Arid_Dm_i) %>% 
  print()
circ_pinea_pot_arid <- circlin.cor(pinea$pot_mesos_rad, pinea$Arid_Dm_i) %>% 
  print()

pinea <- rbind(circ_pinea_sos_arid, circ_pinea_eos_arid,
               circ_pinea_pop_arid, circ_pinea_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Pinus pinea", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))
#nigra
nigra <- yx %>% 
  filter(Sp.x=="Pinus nigra")

circ_nigra_sos_arid <- circlin.cor(nigra$sos_mesos_rad, nigra$Arid_Dm_i) %>% 
  print()
circ_nigra_eos_arid <- circlin.cor(nigra$eos_mesos_rad, nigra$Arid_Dm_i) %>% 
  print()
circ_nigra_pop_arid <- circlin.cor(nigra$pop_mesos_rad, nigra$Arid_Dm_i) %>% 
  print()
circ_nigra_pot_arid <- circlin.cor(nigra$pot_mesos_rad, nigra$Arid_Dm_i) %>% 
  print()

nigra <- rbind(circ_nigra_sos_arid, circ_nigra_eos_arid,
               circ_nigra_pop_arid, circ_nigra_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Pinus nigra", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))
#sylve
sylve <- yx %>% 
  filter(Sp.x=="Pinus sylvestris")

circ_sylve_sos_arid <- circlin.cor(sylve$sos_mesos_rad, sylve$Arid_Dm_i) %>% 
  print()
circ_sylve_eos_arid <- circlin.cor(sylve$eos_mesos_rad, sylve$Arid_Dm_i) %>% 
  print()
circ_sylve_pop_arid <- circlin.cor(sylve$pop_mesos_rad, sylve$Arid_Dm_i) %>% 
  print()
circ_sylve_pot_arid <- circlin.cor(sylve$pot_mesos_rad, sylve$Arid_Dm_i) %>% 
  print()

sylve <- rbind(circ_sylve_sos_arid, circ_sylve_eos_arid,
               circ_sylve_pop_arid, circ_sylve_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Pinus sylvestris", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))

#ilex
ilex <- yx %>% 
  filter(Sp.x=="Quercus ilex")

circ_ilex_sos_arid <- circlin.cor(ilex$sos_mesos_rad, ilex$Arid_Dm_i) %>% 
  print()
circ_ilex_eos_arid <- circlin.cor(ilex$eos_mesos_rad, ilex$Arid_Dm_i) %>% 
  print()
circ_ilex_pop_arid <- circlin.cor(ilex$pop_mesos_rad, ilex$Arid_Dm_i) %>% 
  print()
circ_ilex_pot_arid <- circlin.cor(ilex$pot_mesos_rad, ilex$Arid_Dm_i) %>% 
  print()

ilex <- rbind(circ_ilex_sos_arid, circ_ilex_eos_arid,
               circ_ilex_pop_arid, circ_ilex_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Quercus ilex", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))

#suber
suber <- yx %>% 
  filter(Sp.x=="Quercus suber")

circ_suber_sos_arid <- circlin.cor(suber$sos_mesos_rad, suber$Arid_Dm_i) %>% 
  print()
circ_suber_eos_arid <- circlin.cor(suber$eos_mesos_rad, suber$Arid_Dm_i) %>% 
  print()
circ_suber_pop_arid <- circlin.cor(suber$pop_mesos_rad, suber$Arid_Dm_i) %>% 
  print()
circ_suber_pot_arid <- circlin.cor(suber$pot_mesos_rad, suber$Arid_Dm_i) %>% 
  print()

suber <- rbind(circ_suber_sos_arid, circ_suber_eos_arid,
              circ_suber_pop_arid, circ_suber_pot_arid) %>% 
  as.data.frame() %>% 
  mutate(pheno = c("sos", "eos","pop","pot"), Sp.x = "Quercus suber", 
         sig=ifelse(`p-value`<0.001, "***",
                    ifelse(`p-value`<0.01, "**", 
                           ifelse(`p-value`<0.05, "*","ns"))))


#unimos todos las especies

sp_lmcirc <- rbind(sativa, europaea, camaldu,
                   halep, pinaster, pinea,
                   nigra, sylve, ilex, suber)

write.xlsx(sp_lmcirc, paste(path2csv, file = "sp_lmcirc.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "sp_lmcirc", append = FALSE)
















