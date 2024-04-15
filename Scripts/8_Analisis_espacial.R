## Analisis espacial ###
#partiremos del dataframe: 
# pheno_metr_p_clima el cual viene de: 
# clim94_21_annual_plot con las variables climáticas para cada plot promediadas en el tiempo (linea 266, script 6_Clima mensual)
# pheno_metr_p_clima (linea 358, script 3_Obtencion pheno metrics)
#

names(pheno_metr_p_clima)



################# ANALISIS ESPACIAL #######################################
#Para eso estudiamos el valor promediado del periodo 1994-2021 de cada metrica y variable, o sea que cada punto apenas tendrá un valor tanto
#de las metricas fenologicas como abioticas
#
#### Graficas entre las variables climáticas (abioticas) y fenologicas espacializando las relaciones al trabajar con valores promediados por plot####

#### SOS ####
#Elevation
sos_elev_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=elevation, y=sos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between elevation and SOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

sos_elev_plot

ggsave(sos_elev_plot, device = "tiff", path = path2grafic, filename = "sos_doy_elev_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sos_elev_plot, device = "png", path = path2grafic, filename = "sos_doy_elev_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)

#precip
sos_precip_plot <- ggplot(data=pheno_metr_p_clima %>% 
                            filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Quercus faginea", 
                                               "Quercus canariensis","Olea europaea",
                                               "Castanea sativa")), aes(x=annual_plot_pre, y=sos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between precipitation and SOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(sos_precip_plot, device = "tiff", path = path2grafic, filename = "sos_doy_precip_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sos_precip_plot, device = "png", path = path2grafic, filename = "sos_doy_precip_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#temp
sos_tempe_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber", "Quercus faginea", 
                                              "Quercus canariensis","Olea europaea",
                                              "Castanea sativa")), aes(x=annual_plot_temp, y=sos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between temperature and SOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(sos_tempe_plot, device = "tiff", path = path2grafic, filename = "sos_doy_temp_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sos_tempe_plot, device = "png", path = path2grafic, filename = "sos_doy_temp_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#Arid_Dm
sos_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Olea europaea",
                                             "Castanea sativa")),
                        aes(x=Arid_Dm_i, y=sos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between Aridity and SOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(sos_Arid_plot, device = "tiff", path = path2grafic, filename = "sos_doy_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sos_Arid_plot, device = "png", path = path2grafic, filename = "sos_doy_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)



#Arid_Dm (nc)
sos_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=annual_plot_Arid, y=sos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between Aridity (nc) and SOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(sos_Arid_plot, device = "tiff", path = path2grafic, filename = "sos_doy_Aridnc_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sos_Arid_plot, device = "png", path = path2grafic, filename = "sos_doy_Aridnc_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#### EOS ####
#Elevation
eos_elev_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=elevation, y=eos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between elevation and EOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(eos_elev_plot, device = "tiff", path = path2grafic, filename = "eos_doy_elev_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(eos_elev_plot, device = "png", path = path2grafic, filename = "eos_doy_elev_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#precip
eos_precip_plot <- ggplot(data=pheno_metr_p_clima %>% 
                            filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Quercus faginea", 
                                               "Quercus canariensis","Olea europaea",
                                               "Castanea sativa")), aes(x=annual_plot_pre, y=eos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between precipitation and EOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(eos_precip_plot, device = "tiff", path = path2grafic, filename = "eos_doy_precip_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(eos_precip_plot, device = "png", path = path2grafic, filename = "eos_doy_precip_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#temp
eos_tempe_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber", "Quercus faginea", 
                                              "Quercus canariensis","Olea europaea",
                                              "Castanea sativa")), aes(x=annual_plot_temp, y=eos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between temperature and EOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(eos_tempe_plot, device = "tiff", path = path2grafic, filename = "eos_doy_tempe_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(eos_tempe_plot, device = "png", path = path2grafic, filename = "eos_doy_tempe_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#Arid_Dm
eos_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber","Olea europaea",
                                             "Castanea sativa")),
                        aes(x=Arid_Dm_i, y=eos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between Aridity and EOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow=2)+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(eos_Arid_plot, device = "tiff", path = path2grafic, filename = "eos_doy_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(eos_Arid_plot, device = "png", path = path2grafic, filename = "eos_doy_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#Arid_Dm (nc)
eos_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=annual_plot_Arid, y=eos_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between Aridity (nc) and EOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(eos_Arid_plot, device = "tiff", path = path2grafic, filename = "eos_doy_Aridnc_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(eos_Arid_plot, device = "png", path = path2grafic, filename = "eos_doy_Aridnc_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#### LOS ####
#Elevation
los_elev_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=elevation, y=los))+
  geom_point()+
  ggtitle("Correlation between elevation and LOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 160, size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(los_elev_plot, device = "tiff", path = path2grafic, filename = "los_elev_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_elev_plot, device = "png", path = path2grafic, filename = "los_elev_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#precip
los_precip_plot <- ggplot(data=pheno_metr_p_clima %>% 
                            filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Quercus faginea", 
                                               "Quercus canariensis","Olea europaea",
                                               "Castanea sativa")), aes(x=annual_plot_pre, y=los))+
  geom_point()+
  ggtitle("Correlation between precipitation and LOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 160, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(los_precip_plot, device = "tiff", path = path2grafic, filename = "los_precip_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_precip_plot, device = "png", path = path2grafic, filename = "los_precip_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#temp
los_tempe_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber", "Quercus faginea", 
                                              "Quercus canariensis","Olea europaea",
                                              "Castanea sativa")), aes(x=annual_plot_temp, y=los))+
  geom_point()+
  ggtitle("Correlation between temperature and LOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 160, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(los_tempe_plot, device = "tiff", path = path2grafic, filename = "los_temp_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_tempe_plot, device = "png", path = path2grafic, filename = "los_temp_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#Arid_Dm
los_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Olea europaea",
                                             "Castanea sativa")), aes(x=Arid_Dm_i, y=los))+
  geom_point()+
  ggtitle("Correlation between Aridity and LOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 160, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(los_Arid_plot, device = "tiff", path = path2grafic, filename = "los_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_Arid_plot, device = "png", path = path2grafic, filename = "los_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#Arid_Dm (nc)
los_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=annual_plot_Arid, y=los))+
  geom_point()+
  ggtitle("Correlation between Aridity (nc) and LOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(los_Arid_plot, device = "tiff", path = path2grafic, filename = "los_doy_Aridnc_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_Arid_plot, device = "png", path = path2grafic, filename = "los_doy_Aridnc_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#### POP ####
#Elevation
pop_elev_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=elevation, y=pop_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between elevation and pop")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pop_elev_plot, device = "tiff", path = path2grafic, filename = "pop_doy_elev_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pop_elev_plot, device = "png", path = path2grafic, filename = "pop_doy_elev_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#precip
pop_precip_plot <- ggplot(data=pheno_metr_p_clima %>% 
                            filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Quercus faginea", 
                                               "Quercus canariensis","Olea europaea",
                                               "Castanea sativa")), aes(x=annual_plot_pre, y=pop_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between precipitation and pop")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pop_precip_plot, device = "tiff", path = path2grafic, filename = "pop_doy_precip_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pop_precip_plot, device = "png", path = path2grafic, filename = "pop_doy_precip_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#temp
pop_tempe_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber", "Quercus faginea", 
                                              "Quercus canariensis","Olea europaea",
                                              "Castanea sativa")), aes(x=annual_plot_temp, y=pop_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between temperature and pop")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pop_tempe_plot, device = "tiff", path = path2grafic, filename = "pop_doy_temp_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pop_tempe_plot, device = "png", path = path2grafic, filename = "pop_doy_temp_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#Arid_Dm
pop_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber","Olea europaea",
                                             "Castanea sativa")), aes(x=Arid_Dm_i, y=pop_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between Aridity and pop")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pop_Arid_plot, device = "tiff", path = path2grafic, filename = "pop_doy_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pop_Arid_plot, device = "png", path = path2grafic, filename = "pop_doy_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#Arid_Dm (nc)
pop_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=annual_plot_Arid, y=pop_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between Aridity (nc) and POP")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pop_Arid_plot, device = "tiff", path = path2grafic, filename = "pop_doy_Aridnc_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pop_Arid_plot, device = "png", path = path2grafic, filename = "pop_doy_Aridnc_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#### POT ####
#Elevation
pot_elev_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=elevation, y=pot_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between elevation and POT")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pot_elev_plot, device = "tiff", path = path2grafic, filename = "pot_doy_elev_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pot_elev_plot, device = "png", path = path2grafic, filename = "pot_doy_elev_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#precip
pot_precip_plot <- ggplot(data=pheno_metr_p_clima %>% 
                            filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Quercus faginea", 
                                               "Quercus canariensis","Olea europaea",
                                               "Castanea sativa")), aes(x=annual_plot_pre, y=pot_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between precipitation and POT")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pot_precip_plot, device = "tiff", path = path2grafic, filename = "pot_doy_precip_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pot_precip_plot, device = "png", path = path2grafic, filename = "pot_doy_precip_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#temp
pot_tempe_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber", "Quercus faginea", 
                                              "Quercus canariensis","Olea europaea",
                                              "Castanea sativa")), aes(x=annual_plot_temp, y=pot_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between temperature and POT")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pot_tempe_plot, device = "tiff", path = path2grafic, filename = "pot_doy_temp_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pot_tempe_plot, device = "png", path = path2grafic, filename = "pot_doy_temp_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#Arid_Dm
pot_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber","Olea europaea",
                                             "Castanea sativa")), aes(x=Arid_Dm_i, y=pot_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between Aridity and POT")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pot_Arid_plot, device = "tiff", path = path2grafic, filename = "potdoy__Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pot_Arid_plot, device = "png", path = path2grafic, filename = "pot_doy_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#Arid_Dm (nc)
pot_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=annual_plot_Arid, y=pot_mesos_doy))+
  geom_point()+
  ggtitle("Correlation between Aridity (nc) and POT")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(pot_Arid_plot, device = "tiff", path = path2grafic, filename = "pot_doy_Aridnc_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pot_Arid_plot, device = "png", path = path2grafic, filename = "pot_doy_Aridnc_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#### PEAK ####
#Elevation
peak_elev_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber", "Quercus faginea", 
                                              "Quercus canariensis","Olea europaea",
                                              "Castanea sativa")), aes(x=elevation, y=peak))+
  geom_point()+
  ggtitle("Correlation between elevation and PEAK")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.80,
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(peak_elev_plot, device = "tiff", path = path2grafic, filename = "peak_elev_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(peak_elev_plot, device = "png", path = path2grafic, filename = "peak_elev_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#precip
peak_precip_plot <- ggplot(data=pheno_metr_p_clima %>% 
                             filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                "Pinus sylvestris", "Pinus nigra", 
                                                "Eucalyptus camaldulensis", "Quercus ilex", 
                                                "Quercus suber", "Quercus faginea", 
                                                "Quercus canariensis","Olea europaea",
                                                "Castanea sativa")), aes(x=annual_plot_pre, y=peak))+
  geom_point()+
  ggtitle("Correlation between precipitation and PEAK")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(peak_precip_plot, device = "tiff", path = path2grafic, filename = "peak_precip_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(peak_precip_plot, device = "png", path = path2grafic, filename = "peak_precip_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#temp
peak_tempe_plot <- ggplot(data=pheno_metr_p_clima %>% 
                            filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Quercus faginea", 
                                               "Quercus canariensis","Olea europaea",
                                               "Castanea sativa")), aes(x=annual_plot_temp, y=peak))+
  geom_point()+
  ggtitle("Correlation between temperature and PEAK")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(peak_tempe_plot, device = "tiff", path = path2grafic, filename = "peak_temp_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(peak_tempe_plot, device = "png", path = path2grafic, filename = "peak_temp_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#Arid_Dm
peak_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber","Olea europaea",
                                              "Castanea sativa")), aes(x=Arid_Dm_i, y=peak))+
  geom_point()+
  ggtitle("Correlation between Aridity and PEAK")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(peak_Arid_plot, device = "tiff", path = path2grafic, filename = "peak_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(peak_Arid_plot, device = "png", path = path2grafic, filename = "peak_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)



#Arid_Dm (nc)
peak_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=annual_plot_Arid, y=peak))+
  geom_point()+
  ggtitle("Correlation between Aridity (nc) and PEAK")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(peak_Arid_plot, device = "tiff", path = path2grafic, filename = "peak_doy_Aridnc_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(peak_Arid_plot, device = "png", path = path2grafic, filename = "peak_doy_Aridnc_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)

#### TROUGH ####
#Elevation
trough_elev_plot <- ggplot(data=pheno_metr_p_clima %>% 
                             filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                "Pinus sylvestris", "Pinus nigra", 
                                                "Eucalyptus camaldulensis", "Quercus ilex", 
                                                "Quercus suber", "Quercus faginea", 
                                                "Quercus canariensis","Olea europaea",
                                                "Castanea sativa")), aes(x=elevation, y=trough))+
  geom_point()+
  ggtitle("Correlation between elevation and TROUGH")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.80, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(trough_elev_plot, device = "tiff", path = path2grafic, filename = "trough_elev_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(trough_elev_plot, device = "png", path = path2grafic, filename = "trough_elev_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#precip
trough_precip_plot <- ggplot(data=pheno_metr_p_clima %>% 
                               filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                  "Pinus sylvestris", "Pinus nigra", 
                                                  "Eucalyptus camaldulensis", "Quercus ilex", 
                                                  "Quercus suber", "Quercus faginea", 
                                                  "Quercus canariensis","Olea europaea",
                                                  "Castanea sativa")), aes(x=annual_plot_pre, y=trough))+
  geom_point()+
  ggtitle("Correlation between precipitation and TROUGH")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(trough_precip_plot, device = "tiff", path = path2grafic, filename = "trough_precip_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(trough_precip_plot, device = "png", path = path2grafic, filename = "trough_precip_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#temp
trough_tempe_plot <- ggplot(data=pheno_metr_p_clima %>% 
                              filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                 "Pinus sylvestris", "Pinus nigra", 
                                                 "Eucalyptus camaldulensis", "Quercus ilex", 
                                                 "Quercus suber", "Quercus faginea", 
                                                 "Quercus canariensis","Olea europaea",
                                                 "Castanea sativa")), aes(x=annual_plot_temp, y=trough))+
  geom_point()+
  ggtitle("Correlation between temperature and TROUGH")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(trough_tempe_plot, device = "tiff", path = path2grafic, filename = "trough_temp_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(trough_tempe_plot, device = "png", path = path2grafic, filename = "trough_temp_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#Arid_Dm
trough_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                             filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                "Pinus sylvestris", "Pinus nigra", 
                                                "Eucalyptus camaldulensis", "Quercus ilex", 
                                                "Quercus suber","Olea europaea",
                                                "Castanea sativa")), aes(x=Arid_Dm_i, y=trough))+
  geom_point()+
  ggtitle("Correlation between Aridity and TROUGH")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow=2)+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(trough_Arid_plot, device = "tiff", path = path2grafic, filename = "trough_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(trough_Arid_plot, device = "png", path = path2grafic, filename = "trough_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)



#Arid_Dm (nc)
trough_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber", "Quercus faginea", 
                                              "Quercus canariensis","Olea europaea",
                                              "Castanea sativa")), aes(x=annual_plot_Arid, y=trough))+
  geom_point()+
  ggtitle("Correlation between Aridity (nc) and TROUGH")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(trough_Arid_plot, device = "tiff", path = path2grafic, filename = "trough_doy_Aridnc_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(trough_Arid_plot, device = "png", path = path2grafic, filename = "trough_doy_Aridnc_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)

#### MSP ####
#Elevation
msp_elev_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=elevation, y=msp))+
  geom_point()+
  ggtitle("Correlation between elevation and MSP")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.80, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(msp_elev_plot, device = "tiff", path = path2grafic, filename = "msp_elev_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(msp_elev_plot, device = "png", path = path2grafic, filename = "msp_elev_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#precip
msp_precip_plot <- ggplot(data=pheno_metr_p_clima %>% 
                            filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Quercus faginea", 
                                               "Quercus canariensis","Olea europaea",
                                               "Castanea sativa")), aes(x=annual_plot_pre, y=msp))+
  geom_point()+
  ggtitle("Correlation between precipitation and MSP")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) +
  theme_bar

ggsave(msp_precip_plot, device = "tiff", path = path2grafic, filename = "msp_precip_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(msp_precip_plot, device = "png", path = path2grafic, filename = "msp_precip_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#temp
msp_tempe_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber", "Quercus faginea", 
                                              "Quercus canariensis","Olea europaea",
                                              "Castanea sativa")), aes(x=annual_plot_temp, y=msp))+
  geom_point()+
  ggtitle("Correlation between temperature and MSP")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) +
  theme_bar

ggsave(msp_tempe_plot, device = "tiff", path = path2grafic, filename = "msp_temp_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(msp_tempe_plot, device = "png", path = path2grafic, filename = "msp_temp_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#Arid_Dm
msp_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber","Olea europaea",
                                             "Castanea sativa")), aes(x=Arid_Dm_i, y=msp))+
  geom_point()+
  ggtitle("Correlation between Aridity and MSP")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow=2)+
  theme(strip.text.x = element_text(size = 15)) +
  theme_bar

ggsave(msp_Arid_plot, device = "tiff", path = path2grafic, filename = "msp_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(msp_Arid_plot, device = "png", path = path2grafic, filename = "msp_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)



#Arid_Dm (nc)
msp_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                             filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                "Pinus sylvestris", "Pinus nigra", 
                                                "Eucalyptus camaldulensis", "Quercus ilex", 
                                                "Quercus suber", "Quercus faginea", 
                                                "Quercus canariensis","Olea europaea",
                                                "Castanea sativa")), aes(x=annual_plot_Arid, y=msp))+
  geom_point()+
  ggtitle("Correlation between Aridity (nc) and MSP")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(msp_Arid_plot, device = "tiff", path = path2grafic, filename = "msp_doy_Aridnc_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(msp_Arid_plot, device = "png", path = path2grafic, filename = "msp_doy_Aridnc_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#### MAU ####
#Elevation
mau_elev_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=elevation, y=mau))+
  geom_point()+
  ggtitle("Correlation between elevation and MAU")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.80, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(mau_elev_plot, device = "tiff", path = path2grafic, filename = "mau_elev_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(mau_elev_plot, device = "png", path = path2grafic, filename = "mau_elev_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#precip
mau_precip_plot <- ggplot(data=pheno_metr_p_clima %>% 
                            filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Quercus faginea", 
                                               "Quercus canariensis","Olea europaea",
                                               "Castanea sativa")), aes(x=annual_plot_pre, y=mau))+
  geom_point()+
  ggtitle("Correlation between precipitation and MAU")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(mau_precip_plot, device = "tiff", path = path2grafic, filename = "mau_precip_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(mau_precip_plot, device = "png", path = path2grafic, filename = "mau_precip_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)


#temp
mau_tempe_plot <- ggplot(data=pheno_metr_p_clima %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                              "Pinus sylvestris", "Pinus nigra", 
                                              "Eucalyptus camaldulensis", "Quercus ilex", 
                                              "Quercus suber", "Quercus faginea", 
                                              "Quercus canariensis","Olea europaea",
                                              "Castanea sativa")), aes(x=annual_plot_temp, y=mau))+
  geom_point()+
  ggtitle("Correlation between temperature and MAU")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(mau_tempe_plot, device = "tiff", path = path2grafic, filename = "mau_temp_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(mau_tempe_plot, device = "png", path = path2grafic, filename = "mau_temp_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)



#Arid_Dm
mau_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Olea europaea",
                                             "Castanea sativa")), aes(x=Arid_Dm_i, y=mau))+
  geom_point()+
  ggtitle("Correlation between Aridity and MAU")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 0.3, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow=2)+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(mau_Arid_plot, device = "tiff", path = path2grafic, filename = "mau_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(mau_Arid_plot, device = "png", path = path2grafic, filename = "mau_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#Arid_Dm (nc)
mau_Arid_plot <- ggplot(data=pheno_metr_p_clima %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")), aes(x=annual_plot_Arid, y=mau))+
  geom_point()+
  ggtitle("Correlation between Aridity (nc) and MAU")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free")+
  theme(strip.text.x = element_text(size = 15)) + 
  theme_bar

ggsave(mau_Arid_plot, device = "tiff", path = path2grafic, filename = "mau_doy_Aridnc_plot.tiff", 
       width = 12, height = 9, units = 'in', dpi = 300, compression = 'lzw')
ggsave(mau_Arid_plot, device = "png", path = path2grafic, filename = "mau_doy_Aridnc_plot.png", 
       width = 12, height = 9, units = 'in', dpi = 300)
