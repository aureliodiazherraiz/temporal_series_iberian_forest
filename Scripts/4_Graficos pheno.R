#para ver las diferencias entre especies apra cada variable aplicamos un test de anova o 
#un kruskall wallis caso sea no paramétrico

#usaremos pheno_metr obtenido del scrips sobre métricas fenológicas donde tenemos 
#todas las observaciones

#eos
eos_plot <- ggplot(data = pheno_metr_p %>% 
         mutate(leaf=ifelse(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                           "Pinus sylvestris","Pinus nigra", 
                                           "Eucalyptus camaldulensis","Quercus ilex", 
                                           "Quercus suber", "Olea europaea"), "Evergreen", "Deciduous")) %>% 
         mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                       "Pinus pinea" = "P. pinea",
                       "Pinus pinaster" = "P. pinaster",
                       "Pinus sylvestris"="P. sylvestris",
                       "Pinus nigra"="P. nigra",
                       "Eucalyptus camaldulensis"="E. camaldulensis",
                       "Quercus ilex"="Q. ilex",
                       "Quercus suber"="Q.suber",
                       "Quercus faginea"="Q.faginea",
                       "Quercus canariensis"="Q.canariensis",
                       "Olea europaea"="O. europaea",
                       "Castanea sativa"="C. sativa")),
       aes(x = Species, y = eos_mesos_doy, color = leaf,)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.7, alpha=0.9) +
  ylab("DoY")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species End Of Season (EOS)")

eos_plot

ggsave(eos_plot, device = "tiff", path = path2grafic, filename = "eos_mesos_doy_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(eos_plot, device = "png", path = path2grafic, filename = "eos_mesos_doy_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


#sos
sos_plot <- ggplot(data = pheno_metr_p %>% 
                     mutate(leaf=ifelse(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                                    "Pinus sylvestris","Pinus nigra", 
                                                    "Eucalyptus camaldulensis","Quercus ilex", 
                                                    "Quercus suber", "Olea europaea"), "Evergreen", "Deciduous")) %>% 
                     mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                                    "Pinus pinea" = "P. pinea",
                                                    "Pinus pinaster" = "P. pinaster",
                                                    "Pinus sylvestris"="P. sylvestris",
                                                    "Pinus nigra"="P. nigra",
                                                    "Eucalyptus camaldulensis"="E. camaldulensis",
                                                    "Quercus ilex"="Q. ilex",
                                                    "Quercus suber"="Q.suber",
                                                    "Quercus faginea"="Q.faginea",
                                                    "Quercus canariensis"="Q.canariensis",
                                                    "Olea europaea"="O. europaea",
                                                    "Castanea sativa"="C. sativa")),
                   aes(x = Species, y = sos_mesos_doy, color = leaf,)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.7, alpha=0.9) +
  ylab("DoY")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species Start Of Season (SOS)")

sos_plot

ggsave(sos_plot, device = "tiff", path = path2grafic, filename = "sos_mesos_doy_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sos_plot, device = "png", path = path2grafic, filename = "sos_mesos_doy_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)



#los
los_plot <- ggplot(data = pheno_metr_p %>% 
                     mutate(leaf=ifelse(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                                    "Pinus sylvestris","Pinus nigra", 
                                                    "Eucalyptus camaldulensis","Quercus ilex", 
                                                    "Quercus suber", "Olea europaea"), "Evergreen", "Deciduous")) %>% 
                     mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                                    "Pinus pinea" = "P. pinea",
                                                    "Pinus pinaster" = "P. pinaster",
                                                    "Pinus sylvestris"="P. sylvestris",
                                                    "Pinus nigra"="P. nigra",
                                                    "Eucalyptus camaldulensis"="E. camaldulensis",
                                                    "Quercus ilex"="Q. ilex",
                                                    "Quercus suber"="Q.suber",
                                                    "Quercus faginea"="Q.faginea",
                                                    "Quercus canariensis"="Q.canariensis",
                                                    "Olea europaea"="O. europaea",
                                                    "Castanea sativa"="C. sativa")),
                   aes(x = Species, y = los, color = leaf,)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.8, alpha=0.9) +
  ylab("Number of Days of LOS")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species Length Of Season (LOS)")

los_plot

ggsave(los_plot, device = "tiff", path = path2grafic, filename = "los_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_plot, device = "png", path = path2grafic, filename = "los_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


#los_violin
los_plot2 <- ggplot(data = pheno_metr_p %>% 
                     filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                        "Pinus sylvestris","Pinus nigra", 
                                        "Eucalyptus camaldulensis","Quercus ilex", 
                                        "Quercus suber", "Olea europaea", "Castanea sativa")) %>% 
                     mutate(leaf=ifelse(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                                    "Pinus sylvestris","Pinus nigra", 
                                                    "Eucalyptus camaldulensis","Quercus ilex", 
                                                    "Quercus suber", "Olea europaea"), "Evergreen", "Deciduous")) %>% 
                     mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                                    "Pinus pinea" = "P. pinea",
                                                    "Pinus pinaster" = "P. pinaster",
                                                    "Pinus sylvestris"="P. sylvestris",
                                                    "Pinus nigra"="P. nigra",
                                                    "Eucalyptus camaldulensis"="E. camaldulensis",
                                                    "Quercus ilex"="Q. ilex",
                                                    "Quercus suber"="Q.suber",
                                                    "Quercus faginea"="Q.faginea",
                                                    "Quercus canariensis"="Q.canariensis",
                                                    "Olea europaea"="O. europaea",
                                                    "Castanea sativa"="C. sativa")),
                   aes(x = Species, y = los, color = leaf,)) +
  geom_violin() +
  #geom_jitter(color="black", size=0.8, alpha=0.9) +
  ylab("Number of Days of LOS")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species Length Of Season (LOS)")+
  theme(legend.position="bottom")

los_plot2

ggsave(los_plot2, device = "tiff", path = path2grafic, filename = "los_violin_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_plot2, device = "png", path = path2grafic, filename = "los_violin_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


#pop
pop_plot <- ggplot(data = pheno_metr_p %>% 
                     mutate(leaf=ifelse(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                                    "Pinus sylvestris","Pinus nigra", 
                                                    "Eucalyptus camaldulensis","Quercus ilex", 
                                                    "Quercus suber", "Olea europaea"), "Evergreen", "Deciduous")) %>% 
                     mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                                    "Pinus pinea" = "P. pinea",
                                                    "Pinus pinaster" = "P. pinaster",
                                                    "Pinus sylvestris"="P. sylvestris",
                                                    "Pinus nigra"="P. nigra",
                                                    "Eucalyptus camaldulensis"="E. camaldulensis",
                                                    "Quercus ilex"="Q. ilex",
                                                    "Quercus suber"="Q.suber",
                                                    "Quercus faginea"="Q.faginea",
                                                    "Quercus canariensis"="Q.canariensis",
                                                    "Olea europaea"="O. europaea",
                                                    "Castanea sativa"="C. sativa")),
                   aes(x = Species, y = pop, color = leaf,)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.8, alpha=0.9) +
  ylab("DoY")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species day of maximum (Peak of cicle)")



pop_plot

ggsave(pop_plot, device = "tiff", path = path2grafic, filename = "pop_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pop_plot, device = "png", path = path2grafic, filename = "pop_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


peak_plot <- ggplot(data = pheno_metr %>% 
                    filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                       "Pinus sylvestris","Pinus nigra", 
                                       "Eucalyptus camaldulensis","Quercus ilex", 
                                       "Quercus suber", "Quercus faginea", 
                                       "Quercus canariensis","Olea europaea",
                                       "Castanea sativa") & var == "peak") %>% 
                    mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                            "Pinus pinea" = "P. pinea",
                                            "Pinus pinaster" = "P. pinaster",
                                            "Pinus sylvestris"="P. sylvestris",
                                            "Pinus nigra"="P. nigra",
                                            "Eucalyptus camaldulensis"="E. camaldulensis",
                                            "Quercus ilex"="Q. ilex",
                                            "Quercus suber"="Q.suber",
                                            "Quercus faginea"="Q.faginea",
                                            "Quercus canariensis"="Q.canariensis",
                                            "Olea europaea"="O. europaea",
                                            "Castanea sativa"="C. sativa")),
                  aes(x = Species, y = mean, color = Species)) +
  geom_boxplot() +
  ylab("NDVI")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species  maximum value of NDVI (Peak)")

ggsave(peak_plot, device = "tiff", path = path2grafic, filename = "peak_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(peak_plot, device = "png", path = path2grafic, filename = "peak_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)

################# HISTOGRAMA CIRCULAR SOS EOS POP y POT ################

#lo hacemos en forma de ciclo
p_caducas <- ggplot() + 
  geom_histogram(data = pheno_metr_p %>% 
                   filter(Sp.x %in% c("Castanea sativa","Eucalyptus camaldulensis", "Olea europaea",
                                      "Quercus ilex", "Quercus suber"
                                      #"Quercus faginea","Quercus canariensis",
                                      )), aes(x = eos_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 10), colour = "black", fill = "yellow")+
  geom_histogram(data=pheno_metr_p %>% 
                   filter(Sp.x %in% c("Castanea sativa","Eucalyptus camaldulensis", "Olea europaea",
                                      "Quercus ilex", "Quercus suber"
                                      #"Quercus faginea","Quercus canariensis",
                   )), aes(x = sos_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 10), colour = "black", fill = "green")+
  geom_histogram(data = pheno_metr_p %>% 
                   filter(Sp.x %in% c("Castanea sativa","Eucalyptus camaldulensis", "Olea europaea",
                                      "Quercus ilex", "Quercus suber"
                                      #"Quercus faginea","Quercus canariensis",
                   )), aes(x = pop_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 10), colour = "black", fill = "blue")+
  geom_histogram(data=pheno_metr_p %>% 
                   filter(Sp.x %in% c("Castanea sativa","Eucalyptus camaldulensis", "Olea europaea",
                                      "Quercus ilex", "Quercus suber"
                                      #"Quercus faginea","Quercus canariensis",
                   )), aes(x = pot_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 15), colour = "black", fill = "orange")+
  facet_wrap(~Sp.x, nrow = 1)+ theme(strip.text.x = element_text(size = 14))

p_caducas <- p_caducas + 
  labs(x = "Day of Year (DoY)", y = "Density")+
  coord_polar()+
  scale_x_continuous(limits = c(0, 365), 
                     breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                "Jun", "Jul", "Aug", "Sep","Oct","Nov","Dec"))+ 
  theme_bw()
# p_caducas <- p_caducas + 
#   labs(title = "SOS (green), POP (blue), EOS (yelow), and POT (orange) of deciduous species") 
p_caducas

ggsave(p_caducas, device = "tiff", path = path2grafic, filename = "p_caducas.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(p_caducas, device = "png", path = path2grafic, filename = "p_caducas.png", 
       width = 12, height = 5, units = 'in', dpi = 300)



#perennes separamos las especies para que podamos ver mejor los graficos
p_perennes <- ggplot() + 
  geom_histogram(data = pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus pinea","Pinus halepensis","Pinus pinaster",
                                      "Pinus nigra", "Pinus sylvestris")), aes(x = eos_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 10), colour = "black", fill = "yellow")+
  geom_histogram(data=pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus pinea","Pinus halepensis","Pinus pinaster",
                                      "Pinus nigra", "Pinus sylvestris")), aes(x=sos_mesos_doy, y = ..density.., ),
                 breaks = seq(0,365, by = 10), colour = "black", fill = "green")+
  geom_histogram(data = pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus pinea","Pinus halepensis","Pinus pinaster",
                                      "Pinus nigra", "Pinus sylvestris")), aes(x = pop_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 10), colour = "black", fill = "blue")+
  geom_histogram(data=pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus pinea","Pinus halepensis","Pinus pinaster",
                                      "Pinus nigra", "Pinus sylvestris")), aes(x=pot_mesos_doy, y = ..density.., ),
                 breaks = seq(0,365, by = 10), colour = "black", fill = "orange")+
  facet_wrap(~Sp.x, nrow = 1)+ theme(strip.text.x = element_text(size = 15))

p_perennes <- p_perennes + 
  coord_polar()+
  scale_x_continuous(limits = c(0, 365), 
                     breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                "Jun", "Jul", "Aug", "Sep","Oct","Nov","Dec"))+ 
  labs(x = "Day of Year (DoY)", y = "Density")+
  theme_bw()

# p_perennes <- p_perennes + 
#   labs(title = "SOS (green), POP (blue), EOS (yelow), and POT (orange) of evergreen species", 
#                                 x = "Day of Year (DoY)", y = "Density") 
p_perennes


ggsave(p_perennes, device = "tiff", path = path2grafic, filename = "p_perennes.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(p_perennes, device = "png", path = path2grafic, filename = "p_perennes.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


#perennes 2
p_perennes2 <- ggplot() + 
  geom_histogram(data = pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus halepensis","Pinus pinaster",
                                      "Quercus ilex")), aes(x = eos_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 10), colour = "black", fill = "yellow")+
  geom_histogram(data=pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus halepensis","Pinus pinaster",
                                      "Quercus ilex")), aes(x=sos_mesos_doy, y = ..density.., ),
                 breaks = seq(0,365, by = 10), colour = "black", fill = "green")+
  geom_histogram(data = pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus halepensis","Pinus pinaster",
                                      "Quercus ilex")), aes(x = pop_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 10), colour = "black", fill = "blue")+
  geom_histogram(data=pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus halepensis","Pinus pinaster",
                                      "Quercus ilex")), aes(x=pot_mesos_doy, y = ..density.., ),
                 breaks = seq(0,365, by = 10), colour = "black", fill = "orange")+
  facet_wrap(~Sp.x)+ theme(strip.text.x = element_text(size = 15))

p_perennes2 <- p_perennes2 + 
  coord_polar()+
  labs(x = "Day of Year (DoY)", y = "Density")+
  scale_x_continuous(limits = c(0, 365), 
                     breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                "Jun", "Jul", "Aug", "Sep","Oct","Nov","Dec"))+ 
  theme_bw()


# p_perennes2 <- p_perennes2 + 
#   labs(title = "SOS (green), POP (blue), EOS (yelow), and POT (orange) of evergreen species", 
#                                 x = "Day of Year (DoY)", y = "Density") 
p_perennes2


ggsave(p_perennes2, device = "tiff", path = path2grafic, filename = "p_perennes2.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(p_perennes2, device = "png", path = path2grafic, filename = "p_perennes2.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


#perennes 3
p_perennes3 <- ggplot() + 
  geom_histogram(data = pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus sylvestris","Pinus nigra", 
                                      "Quercus suber")), aes(x = eos_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 10), colour = "black", fill = "yellow")+
  geom_histogram(data=pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus sylvestris","Pinus nigra", 
                                      "Quercus suber")), aes(x=sos_mesos_doy, y = ..density.., ),
                 breaks = seq(0,365, by = 10), colour = "black", fill = "green")+
  geom_histogram(data = pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus sylvestris","Pinus nigra", 
                                      "Quercus suber")), aes(x = pop_mesos_doy, y = ..density..), 
                 breaks = seq(0,365, by = 10), colour = "black", fill = "blue")+
  geom_histogram(data=pheno_metr_p %>% 
                   filter(Sp.x %in% c("Pinus sylvestris","Pinus nigra", 
                                      "Quercus suber")), aes(x=pot_mesos_doy, y = ..density.., ),
                 breaks = seq(0,365, by = 10), colour = "black", fill = "orange")+
  facet_wrap(~Sp.x)+ theme(strip.text.x = element_text(size = 15))

p_perennes3 <- p_perennes3 + 
  coord_polar()+
  labs(x = "Day of Year (DoY)", y = "Density")+
  scale_x_continuous(limits = c(0, 365), 
                     breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                "Jun", "Jul", "Aug", "Sep","Oct","Nov","Dec"))+ 
  theme_bw()
# p_perennes3 <- p_perennes3 + 
#   labs(title = "SOS (green), POP (blue), EOS (yelow), and POT (orange) of evergreen species", 
#                                  x = "Day of Year (DoY)", y = "Density") 
p_perennes3


ggsave(p_perennes3, device = "tiff", path = path2grafic, filename = "p_perennes3.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(p_perennes3, device = "png", path = path2grafic, filename = "p_perennes3.png", 
       width = 12, height = 5, units = 'in', dpi = 300)
##########################################


mau_plot <- ggplot(data = pheno_metr %>% 
                      filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                         "Pinus sylvestris","Pinus nigra", 
                                         "Eucalyptus camaldulensis","Quercus ilex", 
                                         "Quercus suber", "Quercus faginea", 
                                         "Quercus canariensis","Olea europaea",
                                         "Castanea sativa") & var == "mau") %>% 
                      mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                              "Pinus pinea" = "P. pinea",
                                              "Pinus pinaster" = "P. pinaster",
                                              "Pinus sylvestris"="P. sylvestris",
                                              "Pinus nigra"="P. nigra",
                                              "Eucalyptus camaldulensis"="E. camaldulensis",
                                              "Quercus ilex"="Q. ilex",
                                              "Quercus suber"="Q.suber",
                                              "Quercus faginea"="Q.faginea",
                                              "Quercus canariensis"="Q.canariensis",
                                              "Olea europaea"="O. europaea",
                                              "Castanea sativa"="C. sativa")),
                    aes(x = Species, y = mean, color = Species)) +
  geom_boxplot() +
  ylab("NDVI")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species NDVI value of EOS")

ggsave(mau_plot, device = "tiff", path = path2grafic, filename = "mau_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(mau_plot, device = "png", path = path2grafic, filename = "mau_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


msp_plot <- ggplot(data = pheno_metr %>% 
                     filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                        "Pinus sylvestris","Pinus nigra", 
                                        "Eucalyptus camaldulensis","Quercus ilex", 
                                        "Quercus suber", "Quercus faginea", 
                                        "Quercus canariensis","Olea europaea",
                                        "Castanea sativa") & var == "msp") %>% 
                     mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                             "Pinus pinea" = "P. pinea",
                                             "Pinus pinaster" = "P. pinaster",
                                             "Pinus sylvestris"="P. sylvestris",
                                             "Pinus nigra"="P. nigra",
                                             "Eucalyptus camaldulensis"="E. camaldulensis",
                                             "Quercus ilex"="Q. ilex",
                                             "Quercus suber"="Q.suber",
                                             "Quercus faginea"="Q.faginea",
                                             "Quercus canariensis"="Q.canariensis",
                                             "Olea europaea"="O. europaea",
                                             "Castanea sativa"="C. sativa")),
                   aes(x = Species, y = mean, color = Species)) +
  geom_boxplot() +
  ylab("NDVI")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species NDVI value of SOS")

ggsave(msp_plot, device = "tiff", path = path2grafic, filename = "msp_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(msp_plot, device = "png", path = path2grafic, filename = "msp_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


pot_plot <- ggplot(data = pheno_metr %>% 
                     filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                        "Pinus sylvestris","Pinus nigra", 
                                        "Eucalyptus camaldulensis","Quercus ilex", 
                                        "Quercus suber", "Quercus faginea", 
                                        "Quercus canariensis","Olea europaea",
                                        "Castanea sativa") & var == "pot") %>% 
                     mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                             "Pinus pinea" = "P. pinea",
                                             "Pinus pinaster" = "P. pinaster",
                                             "Pinus sylvestris"="P. sylvestris",
                                             "Pinus nigra"="P. nigra",
                                             "Eucalyptus camaldulensis"="E. camaldulensis",
                                             "Quercus ilex"="Q. ilex",
                                             "Quercus suber"="Q.suber",
                                             "Quercus faginea"="Q.faginea",
                                             "Quercus canariensis"="Q.canariensis",
                                             "Olea europaea"="O. europaea",
                                             "Castanea sativa"="C. sativa")),
                   aes(x = Species, y = mean, color = Species)) +
  geom_boxplot() +
  ylab("DoY")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species day of minimum (Trough)")


ggsave(pot_plot, device = "tiff", path = path2grafic, filename = "pot_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pot_plot, device = "png", path = path2grafic, filename = "pot_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


trough_plot <- ggplot(data = pheno_metr %>% 
                     filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                        "Pinus sylvestris","Pinus nigra", 
                                        "Eucalyptus camaldulensis","Quercus ilex", 
                                        "Quercus suber", "Quercus faginea", 
                                        "Quercus canariensis","Olea europaea",
                                        "Castanea sativa") & var == "trough") %>% 
                     mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                             "Pinus pinea" = "P. pinea",
                                             "Pinus pinaster" = "P. pinaster",
                                             "Pinus sylvestris"="P. sylvestris",
                                             "Pinus nigra"="P. nigra",
                                             "Eucalyptus camaldulensis"="E. camaldulensis",
                                             "Quercus ilex"="Q. ilex",
                                             "Quercus suber"="Q.suber",
                                             "Quercus faginea"="Q.faginea",
                                             "Quercus canariensis"="Q.canariensis",
                                             "Olea europaea"="O. europaea",
                                             "Castanea sativa"="C. sativa")),
                   aes(x = Species, y = mean, color = Species)) +
  geom_boxplot() +
  ylab("NDVI")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species minimum value of NDVI (Trough)")


ggsave(trough_plot, device = "tiff", path = path2grafic, filename = "trough_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(trough_plot, device = "png", path = path2grafic, filename = "trough_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)


#para poder montar las letras del post hoc (TUKEYS) 
#https://www.mathiasecology.com/code/add-tukeys-significant-letters-to-ggplots
library(egg)
library(multcompView)

pheno_metr_nna <- na.omit(pheno_metr_p) %>% 
  dplyr::select(Sp.x, mean_eos:mean_trough) %>% 
  filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                     "Pinus sylvestris","Pinus nigra", 
                     "Eucalyptus camaldulensis","Quercus ilex", 
                     "Quercus suber", "Quercus faginea", 
                     "Quercus canariensis","Olea europaea",
                     "Castanea sativa")) %>% 
  mutate(Sp.x = as.factor(Sp.x)) 
















