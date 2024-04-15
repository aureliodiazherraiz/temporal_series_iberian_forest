#Vamos a categorizar los años en secos (2005, 1994, 2015), humedos (1996, 1997, 2010) y medios (2003, 2006, 2008)
#usamos el dataframe annual_metrics_mean

annual_metrics_kk <- annual_metrics %>% 
  mutate(regime = ifelse(Year %in% c(1996, 1997, 2010), "h",
                         ifelse(Year %in% c(2005, 1994, 2015), "s",
                                ifelse(Year %in% c(2003, 2009, 2008), "m", NA))))

#ahora vamos a aplicar kruskall directamente en unn grafico de bigotes para cada especie
my_comparisons <- list( c("s", "h"), c("s", "m"), c("h", "m"))
sos_kk_plot <- ggplot(data = annual_metrics_kk %>% 
                        na.omit() %>% 
         filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                            "Pinus sylvestris","Pinus nigra", 
                            "Eucalyptus camaldulensis","Quercus ilex", 
                            "Quercus suber", "Quercus faginea", 
                            "Quercus canariensis","Olea europaea",
                            "Castanea sativa") & var == "sos") %>% 
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
       aes(x = regime, y = Value, color = regime)) +
  geom_boxplot() +
  facet_wrap(~Sp.x)+
  ylab("DoY")+
  theme_bw()+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 600)+ # Add global p-value
  ggtitle("Species Start Of Season (SOS)")

sos_kk_plot

ggsave(sos_kk_plot, device = "tiff", path = path2grafic, filename = "sos_kk_plot.tiff", 
       width = 12, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sos_kk_plot, device = "png", path = path2grafic, filename = "sos_kk_plot.png", 
       width = 12, height = 10, units = 'in', dpi = 300)


### EOS
eos_kk_plot <- ggplot(data = annual_metrics_kk %>% 
                        na.omit() %>% 
                        filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                           "Pinus sylvestris","Pinus nigra", 
                                           "Eucalyptus camaldulensis","Quercus ilex", 
                                           "Quercus suber", "Quercus faginea", 
                                           "Quercus canariensis","Olea europaea",
                                           "Castanea sativa") & var == "eos") %>% 
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
                      aes(x = regime, y = Value, color = regime)) +
  geom_boxplot() +
  facet_wrap(~Sp.x)+
  ylab("DoY")+
  theme_bw()+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 600)+ # Add global p-value
  ggtitle("Species Start Of Season (EOS)")

eos_kk_plot

ggsave(eos_kk_plot, device = "tiff", path = path2grafic, filename = "eos_kk_plot.tiff", 
       width = 12, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(eos_kk_plot, device = "png", path = path2grafic, filename = "eos_kk_plot.png", 
       width = 12, height = 10, units = 'in', dpi = 300)


### LOS

los_kk_plot <- ggplot(data = annual_metrics_kk %>% 
                        na.omit() %>% 
                        filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                           "Pinus sylvestris","Pinus nigra", 
                                           "Eucalyptus camaldulensis","Quercus ilex", 
                                           "Quercus suber", "Quercus faginea", 
                                           "Quercus canariensis","Olea europaea",
                                           "Castanea sativa") & var == "los") %>% 
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
                      aes(x = regime, y = Value, color = regime)) +
  geom_boxplot() +
  facet_wrap(~Sp.x)+
  ylab("DoY")+
  theme_bw()+
  #stat_compare_means(aes(group = regime),label =  "p.signif", label.x = 2)+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 600)+ # Add global p-value
  ggtitle("Species Lengh Of Season (los)")

los_kk_plot

ggsave(los_kk_plot, device = "tiff", path = path2grafic, filename = "los_kk_plot.tiff", 
       width = 12, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_kk_plot, device = "png", path = path2grafic, filename = "los_kk_plot.png", 
       width = 12, height = 10, units = 'in', dpi = 300)



