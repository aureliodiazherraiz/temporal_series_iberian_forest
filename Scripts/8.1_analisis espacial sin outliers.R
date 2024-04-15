#eliminacion de los outliers

#hemos encontrado esta funcion pàra eliminar los outliers


#### SOS ####
#halep
hale_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus halepensis") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(hale_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(hale_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(hale_sos_outliers$sos_mesos_doy)

hale_sos_outliers <- subset(hale_sos_outliers, 
                   hale_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                     hale_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))

#sativa
sativa_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Castanea sativa") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(sativa_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(sativa_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(sativa_sos_outliers$sos_mesos_doy)

sativa_sos_outliers <- subset(sativa_sos_outliers, 
                            sativa_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                              sativa_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))
#europaea
euro_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Olea europaea") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(euro_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(euro_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(euro_sos_outliers$sos_mesos_doy)

euro_sos_outliers <- subset(euro_sos_outliers, 
                            euro_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                              euro_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))

#eucal
eucal_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Eucalyptus camaldulensis") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(eucal_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(eucal_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(eucal_sos_outliers$sos_mesos_doy)

eucal_sos_outliers <- subset(eucal_sos_outliers, 
                            eucal_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                              eucal_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))

#nigra
nigra_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus nigra") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(nigra_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(nigra_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(nigra_sos_outliers$sos_mesos_doy)

nigra_sos_outliers <- subset(nigra_sos_outliers, 
                            nigra_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                              nigra_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))

#pinea
pinea_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinea") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(pinea_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(pinea_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(pinea_sos_outliers$sos_mesos_doy)

pinea_sos_outliers <- subset(pinea_sos_outliers, 
                            pinea_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                              pinea_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))

#pinaster
pinaster_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinaster") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(pinaster_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(pinaster_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(pinaster_sos_outliers$sos_mesos_doy)

pinaster_sos_outliers <- subset(pinaster_sos_outliers, 
                            pinaster_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                              pinaster_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))

#sylves
sylves_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus sylvestris") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(sylves_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(sylves_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(sylves_sos_outliers$sos_mesos_doy)

sylves_sos_outliers <- subset(sylves_sos_outliers, 
                            sylves_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                              sylves_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))

#ilex
ilex_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus ilex") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(ilex_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(ilex_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(ilex_sos_outliers$sos_mesos_doy)

ilex_sos_outliers <- subset(ilex_sos_outliers, 
                            ilex_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                              ilex_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))

#suber
suber_sos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus suber") %>% 
  dplyr::select(Sp.x, ID, sos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(suber_sos_outliers$sos_mesos_doy, .25)

Q3 <- quantile(suber_sos_outliers$sos_mesos_doy, .75)

IQR <- IQR(suber_sos_outliers$sos_mesos_doy)

suber_sos_outliers <- subset(suber_sos_outliers, 
                            suber_sos_outliers$sos_mesos_doy> (Q1 - 1.5*IQR) & 
                              suber_sos_outliers$sos_mesos_doy< (Q3 + 1.5*IQR))

#juntamos todos los valores para poder graficarlos juntos
sp.outlier <- rbind(suber_sos_outliers, ilex_sos_outliers, hale_sos_outliers, 
                    sativa_sos_outliers, eucal_sos_outliers, euro_sos_outliers,
                    sylves_sos_outliers, nigra_sos_outliers, pinea_sos_outliers,
                    pinaster_sos_outliers)


sos_Arid_plot <- ggplot(data=sp.outlier,
                        aes(x=Arid_Dm_i, y=sos_mesos_doy))+
  geom_point()+
  ggtitle("Regression between Aridity and SOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  labs(x="Aridity", y="SOS (DoY)")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 14)) + 
  theme_bar

ggsave(sos_Arid_plot, device = "tiff", path = path2grafic, filename = "sos_doy_outliers_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sos_Arid_plot, device = "png", path = path2grafic, filename = "sos_doy_outliers_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)



#rafa quiere meter en un mismo grafico varias especies y asi poder comparar

colores <- c("Castanea sativa" = "#BFE8B2", "Eucalyptus camaldulensis" = "#FFCC9E", "Olea europaea" = "#F4D166",
             "Pinus halepensis" = "#000000", "Pinus nigra" = "#c90076", "Pinus pinaster" = "#e23c00", 
             "Pinus pinea" = "#de9e43", "Pinus sylvestris" = "#50e4e4", "Quercus ilex" = "#4ede79",
             "Quercus suber"= "#0041ab")
sos_Arid_plot_lines <- ggplot()+
  geom_smooth(data=sp.outlier %>%
                filter(Sp.x=="Castanea sativa"),
              aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Castanea sativa", size=1.5), method = "lm")+
  # # geom_smooth(data=sp.outlier %>% 
  #               filter(Sp.x=="Eucalyptus camaldulensis"),
  #             aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Eucalyptus camaldulensis"), method = "lm")+
  # geom_smooth(data=sp.outlier %>% 
  #               filter(Sp.x=="Olea europaea"),
  #             aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Olea europaea"), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
              filter(Sp.x=="Pinus halepensis"),
            aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Pinus halepensis", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
              filter(Sp.x=="Pinus nigra"),
            aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Pinus nigra", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus pinaster"),
              aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Pinus pinaster", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus pinea"),
              aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Pinus pinea", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus sylvestris"),
              aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Pinus sylvestris", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Quercus ilex"),
              aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Quercus ilex", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Quercus suber"),
              aes(x=Arid_Dm_i, y=sos_mesos_doy, colour = "Quercus suber", size=1.5), method = "lm")+
  scale_colour_manual(limits = c("Castanea sativa","Eucalyptus camaldulensis", "Olea europaea",
                                 "Pinus halepensis","Pinus nigra","Pinus pinaster",
                                 "Pinus pinea","Pinus sylvestris","Quercus ilex","Quercus suber"),
                      values = colores)+
  # scale_colour_manual(values =  c("#BFE8B2", "#FFCC9E", "#fff24a","#000000","#c90076","#e23c00","#de9e43","#50e4e4","#4ede79","#0041ab"),
  #                     labels = c("Castanea sativa", "Eucalyptus camaldulensis", "Olea europaea", "Pinus halepensis", 
  #                                "Pinus nigra", "Pinus pinaster", "Pinus pinea", "Pinus sylvestris", "Quercus ilex", "Quercus suber")) +
  #ggtitle("Aridity and SOS regression")+
  geom_smooth(method = "lm")+
  labs(y="SOS (DoY)",
       x="Aridity index")+
  theme_bar+
  theme(axis.text = element_text( size = 24 ),
        axis.text.x = element_text( size = 28 ),
        axis.title = element_text( size = 26, face = "bold"),
        #legend.position="none",
        # The new stuff
        strip.text = element_text(size = 24))
  

sos_Arid_plot_lines


ggsave(sos_Arid_plot_lines, device = "png", path = path2grafic, filename = "sos_Arid_plot_lines.png", 
       width = 16, height = 10, units = 'in', dpi = 300)




#### EOS ####
#halep
hale_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus halepensis") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(hale_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(hale_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(hale_eos_outliers$eos_mesos_doy)

hale_eos_outliers <- subset(hale_eos_outliers, 
                            hale_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                              hale_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))

#sativa
sativa_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Castanea sativa") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(sativa_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(sativa_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(sativa_eos_outliers$eos_mesos_doy)

sativa_eos_outliers <- subset(sativa_eos_outliers, 
                              sativa_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                                sativa_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))
#europaea
euro_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Olea europaea") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(euro_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(euro_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(euro_eos_outliers$eos_mesos_doy)

euro_eos_outliers <- subset(euro_eos_outliers, 
                            euro_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                              euro_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))

#eucal
eucal_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Eucalyptus camaldulensis") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(eucal_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(eucal_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(eucal_eos_outliers$eos_mesos_doy)

eucal_eos_outliers <- subset(eucal_eos_outliers, 
                             eucal_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                               eucal_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))

#nigra
nigra_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus nigra") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(nigra_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(nigra_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(nigra_eos_outliers$eos_mesos_doy)

nigra_eos_outliers <- subset(nigra_eos_outliers, 
                             nigra_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                               nigra_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))

#pinea
pinea_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinea") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(pinea_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(pinea_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(pinea_eos_outliers$eos_mesos_doy)

pinea_eos_outliers <- subset(pinea_eos_outliers, 
                             pinea_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                               pinea_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))

#pinaster
pinaster_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinaster") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(pinaster_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(pinaster_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(pinaster_eos_outliers$eos_mesos_doy)

pinaster_eos_outliers <- subset(pinaster_eos_outliers, 
                                pinaster_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                                  pinaster_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))

#sylves
sylves_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus sylvestris") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

sylves_eos_outliers <- sylves_eos_outliers %>% 
  mutate(eos_mesos_doy = ifelse(eos_mesos_doy<100, eos_mesos_doy+365, eos_mesos_doy))
#ojo, sumamos 365 para que los puntos esten agrupados y la regresion pueda hacerse con logica pues los valores son ciclicos

Q1 <- quantile(sylves_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(sylves_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(sylves_eos_outliers$eos_mesos_doy)

sylves_eos_outliers <- subset(sylves_eos_outliers, 
                              sylves_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                                sylves_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))


#ilex
ilex_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus ilex") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(ilex_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(ilex_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(ilex_eos_outliers$eos_mesos_doy)

ilex_eos_outliers <- subset(ilex_eos_outliers, 
                            ilex_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                              ilex_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))

#suber
suber_eos_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus suber") %>% 
  dplyr::select(Sp.x, ID, eos_mesos_doy, Arid_Dm_i)

Q1 <- quantile(suber_eos_outliers$eos_mesos_doy, .25)

Q3 <- quantile(suber_eos_outliers$eos_mesos_doy, .75)

IQR <- IQR(suber_eos_outliers$eos_mesos_doy)

suber_eos_outliers <- subset(suber_eos_outliers, 
                             suber_eos_outliers$eos_mesos_doy> (Q1 - 1.5*IQR) & 
                               suber_eos_outliers$eos_mesos_doy< (Q3 + 1.5*IQR))

#juntamos todos los valores para poder graficarlos juntos
sp.outlier <- rbind(suber_eos_outliers, ilex_eos_outliers, hale_eos_outliers, 
                    sativa_eos_outliers, eucal_eos_outliers, euro_eos_outliers,
                    sylves_eos_outliers, nigra_eos_outliers, pinea_eos_outliers,
                    pinaster_eos_outliers)


eos_Arid_plot <- ggplot(data=sp.outlier,
                        aes(x=Arid_Dm_i, y=eos_mesos_doy))+
  geom_point()+
  ggtitle("Regression between Aridity and EOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  labs(x="Aridity", y="EOS (DoY)")+
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 14)) + 
  theme_bar

eos_Arid_plot

ggsave(eos_Arid_plot, device = "tiff", path = path2grafic, filename = "eos_doy_outlier_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(eos_Arid_plot, device = "png", path = path2grafic, filename = "eos_doy_outlier_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#hacemos lo mismo para el EOS juntando apenas aquellas que son significativas

eos_Arid_plot_lines <- ggplot()+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus halepensis"),
              aes(x=Arid_Dm_i, y=eos_mesos_doy, colour = "Pinus halepensis", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus nigra"),
              aes(x=Arid_Dm_i, y=eos_mesos_doy, colour = "Pinus nigra", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus pinaster"),
              aes(x=Arid_Dm_i, y=eos_mesos_doy, colour = "Pinus pinaster", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus sylvestris"),
              aes(x=Arid_Dm_i, y=eos_mesos_doy, colour = "Pinus sylvestris", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Quercus suber"),
              aes(x=Arid_Dm_i, y=eos_mesos_doy, colour = "Quercus suber", size=1.5), method = "lm")+
  scale_colour_manual(values =  c("#000000","#c90076","#e23c00","#50e4e4","#0041ab"))+
  #ggtitle("Aridity and EOS regression")+
  labs(y="EOS (DoY)",
       x="Aridity index")+
  theme_bar

eos_Arid_plot_lines


#### POT ####
#halep
hale_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus halepensis") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(hale_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(hale_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(hale_pot_outliers$pot_mesos_doy)

hale_pot_outliers <- subset(hale_pot_outliers, 
                            hale_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                              hale_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))

#sativa
sativa_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Castanea sativa") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(sativa_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(sativa_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(sativa_pot_outliers$pot_mesos_doy)

sativa_pot_outliers <- subset(sativa_pot_outliers, 
                              sativa_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                                sativa_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))
#europaea
euro_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Olea europaea") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(euro_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(euro_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(euro_pot_outliers$pot_mesos_doy)

euro_pot_outliers <- subset(euro_pot_outliers, 
                            euro_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                              euro_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))

#eucal
eucal_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Eucalyptus camaldulensis") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(eucal_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(eucal_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(eucal_pot_outliers$pot_mesos_doy)

eucal_pot_outliers <- subset(eucal_pot_outliers, 
                             eucal_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                               eucal_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))

#nigra
nigra_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus nigra") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(nigra_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(nigra_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(nigra_pot_outliers$pot_mesos_doy)

nigra_pot_outliers <- subset(nigra_pot_outliers, 
                             nigra_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                               nigra_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))

#pinea
pinea_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinea") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(pinea_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(pinea_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(pinea_pot_outliers$pot_mesos_doy)

pinea_pot_outliers <- subset(pinea_pot_outliers, 
                             pinea_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                               pinea_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))

#pinaster
pinaster_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinaster") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(pinaster_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(pinaster_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(pinaster_pot_outliers$pot_mesos_doy)

pinaster_pot_outliers <- subset(pinaster_pot_outliers, 
                                pinaster_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                                  pinaster_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))

#sylves
sylves_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus sylvestris") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(sylves_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(sylves_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(sylves_pot_outliers$pot_mesos_doy)

sylves_pot_outliers <- subset(sylves_pot_outliers, 
                              sylves_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                                sylves_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))

#ilex
ilex_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus ilex") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(ilex_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(ilex_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(ilex_pot_outliers$pot_mesos_doy)

ilex_pot_outliers <- subset(ilex_pot_outliers, 
                            ilex_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                              ilex_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))

#suber
suber_pot_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus suber") %>% 
  dplyr::select(Sp.x, ID, pot_mesos_doy, Arid_Dm_i)

Q1 <- quantile(suber_pot_outliers$pot_mesos_doy, .25)

Q3 <- quantile(suber_pot_outliers$pot_mesos_doy, .75)

IQR <- IQR(suber_pot_outliers$pot_mesos_doy)

suber_pot_outliers <- subset(suber_pot_outliers, 
                             suber_pot_outliers$pot_mesos_doy> (Q1 - 1.5*IQR) & 
                               suber_pot_outliers$pot_mesos_doy< (Q3 + 1.5*IQR))

#juntamos todos los valores para poder graficarlos juntos
sp.outlier <- rbind(suber_pot_outliers, ilex_pot_outliers, hale_pot_outliers, 
                    sativa_pot_outliers, eucal_pot_outliers, euro_pot_outliers,
                    sylves_pot_outliers, nigra_pot_outliers, pinea_pot_outliers,
                    pinaster_pot_outliers)


pot_Arid_plot_out <- ggplot(data=sp.outlier,
                        aes(x=Arid_Dm_i, y=pot_mesos_doy))+
  geom_point()+
  ggtitle("Regression between Aridity and POT")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 150, 
           size = 3.5) +
  labs(x="Aridity", y="POT (DoY)")+
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 14)) + 
  theme_bar

ggsave(pot_Arid_plot_out, device = "tiff", path = path2grafic, filename = "pot_doy_outlier_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pot_Arid_plot_out, device = "png", path = path2grafic, filename = "pot_doy_outlier_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)



#### POP ####
## para el caso del pop debido a caracter ciclico de los datos creoq ue deberiamos operar (artefacto matematico) para 
## evitar eliminar datos y despues aplicar la limpieza de los outliers
##

dt.365 <- pheno_metr_p_clima %>% 
  mutate(pop_mesos_doy_365 = ifelse(pop_mesos_doy<110, pop_mesos_doy+365, pop_mesos_doy))

#limpieza de outliers
#halep
hale_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Pinus halepensis") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(hale_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(hale_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(hale_pop_outliers$pop_mesos_doy_365)

hale_pop_outliers <- subset(hale_pop_outliers, 
                            hale_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                              hale_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))

#sativa
sativa_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Castanea sativa") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(sativa_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(sativa_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(sativa_pop_outliers$pop_mesos_doy_365)

sativa_pop_outliers <- subset(sativa_pop_outliers, 
                              sativa_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                                sativa_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))
#europaea
euro_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Olea europaea") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(euro_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(euro_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(euro_pop_outliers$pop_mesos_doy_365)

euro_pop_outliers <- subset(euro_pop_outliers, 
                            euro_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                              euro_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))

#eucal
eucal_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Eucalyptus camaldulensis") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(eucal_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(eucal_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(eucal_pop_outliers$pop_mesos_doy_365)

eucal_pop_outliers <- subset(eucal_pop_outliers, 
                             eucal_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                               eucal_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))

#nigra
nigra_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Pinus nigra") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(nigra_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(nigra_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(nigra_pop_outliers$pop_mesos_doy_365)

nigra_pop_outliers <- subset(nigra_pop_outliers, 
                             nigra_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                               nigra_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))

#pinea
pinea_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Pinus pinea") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(pinea_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(pinea_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(pinea_pop_outliers$pop_mesos_doy_365)

pinea_pop_outliers <- subset(pinea_pop_outliers, 
                             pinea_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                               pinea_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))

#pinaster
pinaster_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Pinus pinaster") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(pinaster_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(pinaster_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(pinaster_pop_outliers$pop_mesos_doy_365)

pinaster_pop_outliers <- subset(pinaster_pop_outliers, 
                                pinaster_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                                  pinaster_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))

#sylves
sylves_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Pinus sylvestris") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(sylves_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(sylves_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(sylves_pop_outliers$pop_mesos_doy_365)

sylves_pop_outliers <- subset(sylves_pop_outliers, 
                              sylves_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                                sylves_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))

#ilex
ilex_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Quercus ilex") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(ilex_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(ilex_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(ilex_pop_outliers$pop_mesos_doy_365)

ilex_pop_outliers <- subset(ilex_pop_outliers, 
                            ilex_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                              ilex_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))

#suber
suber_pop_outliers <- dt.365 %>% 
  filter(Sp.x=="Quercus suber") %>% 
  dplyr::select(Sp.x, ID, pop_mesos_doy_365, Arid_Dm_i)

Q1 <- quantile(suber_pop_outliers$pop_mesos_doy_365, .25)

Q3 <- quantile(suber_pop_outliers$pop_mesos_doy_365, .75)

IQR <- IQR(suber_pop_outliers$pop_mesos_doy_365)

suber_pop_outliers <- subset(suber_pop_outliers, 
                             suber_pop_outliers$pop_mesos_doy_365> (Q1 - 1.5*IQR) & 
                               suber_pop_outliers$pop_mesos_doy_365< (Q3 + 1.5*IQR))

#juntamos todos los valores para poder graficarlos juntos
sp.outlier <- rbind(suber_pop_outliers, ilex_pop_outliers, hale_pop_outliers, 
                    sativa_pop_outliers, eucal_pop_outliers, euro_pop_outliers,
                    sylves_pop_outliers, nigra_pop_outliers, pinea_pop_outliers,
                    pinaster_pop_outliers)


pop_Arid_plot_out <- ggplot(data=sp.outlier,
                            aes(x=Arid_Dm_i, y=pop_mesos_doy_365))+
  geom_point()+
  ggtitle("Regression between Aridity and POP")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  labs(x="Aridity", y="POP (DoY)")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 14)) + 
  theme_bar

ggsave(pop_Arid_plot_out, device = "tiff", path = path2grafic, filename = "pop_doy_outlier_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pop_Arid_plot_out, device = "png", path = path2grafic, filename = "pop_doy_outlier_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)



#### LOS ####
#halep
hale_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus halepensis") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(hale_los_outliers$los, .25)

Q3 <- quantile(hale_los_outliers$los, .75)

IQR <- IQR(hale_los_outliers$los)

hale_los_outliers <- subset(hale_los_outliers, 
                            hale_los_outliers$los> (Q1 - 1.5*IQR) & 
                              hale_los_outliers$los< (Q3 + 1.5*IQR))

#sativa
sativa_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Castanea sativa") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(sativa_los_outliers$los, .25)

Q3 <- quantile(sativa_los_outliers$los, .75)

IQR <- IQR(sativa_los_outliers$los)

sativa_los_outliers <- subset(sativa_los_outliers, 
                              sativa_los_outliers$los> (Q1 - 1.5*IQR) & 
                                sativa_los_outliers$los< (Q3 + 1.5*IQR))
#europaea
euro_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Olea europaea") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(euro_los_outliers$los, .25)

Q3 <- quantile(euro_los_outliers$los, .75)

IQR <- IQR(euro_los_outliers$los)

euro_los_outliers <- subset(euro_los_outliers, 
                            euro_los_outliers$los> (Q1 - 1.5*IQR) & 
                              euro_los_outliers$los< (Q3 + 1.5*IQR))

#eucal
eucal_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Eucalyptus camaldulensis") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(eucal_los_outliers$los, .25)

Q3 <- quantile(eucal_los_outliers$los, .75)

IQR <- IQR(eucal_los_outliers$los)

eucal_los_outliers <- subset(eucal_los_outliers, 
                             eucal_los_outliers$los> (Q1 - 1.5*IQR) & 
                               eucal_los_outliers$los< (Q3 + 1.5*IQR))

#nigra
nigra_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus nigra") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(nigra_los_outliers$los, .25)

Q3 <- quantile(nigra_los_outliers$los, .75)

IQR <- IQR(nigra_los_outliers$los)

nigra_los_outliers <- subset(nigra_los_outliers, 
                             nigra_los_outliers$los> (Q1 - 1.5*IQR) & 
                               nigra_los_outliers$los< (Q3 + 1.5*IQR))

#pinea
pinea_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinea") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(pinea_los_outliers$los, .25)

Q3 <- quantile(pinea_los_outliers$los, .75)

IQR <- IQR(pinea_los_outliers$los)

pinea_los_outliers <- subset(pinea_los_outliers, 
                             pinea_los_outliers$los> (Q1 - 1.5*IQR) & 
                               pinea_los_outliers$los< (Q3 + 1.5*IQR))

#pinaster
pinaster_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinaster") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(pinaster_los_outliers$los, .25)

Q3 <- quantile(pinaster_los_outliers$los, .75)

IQR <- IQR(pinaster_los_outliers$los)

pinaster_los_outliers <- subset(pinaster_los_outliers, 
                                pinaster_los_outliers$los> (Q1 - 1.5*IQR) & 
                                  pinaster_los_outliers$los< (Q3 + 1.5*IQR))

#sylves
sylves_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus sylvestris") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(sylves_los_outliers$los, .25)

Q3 <- quantile(sylves_los_outliers$los, .75)

IQR <- IQR(sylves_los_outliers$los)

sylves_los_outliers <- subset(sylves_los_outliers, 
                              sylves_los_outliers$los> (Q1 - 1.5*IQR) & 
                                sylves_los_outliers$los< (Q3 + 1.5*IQR))

#ilex
ilex_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus ilex") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(ilex_los_outliers$los, .25)

Q3 <- quantile(ilex_los_outliers$los, .75)

IQR <- IQR(ilex_los_outliers$los)

ilex_los_outliers <- subset(ilex_los_outliers, 
                            ilex_los_outliers$los> (Q1 - 1.5*IQR) & 
                              ilex_los_outliers$los< (Q3 + 1.5*IQR))

#suber
suber_los_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus suber") %>% 
  dplyr::select(Sp.x, ID, los, Arid_Dm_i)

Q1 <- quantile(suber_los_outliers$los, .25)

Q3 <- quantile(suber_los_outliers$los, .75)

IQR <- IQR(suber_los_outliers$los)

suber_los_outliers <- subset(suber_los_outliers, 
                             suber_los_outliers$los> (Q1 - 1.5*IQR) & 
                               suber_los_outliers$los< (Q3 + 1.5*IQR))

#juntamos todos los valores para poder graficarlos juntos
sp.outlier <- rbind(suber_los_outliers, ilex_los_outliers, hale_los_outliers, 
                    sativa_los_outliers, eucal_los_outliers, euro_los_outliers,
                    sylves_los_outliers, nigra_los_outliers, pinea_los_outliers,
                    pinaster_los_outliers)


los_Arid_plot <- ggplot(data=sp.outlier,
                        aes(x=Arid_Dm_i, y=los))+
  geom_point()+
  ggtitle("Regression between Aridity and LOS")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 140, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  labs(x="Aridity", y="LOS (N of days)")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 14)) + 
  theme_bar

ggsave(los_Arid_plot, device = "tiff", path = path2grafic, filename = "los_doy_outliers_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_Arid_plot, device = "png", path = path2grafic, filename = "los_doy_outliers_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


los_Arid_violinplot <- ggplot(data=sp.outlier %>% 
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
                              aes(x = Species, y = los, color = leaf)) +
  geom_violin()+
  ylab("Number of Days of LOS")+
  stat_compare_means()+
  ggtitle("Species Length Of Season (LOS)")+
  theme_bar+
  theme(legend.position="bottom")

ggsave(los_Arid_violinplot, device = "tiff", path = path2grafic, filename = "los_Arid_violinplot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_Arid_violinplot, device = "png", path = path2grafic, filename = "los_Arid_violinplot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#repetimos el grafico unificando todas las especies significativas

los_Arid_plot_lines <- ggplot()+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Eucalyptus camaldulensis"),
              aes(x=Arid_Dm_i, y=los, colour = "Eucalyptus camaldulensis", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Olea europaea"),
              aes(x=Arid_Dm_i, y=los, colour = "Olea europaea", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus halepensis"),
              aes(x=Arid_Dm_i, y=los, colour = "Pinus halepensis", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus pinaster"),
              aes(x=Arid_Dm_i, y=los, colour = "Pinus pinaster", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus pinea"),
              aes(x=Arid_Dm_i, y=los, colour = "Pinus pinea", size=1.5), method = "lm")+
  geom_smooth(data=sp.outlier %>% 
                filter(Sp.x=="Pinus sylvestris"),
              aes(x=Arid_Dm_i, y=los, colour = "Pinus sylvestris", size=1.5), method = "lm")+
  scale_colour_manual(values =  c("#BEC45C", "#F69541","#000000","#e23c00","#de9e43","#50e4e4"))+
  #ggtitle("Aridity and LOS regression")+
  geom_smooth(method = "lm")+
  labs(y="LOS (Days)",
       x="Aridity index")+
  theme_bar

# los_Arid_plot_lines <- ggplot()+
#   geom_smooth(data=sp.outlier,
#               aes(x=Arid_Dm_i, y=los, colour = as.factor(Sp.x)), method = "lm")+
#   #scale_colour_manual(values =  c("#BEC45C", "#F69541","#000000","#e23c00","#de9e43","#50e4e4"))+
#   #ggtitle("Aridity and LOS regression")+
#   geom_smooth(method = "lm")+
#   labs(y="LOS (Days)",
#        x="Aridity index")+
#   theme_bar

los_Arid_plot_lines


#sumamos los tres graficos SOS, EOS y LOS por especies para despues exportar

plot_lines <- ggarrange(sos_Arid_plot_lines, eos_Arid_plot_lines, los_Arid_plot_lines,
          labels = c("A)", "B)", "C)"),
          common.legend = T,
          legend = "top",
          align = "hv",
          widths = 1,
          heights = 1,
          ncol = 3, nrow = 1)

ggsave(plot_lines, device = "tiff", path = path2grafic, filename = "plot_lines.tiff", 
       width = 22, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(plot_lines, device = "png", path = path2grafic, filename = "plot_lines.png", 
       width = 22, height = 10, units = 'in', dpi = 300)



#### peak ####
#halep
hale_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus halepensis") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(hale_peak_outliers$peak, .25)

Q3 <- quantile(hale_peak_outliers$peak, .75)

IQR <- IQR(hale_peak_outliers$peak)

hale_peak_outliers <- subset(hale_peak_outliers, 
                            hale_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                              hale_peak_outliers$peak< (Q3 + 1.5*IQR))

#sativa
sativa_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Castanea sativa") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(sativa_peak_outliers$peak, .25)

Q3 <- quantile(sativa_peak_outliers$peak, .75)

IQR <- IQR(sativa_peak_outliers$peak)

sativa_peak_outliers <- subset(sativa_peak_outliers, 
                              sativa_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                                sativa_peak_outliers$peak< (Q3 + 1.5*IQR))
#europaea
euro_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Olea europaea") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(euro_peak_outliers$peak, .25)

Q3 <- quantile(euro_peak_outliers$peak, .75)

IQR <- IQR(euro_peak_outliers$peak)

euro_peak_outliers <- subset(euro_peak_outliers, 
                            euro_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                              euro_peak_outliers$peak< (Q3 + 1.5*IQR))

#eucal
eucal_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Eucalyptus camaldulensis") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(eucal_peak_outliers$peak, .25)

Q3 <- quantile(eucal_peak_outliers$peak, .75)

IQR <- IQR(eucal_peak_outliers$peak)

eucal_peak_outliers <- subset(eucal_peak_outliers, 
                             eucal_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                               eucal_peak_outliers$peak< (Q3 + 1.5*IQR))

#nigra
nigra_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus nigra") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(nigra_peak_outliers$peak, .25)

Q3 <- quantile(nigra_peak_outliers$peak, .75)

IQR <- IQR(nigra_peak_outliers$peak)

nigra_peak_outliers <- subset(nigra_peak_outliers, 
                             nigra_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                               nigra_peak_outliers$peak< (Q3 + 1.5*IQR))

#pinea
pinea_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinea") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(pinea_peak_outliers$peak, .25)

Q3 <- quantile(pinea_peak_outliers$peak, .75)

IQR <- IQR(pinea_peak_outliers$peak)

pinea_peak_outliers <- subset(pinea_peak_outliers, 
                             pinea_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                               pinea_peak_outliers$peak< (Q3 + 1.5*IQR))

#pinaster
pinaster_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinaster") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(pinaster_peak_outliers$peak, .25)

Q3 <- quantile(pinaster_peak_outliers$peak, .75)

IQR <- IQR(pinaster_peak_outliers$peak)

pinaster_peak_outliers <- subset(pinaster_peak_outliers, 
                                pinaster_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                                  pinaster_peak_outliers$peak< (Q3 + 1.5*IQR))

#sylves
sylves_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus sylvestris") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(sylves_peak_outliers$peak, .25)

Q3 <- quantile(sylves_peak_outliers$peak, .75)

IQR <- IQR(sylves_peak_outliers$peak)

sylves_peak_outliers <- subset(sylves_peak_outliers, 
                              sylves_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                                sylves_peak_outliers$peak< (Q3 + 1.5*IQR))

#ilex
ilex_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus ilex") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(ilex_peak_outliers$peak, .25)

Q3 <- quantile(ilex_peak_outliers$peak, .75)

IQR <- IQR(ilex_peak_outliers$peak)

ilex_peak_outliers <- subset(ilex_peak_outliers, 
                            ilex_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                              ilex_peak_outliers$peak< (Q3 + 1.5*IQR))

#suber
suber_peak_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus suber") %>% 
  dplyr::select(Sp.x, ID, peak, Arid_Dm_i)

Q1 <- quantile(suber_peak_outliers$peak, .25)

Q3 <- quantile(suber_peak_outliers$peak, .75)

IQR <- IQR(suber_peak_outliers$peak)

suber_peak_outliers <- subset(suber_peak_outliers, 
                             suber_peak_outliers$peak> (Q1 - 1.5*IQR) & 
                               suber_peak_outliers$peak< (Q3 + 1.5*IQR))

#juntamos todos peak valores para poder graficarpeak juntos
sp.outlier <- rbind(suber_peak_outliers, ilex_peak_outliers, hale_peak_outliers, 
                    sativa_peak_outliers, eucal_peak_outliers, euro_peak_outliers,
                    sylves_peak_outliers, nigra_peak_outliers, pinea_peak_outliers,
                    pinaster_peak_outliers)


peak_Arid_plot <- ggplot(data=sp.outlier,
                        aes(x=Arid_Dm_i, y=peak))+
  geom_point()+
  ggtitle("Regression between Aridity and PEAK")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 0.75, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  labs(x="Aridity", y="PEAK (NDVI value)")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 14)) + 
  theme_bar

ggsave(peak_Arid_plot, device = "tiff", path = path2grafic, filename = "peak_doy_outliers_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(peak_Arid_plot, device = "png", path = path2grafic, filename = "peak_doy_outliers_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#### trough ####
#halep
hale_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus halepensis") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(hale_trough_outliers$trough, .25)

Q3 <- quantile(hale_trough_outliers$trough, .75)

IQR <- IQR(hale_trough_outliers$trough)

hale_trough_outliers <- subset(hale_trough_outliers, 
                             hale_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                               hale_trough_outliers$trough< (Q3 + 1.5*IQR))

#sativa
sativa_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Castanea sativa") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(sativa_trough_outliers$trough, .25)

Q3 <- quantile(sativa_trough_outliers$trough, .75)

IQR <- IQR(sativa_trough_outliers$trough)

sativa_trough_outliers <- subset(sativa_trough_outliers, 
                               sativa_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                                 sativa_trough_outliers$trough< (Q3 + 1.5*IQR))
#europaea
euro_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Olea europaea") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(euro_trough_outliers$trough, .25)

Q3 <- quantile(euro_trough_outliers$trough, .75)

IQR <- IQR(euro_trough_outliers$trough)

euro_trough_outliers <- subset(euro_trough_outliers, 
                             euro_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                               euro_trough_outliers$trough< (Q3 + 1.5*IQR))

#eucal
eucal_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Eucalyptus camaldulensis") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(eucal_trough_outliers$trough, .25)

Q3 <- quantile(eucal_trough_outliers$trough, .75)

IQR <- IQR(eucal_trough_outliers$trough)

eucal_trough_outliers <- subset(eucal_trough_outliers, 
                              eucal_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                                eucal_trough_outliers$trough< (Q3 + 1.5*IQR))

#nigra
nigra_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus nigra") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(nigra_trough_outliers$trough, .25)

Q3 <- quantile(nigra_trough_outliers$trough, .75)

IQR <- IQR(nigra_trough_outliers$trough)

nigra_trough_outliers <- subset(nigra_trough_outliers, 
                              nigra_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                                nigra_trough_outliers$trough< (Q3 + 1.5*IQR))

#pinea
pinea_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinea") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(pinea_trough_outliers$trough, .25)

Q3 <- quantile(pinea_trough_outliers$trough, .75)

IQR <- IQR(pinea_trough_outliers$trough)

pinea_trough_outliers <- subset(pinea_trough_outliers, 
                              pinea_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                                pinea_trough_outliers$trough< (Q3 + 1.5*IQR))

#pinaster
pinaster_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinaster") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(pinaster_trough_outliers$trough, .25)

Q3 <- quantile(pinaster_trough_outliers$trough, .75)

IQR <- IQR(pinaster_trough_outliers$trough)

pinaster_trough_outliers <- subset(pinaster_trough_outliers, 
                                 pinaster_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                                   pinaster_trough_outliers$trough< (Q3 + 1.5*IQR))

#sylves
sylves_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus sylvestris") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(sylves_trough_outliers$trough, .25)

Q3 <- quantile(sylves_trough_outliers$trough, .75)

IQR <- IQR(sylves_trough_outliers$trough)

sylves_trough_outliers <- subset(sylves_trough_outliers, 
                               sylves_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                                 sylves_trough_outliers$trough< (Q3 + 1.5*IQR))

#ilex
ilex_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus ilex") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(ilex_trough_outliers$trough, .25)

Q3 <- quantile(ilex_trough_outliers$trough, .75)

IQR <- IQR(ilex_trough_outliers$trough)

ilex_trough_outliers <- subset(ilex_trough_outliers, 
                             ilex_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                               ilex_trough_outliers$trough< (Q3 + 1.5*IQR))

#suber
suber_trough_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus suber") %>% 
  dplyr::select(Sp.x, ID, trough, Arid_Dm_i)

Q1 <- quantile(suber_trough_outliers$trough, .25)

Q3 <- quantile(suber_trough_outliers$trough, .75)

IQR <- IQR(suber_trough_outliers$trough)

suber_trough_outliers <- subset(suber_trough_outliers, 
                              suber_trough_outliers$trough> (Q1 - 1.5*IQR) & 
                                suber_trough_outliers$trough< (Q3 + 1.5*IQR))

#juntamos todos trough valores para poder graficartrough juntos
sp.outlier <- rbind(suber_trough_outliers, ilex_trough_outliers, hale_trough_outliers, 
                    sativa_trough_outliers, eucal_trough_outliers, euro_trough_outliers,
                    sylves_trough_outliers, nigra_trough_outliers, pinea_trough_outliers,
                    pinaster_trough_outliers)


trough_Arid_plot <- ggplot(data=sp.outlier,
                         aes(x=Arid_Dm_i, y=trough))+
  geom_point()+
  ggtitle("Regression between Aridity and TROUGH")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.y = 180, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  labs(x="Aridity", y="Trough (NDVI value)")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 14)) + 
  theme_bar

ggsave(trough_Arid_plot, device = "tiff", path = path2grafic, filename = "trough_doy_outliers_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(trough_Arid_plot, device = "png", path = path2grafic, filename = "trough_doy_outliers_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#### msp ####
#halep
hale_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus halepensis") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(hale_msp_outliers$msp, .25)

Q3 <- quantile(hale_msp_outliers$msp, .75)

IQR <- IQR(hale_msp_outliers$msp)

hale_msp_outliers <- subset(hale_msp_outliers, 
                             hale_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                               hale_msp_outliers$msp< (Q3 + 1.5*IQR))

#sativa
sativa_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Castanea sativa") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(sativa_msp_outliers$msp, .25)

Q3 <- quantile(sativa_msp_outliers$msp, .75)

IQR <- IQR(sativa_msp_outliers$msp)

sativa_msp_outliers <- subset(sativa_msp_outliers, 
                               sativa_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                                 sativa_msp_outliers$msp< (Q3 + 1.5*IQR))
#europaea
euro_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Olea europaea") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(euro_msp_outliers$msp, .25)

Q3 <- quantile(euro_msp_outliers$msp, .75)

IQR <- IQR(euro_msp_outliers$msp)

euro_msp_outliers <- subset(euro_msp_outliers, 
                             euro_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                               euro_msp_outliers$msp< (Q3 + 1.5*IQR))

#eucal
eucal_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Eucalyptus camaldulensis") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(eucal_msp_outliers$msp, .25)

Q3 <- quantile(eucal_msp_outliers$msp, .75)

IQR <- IQR(eucal_msp_outliers$msp)

eucal_msp_outliers <- subset(eucal_msp_outliers, 
                              eucal_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                                eucal_msp_outliers$msp< (Q3 + 1.5*IQR))

#nigra
nigra_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus nigra") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(nigra_msp_outliers$msp, .25)

Q3 <- quantile(nigra_msp_outliers$msp, .75)

IQR <- IQR(nigra_msp_outliers$msp)

nigra_msp_outliers <- subset(nigra_msp_outliers, 
                              nigra_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                                nigra_msp_outliers$msp< (Q3 + 1.5*IQR))

#pinea
pinea_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinea") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(pinea_msp_outliers$msp, .25)

Q3 <- quantile(pinea_msp_outliers$msp, .75)

IQR <- IQR(pinea_msp_outliers$msp)

pinea_msp_outliers <- subset(pinea_msp_outliers, 
                              pinea_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                                pinea_msp_outliers$msp< (Q3 + 1.5*IQR))

#pinaster
pinaster_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinaster") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(pinaster_msp_outliers$msp, .25)

Q3 <- quantile(pinaster_msp_outliers$msp, .75)

IQR <- IQR(pinaster_msp_outliers$msp)

pinaster_msp_outliers <- subset(pinaster_msp_outliers, 
                                 pinaster_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                                   pinaster_msp_outliers$msp< (Q3 + 1.5*IQR))

#sylves
sylves_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus sylvestris") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(sylves_msp_outliers$msp, .25)

Q3 <- quantile(sylves_msp_outliers$msp, .75)

IQR <- IQR(sylves_msp_outliers$msp)

sylves_msp_outliers <- subset(sylves_msp_outliers, 
                               sylves_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                                 sylves_msp_outliers$msp< (Q3 + 1.5*IQR))

#ilex
ilex_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus ilex") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(ilex_msp_outliers$msp, .25)

Q3 <- quantile(ilex_msp_outliers$msp, .75)

IQR <- IQR(ilex_msp_outliers$msp)

ilex_msp_outliers <- subset(ilex_msp_outliers, 
                             ilex_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                               ilex_msp_outliers$msp< (Q3 + 1.5*IQR))

#suber
suber_msp_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus suber") %>% 
  dplyr::select(Sp.x, ID, msp, Arid_Dm_i)

Q1 <- quantile(suber_msp_outliers$msp, .25)

Q3 <- quantile(suber_msp_outliers$msp, .75)

IQR <- IQR(suber_msp_outliers$msp)

suber_msp_outliers <- subset(suber_msp_outliers, 
                              suber_msp_outliers$msp> (Q1 - 1.5*IQR) & 
                                suber_msp_outliers$msp< (Q3 + 1.5*IQR))

#juntamos todos msp valores para poder graficarmsp juntos
sp.outlier <- rbind(suber_msp_outliers, ilex_msp_outliers, hale_msp_outliers, 
                    sativa_msp_outliers, eucal_msp_outliers, euro_msp_outliers,
                    sylves_msp_outliers, nigra_msp_outliers, pinea_msp_outliers,
                    pinaster_msp_outliers)


msp_Arid_plot <- ggplot(data=sp.outlier,
                         aes(x=Arid_Dm_i, y=msp))+
  geom_point()+
  ggtitle("Regression between Aridity and MSP")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 0.7, 
           size = 3.5) +
  geom_smooth(method = "lm")+
  labs(x="Aridity", y="MSP (NDVI value)")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 14)) + 
  theme_bar

ggsave(msp_Arid_plot, device = "tiff", path = path2grafic, filename = "msp_doy_outliers_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(msp_Arid_plot, device = "png", path = path2grafic, filename = "msp_doy_outliers_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#### mau ####
#halep
hale_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus halepensis") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(hale_mau_outliers$mau, .25)

Q3 <- quantile(hale_mau_outliers$mau, .75)

IQR <- IQR(hale_mau_outliers$mau)

hale_mau_outliers <- subset(hale_mau_outliers, 
                            hale_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                              hale_mau_outliers$mau< (Q3 + 1.5*IQR))

#sativa
sativa_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Castanea sativa") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(sativa_mau_outliers$mau, .25)

Q3 <- quantile(sativa_mau_outliers$mau, .75)

IQR <- IQR(sativa_mau_outliers$mau)

sativa_mau_outliers <- subset(sativa_mau_outliers, 
                              sativa_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                                sativa_mau_outliers$mau< (Q3 + 1.5*IQR))
#europaea
euro_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Olea europaea") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(euro_mau_outliers$mau, .25)

Q3 <- quantile(euro_mau_outliers$mau, .75)

IQR <- IQR(euro_mau_outliers$mau)

euro_mau_outliers <- subset(euro_mau_outliers, 
                            euro_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                              euro_mau_outliers$mau< (Q3 + 1.5*IQR))

#eucal
eucal_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Eucalyptus camaldulensis") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(eucal_mau_outliers$mau, .25)

Q3 <- quantile(eucal_mau_outliers$mau, .75)

IQR <- IQR(eucal_mau_outliers$mau)

eucal_mau_outliers <- subset(eucal_mau_outliers, 
                             eucal_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                               eucal_mau_outliers$mau< (Q3 + 1.5*IQR))

#nigra
nigra_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus nigra") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(nigra_mau_outliers$mau, .25)

Q3 <- quantile(nigra_mau_outliers$mau, .75)

IQR <- IQR(nigra_mau_outliers$mau)

nigra_mau_outliers <- subset(nigra_mau_outliers, 
                             nigra_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                               nigra_mau_outliers$mau< (Q3 + 1.5*IQR))

#pinea
pinea_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinea") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(pinea_mau_outliers$mau, .25)

Q3 <- quantile(pinea_mau_outliers$mau, .75)

IQR <- IQR(pinea_mau_outliers$mau)

pinea_mau_outliers <- subset(pinea_mau_outliers, 
                             pinea_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                               pinea_mau_outliers$mau< (Q3 + 1.5*IQR))

#pinaster
pinaster_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus pinaster") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(pinaster_mau_outliers$mau, .25)

Q3 <- quantile(pinaster_mau_outliers$mau, .75)

IQR <- IQR(pinaster_mau_outliers$mau)

pinaster_mau_outliers <- subset(pinaster_mau_outliers, 
                                pinaster_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                                  pinaster_mau_outliers$mau< (Q3 + 1.5*IQR))

#sylves
sylves_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Pinus sylvestris") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(sylves_mau_outliers$mau, .25)

Q3 <- quantile(sylves_mau_outliers$mau, .75)

IQR <- IQR(sylves_mau_outliers$mau)

sylves_mau_outliers <- subset(sylves_mau_outliers, 
                              sylves_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                                sylves_mau_outliers$mau< (Q3 + 1.5*IQR))

#ilex
ilex_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus ilex") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(ilex_mau_outliers$mau, .25)

Q3 <- quantile(ilex_mau_outliers$mau, .75)

IQR <- IQR(ilex_mau_outliers$mau)

ilex_mau_outliers <- subset(ilex_mau_outliers, 
                            ilex_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                              ilex_mau_outliers$mau< (Q3 + 1.5*IQR))

#suber
suber_mau_outliers <- pheno_metr_p_clima %>% 
  filter(Sp.x=="Quercus suber") %>% 
  dplyr::select(Sp.x, ID, mau, Arid_Dm_i)

Q1 <- quantile(suber_mau_outliers$mau, .25)

Q3 <- quantile(suber_mau_outliers$mau, .75)

IQR <- IQR(suber_mau_outliers$mau)

suber_mau_outliers <- subset(suber_mau_outliers, 
                             suber_mau_outliers$mau> (Q1 - 1.5*IQR) & 
                               suber_mau_outliers$mau< (Q3 + 1.5*IQR))

#juntamos todos mau valores para poder graficarmau juntos
sp.outlier <- rbind(suber_mau_outliers, ilex_mau_outliers, hale_mau_outliers, 
                    sativa_mau_outliers, eucal_mau_outliers, euro_mau_outliers,
                    sylves_mau_outliers, nigra_mau_outliers, pinea_mau_outliers,
                    pinaster_mau_outliers)


mau_Arid_plot <- ggplot(data=sp.outlier,
                        aes(x=Arid_Dm_i, y=mau))+
  geom_point()+
  ggtitle("Regression between Aridity and MAU")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 0.7, 
           size = 3.5) +
  labs(x="Aridity", y="MAU (NDVI value)")+
  geom_smooth(method = "lm")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  theme(strip.text.x = element_text(size = 14)) + 
  theme_bar

ggsave(mau_Arid_plot, device = "tiff", path = path2grafic, filename = "mau_doy_outliers_Arid_plot.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(mau_Arid_plot, device = "png", path = path2grafic, filename = "mau_doy_outliers_Arid_plot.png", 
       width = 12, height = 6, units = 'in', dpi = 300)
