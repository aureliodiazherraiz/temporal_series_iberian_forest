#vamos a anlizar la tendencia entre los datos anuales de las métricas fenologicas 
# usaremos pheno_metr del cual hemos de pivotear todos los datos anuales de cada plot
#
annual_metrics <- pheno_metr %>% 
  dplyr::select(-c(max:mean)) %>% 
  pivot_longer(cols=-c(ID, Sp.x, var),
               names_to='Year',
               values_to='Value') %>% 
  na.omit() %>% 
  mutate(Value_grados = ifelse(var %in% c("eos","sos","pot","pop"), Value*(360/365), Value),
         Value_rads = ifelse(var %in% c("eos","sos","pot","pop"), Value_grados*(pi/180), Value)) #creamos dos columnas nuevas que puedan ayudarnos con las 
#futuras metricas circulares

annual_metrics_new <- annual_metrics %>% 
  dplyr::select(-Value_grados, -Value_rads) %>% 
  pivot_wider(names_from = var, values_from = Value)%>% 
  group_by(ID, Year) %>% 
  filter(eos>sos) %>% 
  dplyr::select(Sp.x, sos, eos, los, pop, pot)#ahora tenemos las metricas por año y estudiamos donde el eos>sos

annual_metrics_new %>% 
  group_by(Sp.x, ID) %>% 
  summarise(n=n())

#lo graficamos para todos los puntos
ggplot(annual_metrics %>% 
         filter(var=="eos"&
                  Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                              "Pinus sylvestris", "Pinus nigra", 
                              "Eucalyptus camaldulensis", "Quercus ilex", 
                              "Quercus suber", "Quercus faginea", 
                              "Quercus canariensis","Olea europaea",
                              "Castanea sativa")), aes(x=as.factor(Year), y=Value))+
  geom_point()+ 
  theme_bw()+
  facet_wrap(~Sp.x)#no salen cosas muy logicas, Rafa quiere estudiar la posible relacion entre
#las métricas junto al clima por lo que estaria bien coger estas métricas y mediante PCA
#relacionarlo con la Temp y la precip
#

ggplot(data=annual_metrics %>% 
         filter(var=="eos"&
                  Sp.x %in% c("Quercus faginea")), aes(x=as.numeric(Year), y=Value, colour=as.factor(ID)))+
  geom_point()+ 
  geom_smooth(method = "lm") +
  geom_line()+
  theme_bw()
  

#los valores promediados por especie y año para eso trabajaremos con el dataframe pheno_metr_sp_p donde 
#cada plot para eos, sos, pop y plot tiene valores de las metricas fenologicas en grados por lo que habra que calcular 
#el valor angular medio por año de estas metricas 

#sin embargo hemos de limpiar los datos de posibles outliers para cada variable por lo que
#tendremos que operar mediante condiciones


dt.temp <- pheno_metr_sp_p %>% 
  group_by(Sp.x, year) %>% 
  mutate(q1los=quantile(los, .25),q3los=quantile(los, .75), IQRlos = IQR(los),
         inflos=q1los-1.5*IQRlos, suplos=q3los+1.5*IQR,
         q1peak=quantile(peak, .25),q3peak=quantile(peak, .75), IQRpeak = IQR(peak),
         infpeak=q1peak-1.5*IQRpeak, suppeak=q3peak+1.5*IQR,
         q1trough=quantile(trough, .25),q3trough=quantile(trough, .75), IQRtrough = IQR(trough),
         inftrough=q1trough-1.5*IQRtrough, suptrough=q3trough+1.5*IQR,
         q1msp=quantile(msp, .25),q3msp=quantile(msp, .75), IQRmsp = IQR(msp),
         infmsp=q1msp-1.5*IQRmsp, supmsp=q3msp+1.5*IQR,
         q1mau=quantile(mau, .25),q3mau=quantile(mau, .75), IQRmau = IQR(mau),
         infmau=q1mau-1.5*IQRmau, supmau=q3mau+1.5*IQR, 
         q1sos=quantile(sos_grados, .25),q3sos=quantile(sos_grados, .75), IQRsos = IQR(sos_grados),
         infsos=q1sos-1.5*IQRsos, supsos=q3sos+1.5*IQR,
         q1eos=quantile(eos_grados, .25),q3eos=quantile(eos_grados, .75), IQReos = IQR(eos_grados),
         infeos=q1eos-1.5*IQReos, supeos=q3eos+1.5*IQR,
         q1pot=quantile(pot_grados, .25),q3pot=quantile(pot_grados, .75), IQRpot = IQR(pot_grados),
         infpot=q1pot-1.5*IQRpot, suppot=q3pot+1.5*IQR,
         q1pop=quantile(pop_grados, .25),q3pop=quantile(pop_grados, .75), IQRpop = IQR(pop_grados),
         infpop=q1pop-1.5*IQRpop, suppop=q3pop+1.5*IQR) %>% 
  mutate(sos_grados = ifelse(infsos>sos_grados, NA, ifelse(sos_grados<supsos,sos_grados,NA)),
         eos_grados = ifelse(infeos>eos_grados, NA, ifelse(eos_grados<supeos, eos_grados, NA)),
         pot_grados = ifelse(infpot>pot_grados, NA, ifelse(pot_grados<suppot, pot_grados, NA)),
         pop_grados = ifelse(infpop>pop_grados, NA, ifelse(pop_grados<suppop, pop_grados, NA)),
         los = ifelse(inflos>los, NA, ifelse(los<suplos, los, NA)),
         peak = ifelse(infpeak>peak, NA, ifelse(peak<suppeak, peak, NA)),
         trough = ifelse(inftrough>trough, NA,ifelse(trough<suptrough, trough, NA)),
         msp = ifelse(infmsp>msp, NA, ifelse(msp<supmsp, msp, NA)),
         mau = ifelse(infmau>mau, NA,ifelse(mau<supmau, mau, NA)))


#primero calculamos los valores promediados anuales de cada especie de las metricas relativas al NDVI que pueden trabajarse con
#estadistica lineal
annual_metrics_mean <- dt.temp %>% 
  dplyr::select(Sp.x, year, los, peak, mau, msp, trough) %>% 
  group_by(Sp.x, year) %>% 
  na.omit() %>% 
  summarise(an_mean_los = mean(los), an_mean_peak = mean(peak),
            an_mean_trough = mean(trough),
            an_mean_msp = mean(msp), annual_mean_mau = mean(mau))


#aplicamos mediante un summarise para obtener el resto de las variables

aaa <- dt.temp %>% 
  dplyr::select(Sp.x, year, eos_grados:los_grados) %>%
  group_by(Sp.x, year) %>% 
  na.omit() %>% 
  summarise(sos_mesos_cir = circ.summary(sos_grados, plot = F),
            eos_mesos_cir = circ.summary(eos_grados, plot = F),
            pop_mesos_cir = circ.summary(pop_grados, plot = F),
            pot_mesos_cir = circ.summary(pot_grados, plot = F)) %>% 
  as.data.frame() 

aaa$var <- c("mesos", "inter", "kappa", "MRL", "circvar", "circstd", "loglik")

aaa <- aaa %>% 
  filter(var == "mesos") 

#trabajar con estadistica circular devuleve valores sin formato para pdoer operar con ellos
#por ese motivo hay que transformarlo primero en caracter para despues hacerlo en numerico

aaa$sos_mesos_cir<- round(as.numeric(as.character(aaa$sos_mesos_cir)),2)
aaa$eos_mesos_cir<- round(as.numeric(as.character(aaa$eos_mesos_cir)),2)
aaa$pop_mesos_cir<- round(as.numeric(as.character(aaa$pop_mesos_cir)),2)
aaa$pot_mesos_cir<- round(as.numeric(as.character(aaa$pot_mesos_cir)),2)

aaa <- aaa %>% 
  mutate(sos_mesos_doy = sos_mesos_cir*365/360,
         eos_mesos_doy = eos_mesos_cir*365/360,
         pop_mesos_doy = pop_mesos_cir*365/360,
         pot_mesos_doy = pot_mesos_cir*365/360)
  
#lo juntamos con el resto de las variables obtenidas para cada especie y año

annual_metrics_mean <- merge(annual_metrics_mean, aaa, by=c("Sp.x", "year"))

#ademas lo podemos cruzar con los valores del clima, en este caso de la precip y temp media anual de cada especie

annual_metrics_mean <- merge(annual_metrics_mean, clim94_21_annual_sp %>% 
                               rename(year=Year), by=c("Sp.x","year"))




#### GRAFICAS de metricas fenologicas en el tiempo para cada especie ####
#### Tendencias temporales ####
### EOS 
eos_plot_mean_lines <- ggplot(data=annual_metrics_mean %>% 
                            filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Quercus faginea", 
                                               "Quercus canariensis","Olea europaea",
                                               "Castanea sativa")), aes(x=as.numeric(year)))+
  geom_line(aes(y=eos_mesos_doy), size=2, color = "orange")+ 
  geom_smooth(method = "loess", aes(y=eos_mesos_doy)) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"),y=eos_mesos_doy),
           label.x = 1995, label.y = 150,
           size = 3.5) +
  geom_line(aes(y=annual_pre_fin/10), size=2, color = "blue")+ 
  geom_smooth(method = "loess", aes(y=annual_pre_fin/10)) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"),y=annual_pre_fin),
           label.x = 1995, label.y = 85,
           size = 3.5) +
  scale_y_continuous(name = "EOS (DoY)",
    sec.axis = sec_axis(trans=~.*10, name="Accumulated precipitation (mm)"))+
  labs(x="Year", y="DoY")+
  theme_bar+
  ggtitle("EOS of Mediterranean species in time")+
  facet_wrap(~Sp.x)

#la tendencia temporal
eos_plot_mean <- ggplot(annual_metrics_mean %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Olea europaea",
                                               "Castanea sativa")), aes(x=as.numeric(year), y=eos_mesos_doy))+
  geom_point()+  
  geom_smooth(method = "lm") +
  labs(x="Year", y="DoY")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 240,
           size = 3.5) +
  theme_bar+
  ggtitle("EOS of Mediterranean species in time")+
  facet_wrap(~Sp.x, scales = "free", nrow=2)

eos_plot_mean

ggsave(eos_plot_mean, device = "tiff", path = path2grafic, filename = "eos_out_plot_mean.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(eos_plot_mean, device = "png", path = path2grafic, filename = "eos_out_plot_mean.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#### SOS 
sos_plot_mean <- ggplot(annual_metrics_mean %>% 
         filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                              "Pinus sylvestris", "Pinus nigra", 
                              "Eucalyptus camaldulensis", "Quercus ilex", 
                              "Quercus suber", "Olea europaea",
                              "Castanea sativa")), aes(x=as.numeric(year), y=sos_mesos_doy))+
  geom_point()+  
  geom_smooth(method = "lm") +
  labs(x="Year", y="DoY")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 240,
           size = 3.5) +
  theme_bar+
  ggtitle("SOS of Mediterranean species in time")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)

sos_plot_mean

ggsave(sos_plot_mean, device = "tiff", path = path2grafic, filename = "sos_out_plot_mean.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sos_plot_mean, device = "png", path = path2grafic, filename = "sos_out_plot_mean.png", 
       width = 12, height = 6, units = 'in', dpi = 300)

###LOS
los_plot_mean <- ggplot(annual_metrics_mean %>% 
         filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                              "Pinus sylvestris", "Pinus nigra", 
                              "Eucalyptus camaldulensis", "Quercus ilex", 
                              "Quercus suber", "Olea europaea",
                              "Castanea sativa")), aes(x=as.numeric(year), y=an_mean_los))+
  geom_point()+ 
  labs(x="Year", y="Number of days")+
  theme_bar+
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
          #label.x = 1995, label.y = 150,
           size = 3.5) +
  ggtitle("LOS of Mediterranean species in time")+
  facet_wrap(~Sp.x, scales = "free", nrow = 2)


los_plot_mean

ggsave(los_plot_mean, device = "tiff", path = path2grafic, filename = "los_out_plot_mean.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_plot_mean, device = "png", path = path2grafic, filename = "los_out_plot_mean.png", 
       width = 12, height = 6, units = 'in', dpi = 300)

###POP
pop_plot_mean <- ggplot(annual_metrics_mean %>% 
                          mutate(pop_mesos_doy365 = ifelse(pop_mesos_doy<100, pop_mesos_doy+360, pop_mesos_doy)) %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Olea europaea",
                                               "Castanea sativa")), aes(x=as.numeric(year), y=pop_mesos_doy365))+
  geom_point()+ 
  labs(x="Year", y="DoY")+
  theme_bar+
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  ggtitle("POP of Mediterranean species in time")+
  facet_wrap(~Sp.x, scales ="free", nrow = 2)


pop_plot_mean

ggsave(pop_plot_mean, device = "tiff", path = path2grafic, filename = "pop365_out_plot_mean.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pop_plot_mean, device = "png", path = path2grafic, filename = "pop365_out_plot_mean.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


###POT
pot_plot_mean <- ggplot(annual_metrics_mean %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Olea europaea",
                                               "Castanea sativa")), aes(x=as.numeric(year), y=pot_mesos_doy))+
  geom_point()+ 
  labs(x="Year", y="DoY")+
  theme_bar+
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  ggtitle("pot of Mediterranean species in time")+
  facet_wrap(~Sp.x, scales ="free", nrow = 2)


pot_plot_mean

ggsave(pot_plot_mean, device = "tiff", path = path2grafic, filename = "pot_out_plot_mean.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pot_plot_mean, device = "png", path = path2grafic, filename = "pot_out_plot_mean.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


### metricas absolutas (NDVI) 
###PEAK
peak_plot_mean <- ggplot(annual_metrics_mean %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                               "Pinus sylvestris", "Pinus nigra", 
                                               "Eucalyptus camaldulensis", "Quercus ilex", 
                                               "Quercus suber", "Olea europaea",
                                               "Castanea sativa")), aes(x=as.numeric(year), y=an_mean_peak))+
  geom_point()+ 
  labs(x="Year", y="NDVI")+
  theme_bar+
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  ggtitle("Peak of Mediterranean species in time")+
  facet_wrap(~Sp.x, scales ="free", nrow = 2)


peak_plot_mean

ggsave(peak_plot_mean, device = "tiff", path = path2grafic, filename = "peak_out_plot_mean.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(peak_plot_mean, device = "png", path = path2grafic, filename = "peak_out_plot_mean.png", 
       width = 12, height = 6, units = 'in', dpi = 300)

###TROUGH
trough_plot_mean <- ggplot(annual_metrics_mean %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                "Pinus sylvestris", "Pinus nigra", 
                                                "Eucalyptus camaldulensis", "Quercus ilex", 
                                                "Quercus suber", "Olea europaea",
                                                "Castanea sativa")), aes(x=as.numeric(year), y=an_mean_trough))+
  geom_point()+ 
  labs(x="Year", y="NDVI")+
  theme_bar+
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  ggtitle("Trough of Mediterranean species in time")+
  facet_wrap(~Sp.x, scales ="free", nrow = 2)


trough_plot_mean

ggsave(trough_plot_mean, device = "tiff", path = path2grafic, filename = "trough_out_plot_mean.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(trough_plot_mean, device = "png", path = path2grafic, filename = "trough_out_plot_mean.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


###MSP
msp_plot_mean <- ggplot(annual_metrics_mean %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                "Pinus sylvestris", "Pinus nigra", 
                                                "Eucalyptus camaldulensis", "Quercus ilex", 
                                                "Quercus suber", "Olea europaea",
                                                "Castanea sativa")), aes(x=as.numeric(year), y=an_mean_msp))+
  geom_point()+ 
  labs(x="Year", y="NDVI")+
  theme_bar+
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  ggtitle("MSP of Mediterranean species in time")+
  facet_wrap(~Sp.x, scales ="free", nrow = 2)


msp_plot_mean

ggsave(msp_plot_mean, device = "tiff", path = path2grafic, filename = "msp_out_plot_mean.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(msp_plot_mean, device = "png", path = path2grafic, filename = "msp_out_plot_mean.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


###MAU
mau_plot_mean <- ggplot(annual_metrics_mean %>% 
                           filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                "Pinus sylvestris", "Pinus nigra", 
                                                "Eucalyptus camaldulensis", "Quercus ilex", 
                                                "Quercus suber", "Olea europaea",
                                                "Castanea sativa")), aes(x=as.numeric(year), y=annual_mean_mau))+
  geom_point()+ 
  labs(x="Year", y="NDVI")+
  theme_bar+
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  ggtitle("Mau of Mediterranean species in time")+
  facet_wrap(~Sp.x, scales ="free", nrow = 2)


mau_plot_mean

ggsave(mau_plot_mean, device = "tiff", path = path2grafic, filename = "mau_out_plot_mean.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(mau_plot_mean, device = "png", path = path2grafic, filename = "mau_out_plot_mean.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


######### GRAFICAS de relaciones entre meticas fenologicas con abioticas por año #################
#tambien podemos hacer una correlacion entre las metricas fenologicas y las climáticas

#### precipitation ####

##EOS
preci_eos_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                          filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                             "Pinus sylvestris", "Pinus nigra", 
                                             "Eucalyptus camaldulensis", "Quercus ilex", 
                                             "Quercus suber", "Quercus faginea", 
                                             "Quercus canariensis","Olea europaea",
                                             "Castanea sativa")),
                        aes(x=annual_pre_fin, y=eos_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="(mm)", y="DoY")+
  theme_bar+
  ggtitle("Annual EOS and accumulated precipitation correlation")+
  facet_wrap(~Sp.x, scales = "free")

preci_eos_plot_mean

ggsave(preci_eos_plot_mean, device = "tiff", path = path2grafic, filename = "cor_eos_plot_mean_prec.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(preci_eos_plot_mean, device = "png", path = path2grafic, filename = "cor_eos_plot_mean_prec.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##SOS
preci_sos_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_pre_fin, y=sos_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="(mm)", y="DoY")+
  theme_bar+
  ggtitle("Annual SOS and accumulated precipitation correlation")+
  facet_wrap(~Sp.x, scales = "free")

preci_sos_plot_mean

ggsave(preci_sos_plot_mean, device = "tiff", path = path2grafic, filename = "cor_sos_plot_mean_prec.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(preci_sos_plot_mean, device = "png", path = path2grafic, filename = "cor_sos_plot_mean_prec.png", 
       width = 10, height = 10, units = 'in', dpi = 300)

##LOS
preci_los_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_pre_fin, y=an_mean_los), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="(mm)", y="Number of days")+
  theme_bar+
  ggtitle("Annual LOS and accumulated precipitation correlation")+
  facet_wrap(~Sp.x, scales = "free")

preci_los_plot_mean

ggsave(preci_los_plot_mean, device = "tiff", path = path2grafic, filename = "cor_los_plot_mean_prec.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(preci_los_plot_mean, device = "png", path = path2grafic, filename = "cor_los_plot_mean_prec.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##POP
preci_pop_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_pre_fin, y=pop_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="(mm)", y="DoY")+
  theme_bar+
  ggtitle("Annual POP and accumulated precipitation correlation")+
  facet_wrap(~Sp.x, scales = "free")

preci_pop_plot_mean

ggsave(preci_pop_plot_mean, device = "tiff", path = path2grafic, filename = "cor_pop_plot_mean_prec.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(preci_pop_plot_mean, device = "png", path = path2grafic, filename = "cor_pop_plot_mean_prec.png", 
       width = 10, height = 10, units = 'in', dpi = 300)

##POT
preci_pot_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_pre_fin, y=pot_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="(mm)", y="DoY")+
  theme_bar+
  ggtitle("Annual POT and accumulated precipitation correlation")+
  facet_wrap(~Sp.x, scales = "free")

preci_pot_plot_mean

ggsave(preci_pot_plot_mean, device = "tiff", path = path2grafic, filename = "cor_pot_plot_mean_prec.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(preci_pot_plot_mean, device = "png", path = path2grafic, filename = "cor_pot_plot_mean_prec.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##PEAK
preci_peak_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_pre_fin, y=an_mean_peak), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="(mm)", y="NDVi")+
  theme_bar+
  ggtitle("Annual PEAK and accumulated precipitation correlation")+
  facet_wrap(~Sp.x, scales = "free")

preci_peak_plot_mean

ggsave(preci_peak_plot_mean, device = "tiff", path = path2grafic, filename = "cor_peak_plot_mean_prec.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(preci_peak_plot_mean, device = "png", path = path2grafic, filename = "cor_peak_plot_mean_prec.png", 
       width = 10, height = 10, units = 'in', dpi = 300)



##TROUGH
preci_trough_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                 filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                    "Pinus sylvestris", "Pinus nigra", 
                                                    "Eucalyptus camaldulensis", "Quercus ilex", 
                                                    "Quercus suber", "Quercus faginea", 
                                                    "Quercus canariensis","Olea europaea",
                                                    "Castanea sativa")),
                               aes(x=annual_pre_fin, y=an_mean_trough), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="(mm)", y="NDVI")+
  theme_bar+
  ggtitle("Annual TROUGH and accumulated precipitation correlation")+
  facet_wrap(~Sp.x, scales = "free")

preci_trough_plot_mean

ggsave(preci_trough_plot_mean, device = "tiff", path = path2grafic, filename = "cor_trough_plot_mean_prec.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(preci_trough_plot_mean, device = "png", path = path2grafic, filename = "cor_trough_plot_mean_prec.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##MSP
preci_msp_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                   filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                      "Pinus sylvestris", "Pinus nigra", 
                                                      "Eucalyptus camaldulensis", "Quercus ilex", 
                                                      "Quercus suber", "Quercus faginea", 
                                                      "Quercus canariensis","Olea europaea",
                                                      "Castanea sativa")),
                                 aes(x=annual_pre_fin, y=an_mean_msp), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="(mm)", y="NVDI")+
  theme_bar+
  ggtitle("Annual MSP and accumulated precipitation correlation")+
  facet_wrap(~Sp.x, scales = "free")

preci_msp_plot_mean

ggsave(preci_msp_plot_mean, device = "tiff", path = path2grafic, filename = "cor_msp_plot_mean_prec.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(preci_msp_plot_mean, device = "png", path = path2grafic, filename = "cor_msp_plot_mean_prec.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##MAU
preci_mau_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_pre_fin, y=annual_mean_mau), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="(mm)", y="NDVI")+
  theme_bar+
  ggtitle("Annual MAU and accumulated precipitation correlation")+
  facet_wrap(~Sp.x, scales = "free")

preci_mau_plot_mean

ggsave(preci_mau_plot_mean, device = "tiff", path = path2grafic, filename = "cor_mau_plot_mean_prec.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(preci_mau_plot_mean, device = "png", path = path2grafic, filename = "cor_mau_plot_mean_prec.png", 
       width = 10, height = 10, units = 'in', dpi = 300)



#### Temperature ####
##EOS
temp_eos_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_temp_fin, y=eos_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="DoY")+
  theme_bar+
  ggtitle("Anual EOS and averaged mean temperature correlation")+
  facet_wrap(~Sp.x, scales = "free")

temp_eos_plot_mean

ggsave(temp_eos_plot_mean, device = "tiff", path = path2grafic, filename = "cor_eos_plot_mean_temp.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(temp_eos_plot_mean, device = "png", path = path2grafic, filename = "cor_eos_plot_mean_temp.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##SOS
temp_sos_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_temp_fin, y=sos_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="DoY")+
  theme_bar+
  ggtitle("Anual SOS and averaged mean temperature correlation")+
  facet_wrap(~Sp.x, scales = "free")

temp_sos_plot_mean

ggsave(temp_sos_plot_mean, device = "tiff", path = path2grafic, filename = "cor_sos_plot_mean_temp.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(temp_sos_plot_mean, device = "png", path = path2grafic, filename = "cor_sos_plot_mean_temp.png", 
       width = 10, height = 10, units = 'in', dpi = 300)

##LOS
temp_los_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_temp_fin, y=an_mean_los), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="Number of days")+
  theme_bar+
  ggtitle("Anual LOS and averaged mean temperature correlation")+
  facet_wrap(~Sp.x, scales = "free")

temp_los_plot_mean

ggsave(temp_los_plot_mean, device = "tiff", path = path2grafic, filename = "cor_los_plot_mean_temp.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(temp_los_plot_mean, device = "png", path = path2grafic, filename = "cor_los_plot_mean_temp.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##POP
temp_pop_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_temp_fin, y=pop_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="DoY")+
  theme_bar+
  ggtitle("Anual POP and averaged mean temperature correlation")+
  facet_wrap(~Sp.x, scales = "free")

temp_pop_plot_mean

ggsave(temp_pop_plot_mean, device = "tiff", path = path2grafic, filename = "cor_pop_plot_mean_temp.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(temp_pop_plot_mean, device = "png", path = path2grafic, filename = "cor_pop_plot_mean_temp.png", 
       width = 10, height = 10, units = 'in', dpi = 300)

##POT
temp_pot_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_temp_fin, y=pot_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="DoY")+
  theme_bar+
  ggtitle("Anual POT and averaged mean temperature correlation")+
  facet_wrap(~Sp.x, scales = "free")

temp_pot_plot_mean

ggsave(temp_pot_plot_mean, device = "tiff", path = path2grafic, filename = "cor_pot_plot_mean_temp.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(temp_pot_plot_mean, device = "png", path = path2grafic, filename = "cor_pot_plot_mean_temp.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##PEAK
temp_peak_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                 filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                    "Pinus sylvestris", "Pinus nigra", 
                                                    "Eucalyptus camaldulensis", "Quercus ilex", 
                                                    "Quercus suber", "Quercus faginea", 
                                                    "Quercus canariensis","Olea europaea",
                                                    "Castanea sativa")),
                               aes(x=annual_temp_fin, y=an_mean_peak), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="NDVI")+
  theme_bar+
  ggtitle("Anual PEAK and averaged mean temperature correlation")+
  facet_wrap(~Sp.x, scales = "free")

temp_peak_plot_mean

ggsave(temp_peak_plot_mean, device = "tiff", path = path2grafic, filename = "cor_peak_plot_mean_temp.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(temp_peak_plot_mean, device = "png", path = path2grafic, filename = "cor_peak_plot_mean_temp.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##TROUGH
temp_trough_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                   filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                      "Pinus sylvestris", "Pinus nigra", 
                                                      "Eucalyptus camaldulensis", "Quercus ilex", 
                                                      "Quercus suber", "Quercus faginea", 
                                                      "Quercus canariensis","Olea europaea",
                                                      "Castanea sativa")),
                                 aes(x=annual_temp_fin, y=an_mean_trough), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="NDVI")+
  theme_bar+
  ggtitle("Annual TROUGH and averaged mean temperature correlation")+
  facet_wrap(~Sp.x, scales = "free")

temp_trough_plot_mean

ggsave(temp_trough_plot_mean, device = "tiff", path = path2grafic, filename = "cor_trough_plot_mean_temp.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(temp_trough_plot_mean, device = "png", path = path2grafic, filename = "cor_trough_plot_mean_temp.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##MSP
temp_msp_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_temp_fin, y=an_mean_msp), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="NDVI")+
  theme_bar+
  ggtitle("Anual MSP and averaged mean temperature correlation")+
  facet_wrap(~Sp.x, scales = "free")

temp_msp_plot_mean

ggsave(temp_msp_plot_mean, device = "tiff", path = path2grafic, filename = "cor_msp_plot_mean_temp.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(temp_msp_plot_mean, device = "png", path = path2grafic, filename = "cor_msp_plot_mean_temp.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##MAU
temp_mau_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_temp_fin, y=annual_mean_mau), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="NDVI")+
  theme_bar+
  ggtitle("Anual MAU and averaged mean temperature correlation")+
  facet_wrap(~Sp.x, scales = "free")

temp_mau_plot_mean

ggsave(temp_mau_plot_mean, device = "tiff", path = path2grafic, filename = "cor_mau_plot_mean_temp.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(temp_mau_plot_mean, device = "png", path = path2grafic, filename = "cor_mau_plot_mean_temp.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


#### Aridity simples ####
#### la aridez no ha sido modificada por lo que valores altos son sinonimo de mas humedad
##EOS
Arid_eos_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                               filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                  "Pinus sylvestris", "Pinus nigra", 
                                                  "Eucalyptus camaldulensis", "Quercus ilex", 
                                                  "Quercus suber", "Quercus faginea", 
                                                  "Quercus canariensis","Olea europaea",
                                                  "Castanea sativa")),
                             aes(x=annual_Arid_fin, y=eos_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="Arid", y="DoY")+
  theme_bar+
  ggtitle("Anual EOS and averaged mean Aridity (nc) correlation")+
  facet_wrap(~Sp.x, scales = "free")

Arid_eos_plot_mean

ggsave(Arid_eos_plot_mean, device = "tiff", path = path2grafic, filename = "cor_eos_plot_mean_Arid.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(Arid_eos_plot_mean, device = "png", path = path2grafic, filename = "cor_eos_plot_mean_Arid.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##SOS
Arid_sos_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                               filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                  "Pinus sylvestris", "Pinus nigra", 
                                                  "Eucalyptus camaldulensis", "Quercus ilex", 
                                                  "Quercus suber", "Quercus faginea", 
                                                  "Quercus canariensis","Olea europaea",
                                                  "Castanea sativa")),
                             aes(x=annual_Arid_fin, y=sos_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="DoY")+
  theme_bar+
  ggtitle("Anual SOS and averaged mean Aridity correlation")+
  facet_wrap(~Sp.x, scales = "free")

Arid_sos_plot_mean

ggsave(Arid_sos_plot_mean, device = "tiff", path = path2grafic, filename = "cor_sos_plot_mean_Arid.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(Arid_sos_plot_mean, device = "png", path = path2grafic, filename = "cor_sos_plot_mean_Arid.png", 
       width = 10, height = 10, units = 'in', dpi = 300)

##LOS
Arid_los_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                               filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                  "Pinus sylvestris", "Pinus nigra", 
                                                  "Eucalyptus camaldulensis", "Quercus ilex", 
                                                  "Quercus suber", "Quercus faginea", 
                                                  "Quercus canariensis","Olea europaea",
                                                  "Castanea sativa")),
                             aes(x=annual_Arid_fin, y=an_mean_los), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="Number of days")+
  theme_bar+
  ggtitle("Anual LOS and averaged mean Aridity (nc) correlation")+
  facet_wrap(~Sp.x, scales = "free")

Arid_los_plot_mean

ggsave(Arid_los_plot_mean, device = "tiff", path = path2grafic, filename = "cor_los_plot_mean_Arid.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(Arid_los_plot_mean, device = "png", path = path2grafic, filename = "cor_los_plot_mean_Arid.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##POP
Arid_pop_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                               filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                  "Pinus sylvestris", "Pinus nigra", 
                                                  "Eucalyptus camaldulensis", "Quercus ilex", 
                                                  "Quercus suber", "Quercus faginea", 
                                                  "Quercus canariensis","Olea europaea",
                                                  "Castanea sativa")),
                             aes(x=annual_Arid_fin, y=pop_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="DoY")+
  theme_bar+
  ggtitle("Anual POP and averaged mean Aridity (nc) correlation")+
  facet_wrap(~Sp.x, scales = "free")

Arid_pop_plot_mean

ggsave(Arid_pop_plot_mean, device = "tiff", path = path2grafic, filename = "cor_pop_plot_mean_Arid.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(Arid_pop_plot_mean, device = "png", path = path2grafic, filename = "cor_pop_plot_mean_Arid.png", 
       width = 10, height = 10, units = 'in', dpi = 300)

##POT
Arid_pot_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                               filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                  "Pinus sylvestris", "Pinus nigra", 
                                                  "Eucalyptus camaldulensis", "Quercus ilex", 
                                                  "Quercus suber", "Quercus faginea", 
                                                  "Quercus canariensis","Olea europaea",
                                                  "Castanea sativa")),
                             aes(x=annual_Arid_fin, y=pot_mesos_doy), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="DoY")+
  theme_bar+
  ggtitle("Anual POT and averaged mean Aridity (nc) correlation")+
  facet_wrap(~Sp.x, scales = "free")

Arid_pot_plot_mean

ggsave(Arid_pot_plot_mean, device = "tiff", path = path2grafic, filename = "cor_pot_plot_mean_Arid.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(Arid_pot_plot_mean, device = "png", path = path2grafic, filename = "cor_pot_plot_mean_Arid.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##PEAK
Arid_peak_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                   "Pinus sylvestris", "Pinus nigra", 
                                                   "Eucalyptus camaldulensis", "Quercus ilex", 
                                                   "Quercus suber", "Quercus faginea", 
                                                   "Quercus canariensis","Olea europaea",
                                                   "Castanea sativa")),
                              aes(x=annual_Arid_fin, y=an_mean_peak), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="NDVI")+
  theme_bar+
  ggtitle("Anual PEAK and averaged mean Aridity (nc) correlation")+
  facet_wrap(~Sp.x, scales = "free")

Arid_peak_plot_mean

ggsave(Arid_peak_plot_mean, device = "tiff", path = path2grafic, filename = "cor_peak_plot_mean_Arid.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(Arid_peak_plot_mean, device = "png", path = path2grafic, filename = "cor_peak_plot_mean_Arid.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##TROUGH
Arid_trough_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                                  filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                     "Pinus sylvestris", "Pinus nigra", 
                                                     "Eucalyptus camaldulensis", "Quercus ilex", 
                                                     "Quercus suber", "Quercus faginea", 
                                                     "Quercus canariensis","Olea europaea",
                                                     "Castanea sativa")),
                                aes(x=annual_Arid_fin, y=an_mean_trough), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="NDVI")+
  theme_bar+
  ggtitle("Annual TROUGH and averaged mean Aridity (nc) correlation")+
  facet_wrap(~Sp.x, scales = "free")

Arid_trough_plot_mean

ggsave(Arid_trough_plot_mean, device = "tiff", path = path2grafic, filename = "cor_trough_plot_mean_Arid.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(Arid_trough_plot_mean, device = "png", path = path2grafic, filename = "cor_trough_plot_mean_Arid.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##MSP
Arid_msp_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                               filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                  "Pinus sylvestris", "Pinus nigra", 
                                                  "Eucalyptus camaldulensis", "Quercus ilex", 
                                                  "Quercus suber", "Quercus faginea", 
                                                  "Quercus canariensis","Olea europaea",
                                                  "Castanea sativa")),
                             aes(x=annual_Arid_fin, y=an_mean_msp), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="NDVI")+
  theme_bar+
  ggtitle("Anual MSP and averaged mean Aridity (nc) correlation")+
  facet_wrap(~Sp.x, scales = "free")

Arid_msp_plot_mean

ggsave(Arid_msp_plot_mean, device = "tiff", path = path2grafic, filename = "cor_msp_plot_mean_Arid.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(Arid_msp_plot_mean, device = "png", path = path2grafic, filename = "cor_msp_plot_mean_Arid.png", 
       width = 10, height = 10, units = 'in', dpi = 300)


##MAU
Arid_mau_plot_mean <- ggplot(data=annual_metrics_mean %>% 
                               filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                  "Pinus sylvestris", "Pinus nigra", 
                                                  "Eucalyptus camaldulensis", "Quercus ilex", 
                                                  "Quercus suber", "Quercus faginea", 
                                                  "Quercus canariensis","Olea europaea",
                                                  "Castanea sativa")),
                             aes(x=annual_Arid_fin, y=annual_mean_mau), size=2)+
  geom_point()+ 
  geom_smooth(method = "lm") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 150,
           size = 3.5) +
  labs(x="ºC", y="NDVI")+
  theme_bar+
  ggtitle("Anual MAU and averaged mean Aridity (nc) correlation")+
  facet_wrap(~Sp.x, scales = "free")

Arid_mau_plot_mean

ggsave(Arid_mau_plot_mean, device = "tiff", path = path2grafic, filename = "cor_mau_plot_mean_Arid.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(Arid_mau_plot_mean, device = "png", path = path2grafic, filename = "cor_mau_plot_mean_Arid.png", 
       width = 10, height = 10, units = 'in', dpi = 300)



































