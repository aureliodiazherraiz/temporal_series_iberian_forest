#vamos a procesar toda la informacion del NDVI para limpiar el ruido
#para eso tendremos que usar la libreria rbeast



#ahora tendremos que transformar nuestros datos (dataframe) en una lista de puntos agrupados por el ID,
#cada uno con sus características
#creamos la funcion:

split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[, col])

#ahora creamos la lista de puntos apenas 
dflist_allsp <- split_tibble(ndvi_pivot %>% 
                              dplyr::select(ID, NDVI_month), "ID")

#ahora creamos el archivo temporal de cada uno de ellos para obtener df.list.ts[[1]][[14]]
dflist_allsp_ts <- lapply(dflist_allsp, 
                          function(x) lapply(x, 
                                             function(y) ts(y, freq=12, start=c(1994,1))))

#limpiamos

dflist_allsp_ts_descomp <- lapply(dflist_allsp_ts, 
                             function(x) lapply(x[["NDVI_month"]],
                                                function(y) beast(y, freq = 12, print.progress=F)))


#ahora tendremos que extraer las variables que nos interesan y operar con ellas
#para sacar los valores apenas de NDVI
a <- lapply(dflist_allsp_ts_descomp, function(x) rbind(x[[1]][["season"]][["Y"]]))#extraemos 
#los valores de Y que la lista trend que ha creado mediante la funcion beast

b <- lapply(dflist_allsp_ts_descomp, function(x) rbind(x[[1]][["trend"]][["Y"]]))#extraemos 
#los valores de Y que la lista season que ha creado mediante la funcion beast

n <- names(dflist_allsp_ts_descomp)

a <- do.call("rbind", a)#transformamos las listas en matrices juntando las columnas mediante rbind
b <- do.call("rbind", b)#transformamos las listas en matrices juntando las columnas mediante rbind

c <- (a+b) %>% as.data.frame() %>% 
  mutate(ID = n) %>% 
  pivot_longer(cols=c(`V1`:`V336`),
               names_to='N_month',
               values_to='NDVI_clear_month') #sumamos las matrices para obtener los valores de NDVI limpio, trend + season
c <- as.data.frame(c)

c$m <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
c$year <- rep(1994:2021, each = 12)


ndvi_allsp_clean <- c %>% 
  mutate(m_y = paste(m, year, sep = "_")) %>%
  dplyr::select(ID, m_y, NDVI_clear_month) %>% 
  pivot_wider(names_from = m_y, values_from = NDVI_clear_month)


# rare_plot <- plot_ly(sum_allsp_clean_p %>%
#                        filter(Sp.x %in% c("Quercus suber","Quercus canariensis")), type = 'scatter', mode = 'lines')%>%
#     add_trace(x = ~as.Date(year_date), y = ~NDVI_month_mean, name = 'M NDVI ')%>%
#     layout(showlegend = F)
# options(warn = -1)
# 
# rare_plot <- rare_plot %>%
#   layout(xaxis = list(zerolinecolor = '#ffff',
#                zerolinewidth = 2,
#                gridcolor = 'ffff'),
#   yaxis = list(zerolinecolor = '#ffff',
#                zerolinewidth = 2,
#                gridcolor = 'ffff'),
#   plot_bgcolor='#e5ecf6', width = 900)

#debemos eliminar los datos de NDVI para las dos especies Q suber y Q canariensis
#para el mes de marzo de 2018 puesto que hay algun tipo de error bajando estrepitosamente el valor de NDVI para este mes


  
ndvi_rares <- ndvi_allsp_clean %>% 
  dplyr::select(ID:Sp.x, mar_2018) %>% 
  filter(Sp.x %in% c("Quercus suber", "Quercus canariensis")) %>% 
  filter(mar_2018<0.4)
#ahora los eliminaremos del dataframe inicial de NDVI para recomenzar de nuevo todos los analisis


ndvi_allsp_clean <- merge(ndvi %>% 
                            dplyr::select(Provincia:latitud, Sp.x:Arid_Dm_i), ndvi_allsp_clean,  by="ID")## OJO importante, 
#hemos realizado un filtro que altera el numero de observaciones
# para no tener que correr todo el código de nuevo lo filtramos aqui, hemos retirado todas las parcelas donde la abundancia de individuos
# es inferior a 90%


#ahora hemos de calcular las métricas para cada punto para luego sacar valores medios
#ESTO ES IMPORTANTE este es el camino, y no al revés. primero hay que operar por plot para despues
#obtener las métricas agrupadas por especie y no al revés

ndvi_allsp_clean$max_NDVI <- apply(ndvi_allsp_clean[13:348], 1, max)#al usar 1 quiere decir que operamos por filas
ndvi_allsp_clean$min_NDVI <- apply(ndvi_allsp_clean[13:348], 1, min)
ndvi_allsp_clean$mean_NDVI <- apply(ndvi_allsp_clean[13:348], 1, mean)
ndvi_allsp_clean$integral_NDVI <- apply(ndvi_allsp_clean[13:348], 1, sum)
ndvi_allsp_clean$amp <- ndvi_allsp_clean$max_NDVI-ndvi_allsp_clean$min_NDVI

#guardamos el fichero en excell por si algun dia hace falta

write.xlsx(ndvi_allsp_clean, paste(path2csv, file = "ndvi_allsp_clean.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "ndvi_allsp_clean", append = FALSE)

#principales métricas de cada especie
#en resumen tendremos el min, max, media para cada especie del NDVI, precip, temp, aridez y densidad

res_final <- ndvi_allsp_clean %>% na.omit() %>% 
  dplyr::select(Sp.x:Arid_Dm_i, max_NDVI:amp) %>% 
  dplyr::group_by(Sp.x) %>% 
  summarise(Tree_dens2_mean = mean(Tree_dens2), Tree_dens2_max = max(Tree_dens2),
            Tree_dens2_min = min(Tree_dens2),Tree_dens2_sd = sd(Tree_dens2),
            Tree_dens3_mean = mean(Tree_dens3), Tree_dens3_max = max(Tree_dens3),
            Tree_dens3_min = min(Tree_dens3),Tree_dens3_sd = sd(Tree_dens3),
            Precip_mean= mean(preci), Precip_max= max(preci), 
            Precip_min= min(preci), Precip_sd= sd(preci),
            Temp_mean=mean(tempe), Temp_max=max(tempe), 
            Temp_min=min(tempe),Temp_sd=sd(tempe),
            Arid_Dm_i_mean = mean(Arid_Dm_i),Arid_Dm_i_max = max(Arid_Dm_i),
            Arid_Dm_i_min = min(Arid_Dm_i),Arid_Dm_i_sd = sd(Arid_Dm_i),
            NDVI_max = mean(max_NDVI), NDVI_maxsd = sd(max_NDVI), 
            NDVI_mean = mean(mean_NDVI),NDVI_meansd = sd(mean_NDVI),
            NDVI_min = mean(min_NDVI), NDVI_minsd = sd(min_NDVI), 
            NDVI_sum = mean(integral_NDVI),NDVI_sumsd = sd(integral_NDVI),
            NDVI_amp = mean(amp), NDVI_ampsd = sd(amp)) %>% 
  filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                    "Pinus sylvestris", "Pinus nigra", 
                                                    "Eucalyptus camaldulensis", "Quercus ilex", 
                                                    "Quercus suber", "Quercus faginea", 
                                                    "Quercus canariensis","Olea europaea",
                                                    "Castanea sativa"))


write.xlsx(res_final, paste(path2csv, file = "metricas_NDVI_species.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "metrics", append = FALSE)


#### Graficos ####

sum_allsp_clean <- ndvi_allsp_clean %>% 
  group_by(Sp.x) %>% 
  summarise(across(c(Tree_dens:dec_2021),
                   list(mean = ~ mean(.x, na.rm = TRUE))))#esto sirve para sacar 
#la media mensual para despues poder graficar el ciclo temporal de cada especie


#ahora operamos para poer graficar en el tiempo a lo largo de toda la secuencia temporal
sum_allsp_clean_p <- sum_allsp_clean %>% 
  dplyr::select(Sp.x, c(jan_1994_mean:dec_2021_mean)) %>% 
  pivot_longer(cols=c("jan_1994_mean":"dec_2021_mean"),
               names_to='Time',
               values_to='NDVI_month_mean') %>% 
  mutate(Sp.x =as.factor(Sp.x))

sum_allsp_clean_p$year <- paste(15, str_sub(sum_allsp_clean_p$Time,1,3), str_sub(sum_allsp_clean_p$Time,5,8), sep=" ")

sum_allsp_clean_p$month <- str_sub(sum_allsp_clean_p$Time,1,3)

sum_allsp_clean_p <- sum_allsp_clean_p %>% 
  mutate(year_date= lubridate::dmy(year))#le damos formato temporal para poderlo graficar mejor

plot_1 <- ggplot(sum_allsp_clean_p %>%
         filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                            "Pinus sylvestris", "Pinus nigra")), 
       aes(x=as.Date(year_date), y=NDVI_month_mean, shape=Sp.x, group= Sp.x, colour=Sp.x))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 0.7,
           size = 6.5, color="black") +
  geom_line()+
  geom_smooth(method = "loess")+
  labs(x="Years", y="NDVI")+
  facet_wrap(~Sp.x, nrow=1)+
  theme_bar +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") + 
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 18 ),
        axis.title = element_text( size = 18, face = "bold" ),
        legend.position="none",
        # The new stuff
        strip.text = element_text(size = 14))

plot_1

ggsave(plot_1, device = "tiff", path = path2grafic, filename = "plot_1.tiff", 
       width = 15, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(plot_1, device = "png", path = path2grafic, filename = "plot_1.png", 
       width = 15, height = 5, units = 'in', dpi = 300)


plot_2 <- ggplot(sum_allsp_clean_p %>%
                   filter(Sp.x %in% c("Quercus ilex", "Quercus suber", 
                                      "Eucalyptus camaldulensis", "Olea europaea", "Castanea sativa")),  
       aes(x=as.Date(year_date), y=NDVI_month_mean, shape=Sp.x, group= Sp.x, colour=Sp.x))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 0.75,
           size = 6.5) +
  geom_line()+
  geom_smooth(method = "loess")+
  labs(x="Years", y="NDVI")+
  facet_wrap(~Sp.x, nrow=1)+
  theme_bar+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")+ 
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 18 ),
        axis.title = element_text( size = 18, face = "bold" ),
        legend.position="none",
        # The new stuff
        strip.text = element_text(size = 14))
 
plot_2

ggsave(plot_2, device = "tiff", path = path2grafic, filename = "plot_2.tiff", 
       width = 15, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(plot_2, device = "png", path = path2grafic, filename = "plot_2.png", 
       width = 15, height = 5, units = 'in', dpi = 300)




#### Grafica con valores medios anuales de NDVI ####

# recreamos el dataframe con valores anuales
 
sum_allsp_clean_p <- sum_allsp_clean_p %>% as.data.frame()
sum_allsp_clean_p$ye <- rep(1994:2021, each = 12)

ndvi_a_mean <- sum_allsp_clean_p %>% as.data.frame()  
ndvi_a_mean$ye <- rep(1994:2021, each = 12)
ndvi_a_mean <- ndvi_a_mean %>% 
  group_by(Sp.x, ye) %>% 
  summarise(mean_a_NDVI = mean(NDVI_month_mean), ye=mean(ye))
ndvi_a_mean$month <- "jun"


dt <- merge(sum_allsp_clean_p,ndvi_a_mean, by=c("Sp.x","ye","month"), all.x = T)



plot_1_2 <- ggplot(data = dt %>%
                     filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                        "Pinus sylvestris", "Pinus nigra")), 
                   aes(x=as.Date(year_date), y=mean_a_NDVI, colour = Sp.x))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 0.7,
           size = 6.5, color="black") +
  geom_smooth(method = "loess")+
  geom_line(data = dt %>%
              filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                 "Pinus sylvestris", "Pinus nigra")), 
            aes(x=as.Date(year_date), y=NDVI_month_mean, colour = Sp.x))+
  labs(x="Years", y="NDVI")+
  facet_wrap(~Sp.x, nrow=1)+
  theme_bar +
  #scale_x_date(date_breaks = "5 year", date_labels = "%Y") + 
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 18 ),
        axis.title = element_text( size = 18, face = "bold" ),
        legend.position="none",
        # The new stuff
        strip.text = element_text(size = 14))

plot_1_2


ggsave(plot_1_2, device = "tiff", path = path2grafic, filename = "plot_1_2.tiff", 
       width = 15, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(plot_1_2, device = "png", path = path2grafic, filename = "plot_1_2.png", 
       width = 15, height = 5, units = 'in', dpi = 300)



plot_2_2 <- ggplot(data = dt %>%
                     filter(Sp.x %in% c("Quercus ilex", "Quercus suber", 
                                        "Eucalyptus camaldulensis", "Olea europaea", "Castanea sativa")), 
                   aes(x=as.Date(year_date), y=mean_a_NDVI, colour = Sp.x))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 0.8,
           size = 6.5, color="black") +
  geom_line(data = dt %>%
              filter(Sp.x %in% c("Quercus ilex", "Quercus suber", 
                                 "Eucalyptus camaldulensis", "Olea europaea", "Castanea sativa")), 
            aes(x=as.Date(year_date), y=NDVI_month_mean, colour = Sp.x))+
  geom_smooth(method = "loess")+
  labs(x="Years", y="NDVI")+
  facet_wrap(~Sp.x, nrow=1)+
  theme_bar +
  #scale_x_date(date_breaks = "5 year", date_labels = "%Y") + 
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 18 ),
        axis.title = element_text( size = 18, face = "bold" ),
        legend.position="none",
        # The new stuff
        strip.text = element_text(size = 14))

plot_2_2


ggsave(plot_2_2, device = "tiff", path = path2grafic, filename = "plot_2_2.tiff", 
       width = 15, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(plot_2_2, device = "png", path = path2grafic, filename = "plot_2_2.png", 
       width = 15, height = 5, units = 'in', dpi = 300)




plotx <- ggplot(data = dt %>%
                     filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                        "Pinus sylvestris", "Pinus nigra","Quercus ilex", "Quercus suber", 
                                        "Eucalyptus camaldulensis", "Olea europaea", "Castanea sativa")), 
                   aes(x=as.Date(year_date), y=mean_a_NDVI, colour = Sp.x))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = 0.76,
           size = 8.5, color="black") +
  geom_line(data = dt %>%
              filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                 "Pinus sylvestris", "Pinus nigra","Quercus ilex", "Quercus suber", 
                                 "Eucalyptus camaldulensis", "Olea europaea", "Castanea sativa")), 
            aes(x=as.Date(year_date), y=NDVI_month_mean, colour = Sp.x))+
  geom_smooth(method = "loess")+
  labs(x="Years", y="NDVI")+
  facet_wrap(~Sp.x, nrow=2, ncol = 5)+
  scale_y_continuous(limits = c(0.3, 0.8))+
  theme_bar +
  #scale_x_date(date_breaks = "5 year", date_labels = "%Y") + 
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 18 ),
        axis.title = element_text( size = 26, face = "bold"),
        #legend.position="none",
        # The new stuff
        strip.text = element_text(size = 18, face = "bold")) +
  scale_colour_manual(limits = c("Castanea sativa","Eucalyptus camaldulensis", "Olea europaea",
                                 "Pinus halepensis","Pinus nigra","Pinus pinaster",
                                 "Pinus pinea","Pinus sylvestris","Quercus ilex","Quercus suber"),
                      values = colores)

plotx


ggsave(plotx, device = "tiff", path = path2grafic, filename = "plotx.tiff", 
       width = 20, height = 10, units = 'in', dpi = 300, compression = 'lzw')
ggsave(plotx, device = "png", path = path2grafic, filename = "plotx.png", 
       width = 20, height = 10, units = 'in', dpi = 300)



#otra alternativa es sacar los valores promediados de todos los puntos por mes
#asi tendriamos el comportamiento medio de cada especie a lo largo del año

sum_allsp_clean_mean <- sum_allsp_clean_p %>% 
  group_by(Sp.x, month) %>% 
  summarise(mean = mean(NDVI_month_mean)) %>% 
  mutate(cicle=as.numeric(recode(month, "sep"="01","oct"="02","nov"="03",
                                 "dec"="04","jan"="05",
                                 "feb"="06","mar"="07","apr"="08","may"="09",
                                 "jun"="10", "jul"="11","aug"="12")))


plot_ciclo_gimnos <- ggplot(sum_allsp_clean_mean %>% 
                       filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                          "Pinus sylvestris", "Pinus nigra")) %>% 
                         rename(Species=Sp.x), 
                     aes(x=cicle, y=mean, colour=Species)) +
  geom_line(size=1)+geom_point(size=2)+ 
  scale_y_continuous(limits = c(0.35,0.8))+
  ylab("NDVI average")+
  xlab("Months")+
  xlim(c("Sep", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
                                   "Jun", "Jul", "Aug")) +
  theme_bar+ 
  theme(axis.text = element_text( size = 12),
        axis.text.x = element_text( size = 16 ),
        axis.title = element_text( size = 12, face = "bold"),
        #legend.position="none",
        # The new stuff
        strip.text = element_text(size = 12)) +
  scale_colour_manual(limits = c("Castanea sativa","Eucalyptus camaldulensis", "Olea europaea",
                                 "Pinus halepensis","Pinus nigra","Pinus pinaster",
                                 "Pinus pinea","Pinus sylvestris","Quercus ilex","Quercus suber"),values = colores)

plot_ciclo_gimnos 

plot_ciclo_angi <- ggplot(sum_allsp_clean_mean %>% 
                              filter(Sp.x %in% c("Quercus ilex", "Quercus suber", 
                                                 "Eucalyptus camaldulensis",
                                                 "Olea europaea", "Castanea sativa"))%>% 
                            rename(Species=Sp.x),  
                            aes(x=cicle, y=mean, colour=Species)) +
  geom_line(size=1)+geom_point(size=2)+ xlim(c("Sep", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
                                   "Jun", "Jul", "Aug")) +
  scale_y_continuous(limits = c(0.35,0.8))+
  ylab(element_blank())+
  xlab("Months")+
  theme_bar+ 
  theme(axis.text = element_text( size = 12 ),
        axis.text.x = element_text( size = 16 ),
        axis.title = element_text( size = 12, face = "bold"),
        #legend.position="none",
        # The new stuff
        strip.text = element_text(size = 12)) +
  scale_colour_manual(limits = c("Castanea sativa","Eucalyptus camaldulensis", "Olea europaea",
                                 "Pinus halepensis","Pinus nigra","Pinus pinaster",
                                 "Pinus pinea","Pinus sylvestris","Quercus ilex","Quercus suber"),values = colores)

plot_ciclo_angi 


plot_ciclo <- ggarrange(plot_ciclo_gimnos, plot_ciclo_angi + rremove("y.text"),
                          labels = c("A)", "B)"),
                          common.legend = T,
                          legend = "top",
                          align = "hv",
                          widths = 1,
                          heights = 1,
                          ncol = 2, nrow = 1)
plot_ciclo

ggsave(plot_ciclo, device = "tiff", path = path2grafic, filename = "plot_ciclo.tiff", 
       width = 20, height = 10, units = 'in', dpi = 300, compression = 'lzw')

ggsave(plot_ciclo, device = "png", path = path2grafic, filename = "plot_ciclo.png", 
       width = 20, height = 10, units = 'in', dpi = 300)




### Rafa solo quiere las especies de pinos

plot_ciclo_pinus <- ggplot(sum_allsp_clean_mean %>% 
                              filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                                                 "Pinus sylvestris", "Pinus nigra")) %>% 
                              rename(Species=Sp.x), 
                            aes(x=cicle, y=mean, colour=Species)) +
  geom_line(size=1)+geom_point(size=2)+ 
  scale_y_continuous(limits = c(0.35,0.8))+
  ylab("NDVI average")+
  xlab("Months")+
  xlim(c("Sep", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  theme_bar+ 
  theme(axis.text = element_text( size = 26),
        axis.text.x = element_text( size = 26 ),
        axis.title = element_text( size = 26, face = "bold"),
        #legend.position="none",
        # The new stuff
        strip.text = element_text(size = 12)) +
  scale_colour_manual(limits = c("Pinus halepensis","Pinus nigra","Pinus pinaster",
                                 "Pinus pinea","Pinus sylvestris"),values = colores)

plot_ciclo_pinus 


ggsave(plot_ciclo_pinus, device = "png", path = path2grafic, filename = "plot_ciclo_pinus.png", 
       width = 16, height = 10, units = 'in', dpi = 300)

### Rafa solo quiere todas las especies peremnes

plot_ciclo_peremnes <- ggplot(sum_allsp_clean_mean %>% 
                             filter(Sp.x %in% c("Eucalyptus camaldulensis", "Olea europaea",
                                                "Pinus halepensis","Pinus nigra","Pinus pinaster",
                                                "Pinus pinea","Pinus sylvestris","Quercus ilex","Quercus suber")) %>% 
                             rename(Species=Sp.x), 
                           aes(x=cicle, y=mean, colour=Species, shape=Species)) +
  geom_line(size=1)+geom_point(size=2)+ 
  scale_y_continuous(limits = c(0.35,0.8))+
  ylab("NDVI average")+
  xlab("Months")+
  xlim(c("Sep", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  theme_bar+ 
  theme(axis.text = element_text( size = 26),
        axis.text.x = element_text( size = 26 ),
        axis.title = element_text( size = 26, face = "bold"),
        #legend.position="none",
        # The new stuff
        strip.text = element_text(size = 12)) +
  scale_colour_manual(limits = c("Eucalyptus camaldulensis", "Olea europaea",
                                 "Pinus halepensis","Pinus nigra","Pinus pinaster",
                                 "Pinus pinea","Pinus sylvestris","Quercus ilex","Quercus suber"),values = colores)

plot_ciclo_peremnes 


ggsave(plot_ciclo_peremnes, device = "png", path = path2grafic, filename = "plot_ciclo_peremnes.png", 
       width = 18, height = 10, units = 'in', dpi = 300)






