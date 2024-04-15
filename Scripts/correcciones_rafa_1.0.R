#dudas para responder a Rafa partimos de ndvi_sf para verificar si las 
#correlaciones entre Aridez- pre y temp son elevadas


#en este script separaremos los valores de NDVI minimo para categorizar las especies 
#graficamremos en el tiempo a lo largo de tod ala serie temporal para las tres categorias
#posteriormente los correlacionaremos entre ellos parqa ver las diferencias entre las posibles categorias



write.xlsx(pheno_metr_p_clima, paste(path2csv, file = "datos finales.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "resumen", append = FALSE)



#abrimos el archivo con todos los datos del IFN 
Spain <- read_excel("C:/Users/ureli/OneDrive - Universidad de Córdoba/Proyectos/div.func/data/tablas_ilex_pablo/Spain_TotalBiomass.xlsx")


str(Spain)
str(ndvi_sf)

spain_sel <- merge(Spain %>% 
                     select(Provincia, Estadillo, predr, temdr, Sp.x),
                   ndvi_sf, by =c("Provincia", "Estadillo"))

arid_preci <- ggplot(spain_sel, aes(x=preci, y=Arid_Dm_i))+
  geom_point()+
  geom_smooth(method = "loess")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 240,
           size = 3.5)
arid_preci
arid_temp <- ggplot(spain_sel, aes(x=log(tempe), y=Arid_Dm_i))+
  geom_point()+
  geom_smooth(method = "loess")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 240,
           size = 3.5)
arid_temp

write.xlsx(spain_sel %>% 
             select(Provincia:ID, geometry), paste(path2csv, file = "spainsel.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "spain_sel", append = FALSE)


################################################################################
#sobre el calculo del valor minimo del NDVi para intentar resolver el problema del
#matorral


  
spain_sel$min_NDVI <- apply(spain_sel[7:342], 1, min)



#E. Camaldulensis

spain_sel_camaldulensis <- spain_sel %>% 
  filter(Sp.x.x=="Eucalyptus camaldulensis", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_camaldulensis$min_NDVI)# vamos a dividir en tres categorias menor a 0.5, 0.15 a 0.25 y mayor a 0.25


# spain_sel_camaldulensis_mes <- spain_sel_camaldulensis %>% 
#   select_at(vars( min_NDVI =`1994_jan`:`2021_dez`))


spain_sel_camaldulensis <- spain_sel_camaldulensis %>% 
  mutate(Catg= ifelse(min_NDVI<0.15, "Lower", ifelse(min_NDVI>0.25, "Upper", "Middle")))


fig1 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_camaldulensis)+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("E camaldulensis")

#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_camaldulensis <- merge(spain_sel_camaldulensis %>% 
                                   select(ID, Catg), spain_sel %>% 
                                   select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))

#realizamos el valor medio
spain_sel_camaldulensis_mean <- spain_sel_camaldulensis %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_camaldulensis_mean$year <- rep(1994:2021, each=12)
spain_sel_camaldulensis_mean$month <- (01:12)
spain_sel_camaldulensis_mean$date <- paste(spain_sel_camaldulensis_mean$month, spain_sel_camaldulensis_mean$year, sep = "-")

spain_sel_camaldulensis_mean <- spain_sel_camaldulensis_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
camaldulensis_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_camaldulensis_mean)+
  geom_point()+
  geom_line()+
  ggtitle("E. camaldulensis")+
  labs(y="NDVI")+
  theme_bar

#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas

spain_sel_camaldulensis_mean_t <- spain_sel_camaldulensis_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

#lo graficamos
png(file = "spain_sel_camaldulensis_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_camaldulensis_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_camaldulensis_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "E. camaldulensis", cex.main = 1.5, tl.srt=0)
dev.off()



#C. sativa

spain_sel_sativa <- spain_sel %>% 
  filter(Sp.x.x=="Castanea sativa", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_sativa$min_NDVI)# vamos a dividir en tres categorias menor a 0.2, entre 
#0.2 y 0.3 y mayor a 0.3

spain_sel_sativa <- spain_sel_sativa %>% 
  mutate(Catg= ifelse(min_NDVI<0.2, "Lower", ifelse(min_NDVI>0.3, "Upper", "Middle")))

fig2 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_sativa)+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("C sativa")

#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_sativa <- merge(spain_sel_sativa %>% 
                                   select(ID, Catg), spain_sel %>% 
                                   select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))

#realizamos el valor medio
spain_sel_sativa_mean <- spain_sel_sativa %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_sativa_mean$year <- rep(1994:2021, each=12)
spain_sel_sativa_mean$month <- (01:12)
spain_sel_sativa_mean$date <- paste(spain_sel_sativa_mean$month, spain_sel_sativa_mean$year, sep = "-")

spain_sel_sativa_mean <- spain_sel_sativa_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
sativa_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_sativa_mean)+
  geom_point()+
  geom_line()+
  ggtitle("C. sativa")+
  labs(y="NDVI")+
  theme_bar

#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas
spain_sel_sativa_mean_t <- spain_sel_sativa_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

#lo graficamos
png(file = "spain_sel_sativa_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_sativa_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_sativa_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "C. sativa", cex.main = 1.5, tl.srt=0)
dev.off()




#O. europaea

spain_sel_europaea <- spain_sel %>% 
  filter(Sp.x.x=="Olea europaea", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_europaea$min_NDVI)# vamos a dividir en tres categorias menor a 0.2, entre 
#0.2 y 0.3 y mayor a 0.3

spain_sel_europaea <- spain_sel_europaea %>% 
  mutate(Catg= ifelse(min_NDVI<0.2, "Lower", ifelse(min_NDVI>0.3, "Upper", "Middle")))

fig3 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_europaea)+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("O europaea")



#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_europaea <- merge(spain_sel_europaea %>% 
                            select(ID, Catg), spain_sel %>% 
                            select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))

#realizamos el valor medio
spain_sel_europaea_mean <- spain_sel_europaea %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_europaea_mean$year <- rep(1994:2021, each=12)
spain_sel_europaea_mean$month <- (01:12)
spain_sel_europaea_mean$date <- paste(spain_sel_europaea_mean$month, spain_sel_europaea_mean$year, sep = "-")

spain_sel_europaea_mean <- spain_sel_europaea_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
europaea_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_europaea_mean)+
  geom_point()+
  geom_line()+
  ggtitle("O. europaea")+
  labs(y="NDVI")+
  theme_bar


#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas
spain_sel_europaea_mean_t <- spain_sel_europaea_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

#lo graficamos
png(file = "spain_sel_europaea_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_europaea_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_europaea_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "O. europaea", cex.main = 1.5, tl.srt=0)
dev.off()




#P. halepensis

spain_sel_halep <- spain_sel %>% 
  filter(Sp.x.x=="Pinus halepensis", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_halep$min_NDVI)# vamos a dividir en tres categorias menor a 0.2, entre 
#0.2 y 0.3 y mayor a 0.3

spain_sel_halep <- spain_sel_halep %>% 
  mutate(Catg= ifelse(min_NDVI<0.15, "Lower", ifelse(min_NDVI>0.25, "Upper", "Middle")))

fig4 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_halep)+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("P halepensis")



#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_halep <- merge(spain_sel_halep %>% 
                              select(ID, Catg), spain_sel %>% 
                              select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))


#solo por curiosidad vamos a graficar la desnidad de las categorias frente a l valor minimo de NDVI
ggplot(aes(Tree_dens, min_NDVI), data=spain_sel_halep %>% 
         filter(Catg=="MED"))+
  geom_point()+
  geom_smooth(method = "lm")


#realizamos el valor medio
spain_sel_halep_mean <- spain_sel_halep %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_halep_mean$year <- rep(1994:2021, each=12)
spain_sel_halep_mean$month <- (01:12)
spain_sel_halep_mean$date <- paste(spain_sel_halep_mean$month, spain_sel_halep_mean$year, sep = "-")

spain_sel_halep_mean <- spain_sel_halep_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
halep_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_halep_mean)+
  geom_point()+
  geom_line()+
  ggtitle("P. halepensis")+
  labs(y="NDVI")+
  theme_bar

#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas
spain_sel_halep_mean_t <- spain_sel_halep_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

#lo graficamos
png(file = "spain_sel_halep_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_halep_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_halep_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "P. halepensis", cex.main = 1.5, tl.srt=0)
dev.off()



#P. pinaster

spain_sel_pinaster <- spain_sel %>% 
  filter(Sp.x.x=="Pinus pinaster", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_pinaster$min_NDVI)# vamos a dividir en tres categorias menor a 0.2, entre 
#0.2 y 0.3 y mayor a 0.3

spain_sel_pinaster <- spain_sel_pinaster %>% 
  mutate(Catg= ifelse(min_NDVI<0.15, "Lower", ifelse(min_NDVI>0.25, "Upper", "Middle")))

fig5 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_pinaster)+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("P pinaster")




#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_pinaster <- merge(spain_sel_pinaster %>% 
                           select(ID, Catg), spain_sel %>% 
                           select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))

#realizamos el valor medio
spain_sel_pinaster_mean <- spain_sel_pinaster %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_pinaster_mean$year <- rep(1994:2021, each=12)
spain_sel_pinaster_mean$month <- (01:12)
spain_sel_pinaster_mean$date <- paste(spain_sel_pinaster_mean$month, spain_sel_pinaster_mean$year, sep = "-")

spain_sel_pinaster_mean <- spain_sel_pinaster_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
pinaster_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_pinaster_mean)+
  geom_point()+
  geom_line()+
  ggtitle("P. pinaster")+
  labs(y="NDVI")+
  theme_bar

#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas
spain_sel_pinaster_mean_t <- spain_sel_pinaster_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

#lo graficamos
png(file = "spain_sel_pinaster_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_pinaster_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_pinaster_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "P. pinaster", cex.main = 1.5, tl.srt=0)
dev.off()


#P. pinea

spain_sel_pinea <- spain_sel %>% 
  filter(Sp.x.x=="Pinus pinea", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_pinea$min_NDVI)# vamos a dividir en tres categorias menor a 0.2, entre 
#0.2 y 0.3 y mayor a 0.3

spain_sel_pinea <- spain_sel_pinea %>% 
  mutate(Catg= ifelse(min_NDVI<0.15, "Lower", ifelse(min_NDVI>0.25, "Upper", "Middle")))

fig6 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_pinea) +
  geom_point()+  
  geom_smooth(method = "lm")+
  ggtitle("P pinea")


#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_pinea <- merge(spain_sel_pinea %>% 
                              select(ID, Catg), spain_sel %>% 
                              select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))

#realizamos el valor medio
spain_sel_pinea_mean <- spain_sel_pinea %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_pinea_mean$year <- rep(1994:2021, each=12)
spain_sel_pinea_mean$month <- (01:12)
spain_sel_pinea_mean$date <- paste(spain_sel_pinea_mean$month, spain_sel_pinea_mean$year, sep = "-")

spain_sel_pinea_mean <- spain_sel_pinea_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
pinea_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_pinea_mean)+
  geom_point()+
  geom_line()+
  ggtitle("P. pinea")+
  labs(y="NDVI")+
  theme_bar


#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas
spain_sel_pinea_mean_t <- spain_sel_pinea_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

#lo graficamos
png(file = "spain_sel_pinea_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_pinea_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_pinea_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "P. pinea", cex.main = 1.5, tl.srt=0)
dev.off()


#P. nigra

spain_sel_nigra <- spain_sel %>% 
  filter(Sp.x.x=="Pinus nigra", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_nigra$min_NDVI)# vamos a dividir en tres categorias menor a 0.1, entre 
#0.2 y 0.2 y mayor a 0.2

spain_sel_nigra <- spain_sel_nigra %>% 
  mutate(Catg= ifelse(min_NDVI<0.1, "Lower", ifelse(min_NDVI>0.2, "Upper", "Middle")))

fig7 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_nigra)+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("P nigra")



#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_nigra <- merge(spain_sel_nigra %>% 
                              select(ID, Catg), spain_sel %>% 
                              select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))

#realizamos el valor medio
spain_sel_nigra_mean <- spain_sel_nigra %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_nigra_mean$year <- rep(1994:2021, each=12)
spain_sel_nigra_mean$month <- (01:12)
spain_sel_nigra_mean$date <- paste(spain_sel_nigra_mean$month, spain_sel_nigra_mean$year, sep = "-")

spain_sel_nigra_mean <- spain_sel_nigra_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
nigra_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_nigra_mean)+
  geom_point()+
  geom_line()+
  ggtitle("P. nigra")+
  labs(y="NDVI")+
  theme_bar


#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas
spain_sel_nigra_mean_t <- spain_sel_nigra_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

#lo graficamos
png(file = "spain_sel_nigra_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_nigra_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_nigra_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "P. nigra", cex.main = 1.5, tl.srt=0)
dev.off()




#P. sylvestris

spain_sel_sylvestris <- spain_sel %>% 
  filter(Sp.x.x=="Pinus sylvestris", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_sylvestris$min_NDVI)# vamos a dividir en tres categorias menor a 0.1, entre 
#0.1 y 0.2 y mayor a 0.2

spain_sel_sylvestris <- spain_sel_sylvestris %>% 
  mutate(Catg= ifelse(min_NDVI<0.1, "Lower", ifelse(min_NDVI>0.2, "Upper", "Middle")))

fig8 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_sylvestris)+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("P sylvestris")



#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_sylvestris <- merge(spain_sel_sylvestris %>% 
                           select(ID, Catg), spain_sel %>% 
                           select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))

#realizamos el valor medio
spain_sel_sylvestris_mean <- spain_sel_sylvestris %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_sylvestris_mean$year <- rep(1994:2021, each=12)
spain_sel_sylvestris_mean$month <- (01:12)
spain_sel_sylvestris_mean$date <- paste(spain_sel_sylvestris_mean$month, spain_sel_sylvestris_mean$year, sep = "-")

spain_sel_sylvestris_mean <- spain_sel_sylvestris_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
sylvestris_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_sylvestris_mean)+
  geom_point()+
  geom_line()+
  ggtitle("P. sylvestris")+
  labs(y="NDVI")+
  theme_bar


#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas
spain_sel_sylvestris_mean_t <- spain_sel_sylvestris_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

png(file = "spain_sel_sylvestris_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_sylvestris_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_sylvestris_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "P. sylvestris", cex.main = 1.5, tl.srt=0)
dev.off()



#Q. ilex

spain_sel_ilex <- spain_sel %>% 
  filter(Sp.x.x=="Quercus ilex", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_ilex$min_NDVI)# vamos a dividir en tres categorias menor a 0.1, entre 
#0.1 y 0.2 y mayor a 0.2

spain_sel_ilex <- spain_sel_ilex %>% 
  mutate(Catg= ifelse(min_NDVI<0.15, "Lower", ifelse(min_NDVI>0.25, "Upper", "Middle")))

fig9 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_ilex)+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Q ilex")



#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_ilex <- merge(spain_sel_ilex %>% 
                           select(ID, Catg), spain_sel %>% 
                           select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))


#solo por curiosidad vamos a graficar la desnidad de las categorias frente a l valor minimo de NDVI
ggplot(aes(Tree_dens, min_NDVI), data=spain_sel_ilex %>% 
         filter(Catg=="INF"))+
  geom_point()+
  geom_smooth(method = "lm")


#realizamos el valor medio
spain_sel_ilex_mean <- spain_sel_ilex %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_ilex_mean$year <- rep(1994:2021, each=12)
spain_sel_ilex_mean$month <- (01:12)
spain_sel_ilex_mean$date <- paste(spain_sel_ilex_mean$month, spain_sel_ilex_mean$year, sep = "-")

spain_sel_ilex_mean <- spain_sel_ilex_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
ilex_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_ilex_mean)+
  geom_point()+
  geom_line()+
  ggtitle("Q. ilex")+
  labs(y="NDVI")+
  theme_bar

#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas
spain_sel_ilex_mean_t <- spain_sel_ilex_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

#lo graficamos
png(file = "spain_sel_ilex_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_ilex_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_ilex_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "Q. ilex", cex.main = 1.5, tl.srt=0)
dev.off()



#Q. suber

spain_sel_suber <- spain_sel %>% 
  filter(Sp.x.x=="Quercus suber", min_NDVI > 0) %>% 
  select(ID, Sp.x.x, min_NDVI, Tree_dens)#eliminamos los valores negativos

hist(spain_sel_suber$min_NDVI)# vamos a dividir en tres categorias menor a 0.1, entre 
#0.1 y 0.2 y mayor a 0.2

spain_sel_suber <- spain_sel_suber %>% 
  mutate(Catg= ifelse(min_NDVI<0.2, "Lower", ifelse(min_NDVI>0.25, "Upper", "Middle")))

fig10 <- ggplot(aes(Tree_dens, min_NDVI),data=spain_sel_suber)+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Q suber")




#seleccionamos los puntos en función de sus valores categorizados de NDVI min
spain_sel_suber <- merge(spain_sel_suber %>% 
                           select(ID, Catg), spain_sel %>% 
                           select(-c(Sp.x.y, Tree_dens2, Tree_dens3)), by=c("ID"))

#realizamos el valor medio
spain_sel_suber_mean <- spain_sel_suber %>% 
  group_by(Catg) %>% 
  summarise(across(c(`1994_jan`:`2021_dez`), mean)) %>% # creamos el valor medio a lo largo de todos los años
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column() %>% # library(janitor)
  melt(id.vars="rowname") # library(reshape2)

#lo temporalizamos
spain_sel_suber_mean$year <- rep(1994:2021, each=12)
spain_sel_suber_mean$month <- (01:12)
spain_sel_suber_mean$date <- paste(spain_sel_suber_mean$month, spain_sel_suber_mean$year, sep = "-")

spain_sel_suber_mean <- spain_sel_suber_mean %>% 
  mutate(date= as.character(date), date=lubridate::my(date), value=as.numeric(value))

#lo graficamos
suber_ndvi_min <- ggplot(aes(date,value,colour=variable,group=variable), data=spain_sel_suber_mean)+
  geom_point()+
  geom_line()+
  ggtitle("Q. suber")+
  labs(y="NDVI")+
  theme_bar



#Rafa quiere que comparemos las diferentes catgegorias mediante un correlalograma para mostrar las diferencias entre ellas
spain_sel_suber_mean_t <- spain_sel_suber_mean %>% 
  pivot_wider(names_from = variable, values_from = value)

#lo graficamos
png(file = "spain_sel_suber_cor.png", res=150, width = 800, height = 800)
corrplot(cor(spain_sel_suber_mean_t %>% 
               select(Lower:Upper)), p.mat = corr.test(spain_sel_suber_mean_t %>% 
                                                     select(Lower:Upper))$p ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 2.5,
         tl.col = "black", method = "circle", number.cex = 2, tl.cex = 2,  addCoef.col = "white",
         pch.col = "tomato", font.main = 2, family = "serif", mar=c(0,0,2,0), cl.pos = "b", 
         title = "Q. suber", cex.main = 1.5, tl.srt=0)
dev.off()







## Figuras y gráficos

figs_sel <- ggarrange(fig1, fig2, fig3, fig4, fig5,
          fig6, fig7, fig8, fig9, fig10,
          common.legend = T,
          legend = "top",
          align = "hv",
          widths = 1,
          heights = 1,
          ncol = 2, nrow = 5)


ggsave(figs_sel, device = "png", path = path2grafic, filename = "figs_sel_ndvi.png", 
       width = 10, height = 16, units = 'in', dpi = 300)



figs_temp <- ggarrange(camaldulensis_ndvi_min + rremove("x.text") + rremove("x.title"),
                       sativa_ndvi_min + rremove("x.text") + rremove("x.title"), 
                       europaea_ndvi_min + rremove("x.text") + rremove("x.title"),
                       ilex_ndvi_min + rremove("x.text") + rremove("x.title"),
                       suber_ndvi_min,
                      common.legend = T,
                      legend = "top",
                      align = "hv",
                      widths = 1,
                      heights = 1,
                      ncol = 1, nrow = 5)


ggsave(figs_temp, device = "png", path = path2grafic, filename = "figs_temp_ndvi.png", 
       width = 16, height = 16, units = 'in', dpi = 300)
ggsave(figs_temp, device = "tiff", path = path2grafic, filename = "figs_temp_ndvi.tiff", 
       width = 16, height = 16, units = 'in', dpi = 300)


figs_temp1 <- ggarrange(halep_ndvi_min + rremove("x.text") + rremove("x.title"),
                        pinaster_ndvi_min + rremove("x.text") + rremove("x.title"), 
                        pinea_ndvi_min + rremove("x.text") + rremove("x.title"),
                        nigra_ndvi_min + rremove("x.text") + rremove("x.title"), 
                        sylvestris_ndvi_min, 
                        common.legend = T,
                        legend = "top",
                        align = "hv",
                        widths = 1,
                        heights = 1,
                        ncol = 1, nrow = 5)


ggsave(figs_temp1, device = "png", path = path2grafic, filename = "figs_temp_ndvi1.png", 
       width = 16, height = 16, units = 'in', dpi = 300)
ggsave(figs_temp1, device = "tiff", path = path2grafic, filename = "figs_temp_ndvi1.tiff", 
       width = 16, height = 16, units = 'in', dpi = 300)








