#vamos a obtener todos los datos mensuales de precipitacion y temperatura por plot 
#a lo largo del periodo de 1994 2021
getwd()
#setwd("C:/Aurelio_documentos/digital_agri")

path2csv <- "./CSV/"
path2grafic <- "./Graficos/"
path2model <- "./Modelos/"


# Download the climate dataset
download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/tmp/cru_ts4.05.2001.2010.tmp.dat.nc.gz",
              destfile = "Climate/cru_ts4.05.2001.2010.tmp.dat.nc.gz")
download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/tmp/cru_ts4.05.1991.2000.tmp.dat.nc.gz",
              destfile = "Climate/cru_ts4.05.1991.2000.tmp.dat.nc.gz")
download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/tmp/cru_ts4.05.2011.2020.tmp.dat.nc.gz",
              destfile = "Climate/cru_ts4.05.2011.2020.tmp.dat.nc.gz")


download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.06/cruts.2205201912.v4.06/tmp/cru_ts4.06.2021.2021.tmp.dat.nc.gz",
              destfile = "Climate/cru_ts4.06.2021.2021.tmp.dat.nc.gz")#este archivo es anual por lo que con el paso del tiempo
#posiblemente será actualizado con años sucesivos por lo que posiblemente fallará


download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/cru_ts4.05.2001.2010.pre.dat.nc.gz",
              destfile = "Climate/cru_ts4.05.2001.2010.pre.dat.nc.gz")
download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/cru_ts4.05.1991.2000.pre.dat.nc.gz",
              destfile = "Climate/cru_ts4.05.1991.2000.pre.dat.nc.gz")
download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/cru_ts4.05.2011.2020.pre.dat.nc.gz",
              destfile = "Climate/cru_ts4.05.2011.2020.pre.dat.nc.gz")
download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.06/cruts.2205201912.v4.06/pre/cru_ts4.06.2021.2021.pre.dat.nc.gz",
              destfile = "Climate/cru_ts4.06.2021.2021.pre.dat.nc.gz")#este archivo es anual por lo que con el paso del tiempo
#posiblemente será actualizado con años sucesivos por lo que posiblemente fallará


download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pet/cru_ts4.05.2001.2010.pet.dat.nc.gz",
              destfile = "Climate/cru_ts4.05.2001.2010.pet.dat.nc.gz")
download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pet/cru_ts4.05.1991.2000.pet.dat.nc.gz",
              destfile = "Climate/cru_ts4.05.1991.2000.pet.dat.nc.gz")
download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pet/cru_ts4.05.2011.2020.pet.dat.nc.gz",
              destfile = "Climate/cru_ts4.05.2011.2020.pet.dat.nc.gz")



# Unzipping the dataset
gunzip("Climate/cru_ts4.05.2001.2010.tmp.dat.nc.gz", 
       remove = FALSE, overwrite = TRUE)
gunzip("Climate/cru_ts4.05.1991.2000.tmp.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)
gunzip("Climate/cru_ts4.05.2011.2020.tmp.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)
gunzip("Climate/cru_ts4.06.2021.2021.tmp.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)


gunzip("Climate/cru_ts4.05.2001.2010.pre.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)
gunzip("Climate/cru_ts4.05.1991.2000.pre.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)
gunzip("Climate/cru_ts4.05.2011.2020.pre.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)
gunzip("Climate/cru_ts4.06.2021.2021.pre.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)


gunzip("Climate/cru_ts4.05.2001.2010.pet.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)
gunzip("Climate/cru_ts4.05.1991.2000.pet.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)
gunzip("Climate/cru_ts4.05.2011.2020.pet.dat.nc.gz",
       remove = FALSE, overwrite = TRUE)


#### TEMPERATURA ####

#######################################
#el periodo del 1994 hasta el 2020 ####

########################
#para los años 1994-2000
file <- ("Climate/cru_ts4.05.1991.2000.tmp.dat.nc")
years <- 1994:2000 #Requested year
var <- "tmp" #Variable code for precipitation
#lons <- -7.420441 #Longitude of Cilacap town
#lats <- 37.376229 #Latitude of Cilacap town
#datres_96_00 <- extractcru(file, lons, lats, var, year = years)  %>% print()

my_list_new <- list()               # Create empty list
for(i in 1:nrow(ndvi_coord)) {
  mymat <- extractcru(file, lon = ndvi_coord[i,"longitud"], lat = ndvi_coord[i,"latitud"], var, year = years)
  my_list_new[[length(my_list_new) + 1]] <- mymat
}#obtengo una lista de de dataframes

tmp94_00<- bind_rows(my_list_new, .id = "column_label") #obtengo una lista de de dataframes



########################
#para los años 2001-2010
file <- ("Climate/cru_ts4.05.2001.2010.tmp.dat.nc")
years <- 2001:2010 #Requested year
var <- "tmp" #Variable
#lons <- -7.420441 #Longitude
#lats <- 37.376229 #Latitude
#datres_01_07 <- extractcru(file, lons, lats, var, year = years) %>% print()

#para extraer de una misma vez con un bucle para ir sacando todos los puntos de una vez. Cuidado el archivo inicial no fue reconocido por ser una tible, hubo que transformarla en dataframe

my_list_new <- list()               # Create empty list
for(i in 1:nrow(ndvi_coord)) {
  mymat <- extractcru(file, lon = ndvi_coord[i,"longitud"], lat = ndvi_coord[i,"latitud"], var, year = years)
  my_list_new[[length(my_list_new) + 1]] <- mymat
}#obtengo una lista de de dataframes

tmp01_10<- bind_rows(my_list_new, .id = "column_label")



#########################
# para los años 2010-2020
file <- ("Climate/cru_ts4.05.2011.2020.tmp.dat.nc")
years <- 2011:2020 #Requested year
var <- "tmp" #Variable

my_list_new <- list()               # Create empty list
for(i in 1:nrow(ndvi_coord)) {
  mymat <- extractcru(file, lon = ndvi_coord[i,"longitud"], lat = ndvi_coord[i,"latitud"], var, year = years)
  my_list_new[[length(my_list_new) + 1]] <- mymat
}#obtengo una lista de de dataframes

tmp11_20<- bind_rows(my_list_new, .id = "column_label") #obtengo una lista de de dataframes



# para el año 2021
file <- ("Climate/cru_ts4.06.2021.2021.tmp.dat.nc")
years <- 2021 #Requested year
var <- "tmp" #Variable

my_list_new <- list()               # Create empty list
for(i in 1:nrow(ndvi_coord)) {
  mymat <- extractcru(file, lon = ndvi_coord[i,"longitud"], lat = ndvi_coord[i,"latitud"], var, year = years)
  my_list_new[[length(my_list_new) + 1]] <- mymat
}#obtengo una lista de de dataframes

tmp21<- bind_rows(my_list_new, .id = "column_label") #obtengo una lista de de dataframes

tmp94_21 <- rbind(tmp94_00, tmp01_10, tmp11_20, tmp21) #unimos todos dataframes


#### PRECIPITACION ####

##################
### Años 1994-2000

file <- ("Climate/cru_ts4.05.1991.2000.pre.dat.nc")
years <- 1994:2000 #Requested year
var <- "pre" #Variable
#lons <- -7.420441 #Longitude
#lats <- 37.376229 #Latitude
#datres_01_07 <- extractcru(file, lons, lats, var, year = years) %>% print()

#para extraer de una misma vez con un bucle para ir sacando todos los puntos de una vez. Cuidado el archivo inicial no fue reconocido por ser una tible, hubo que transformarla en dataframe

my_list_new <- list()               # Create empty list
for(i in 1:nrow(ndvi_coord)) {
  mymat <- extractcru(file, lon = ndvi_coord[i,"longitud"], lat = ndvi_coord[i,"latitud"], var, year = years)
  my_list_new[[length(my_list_new) + 1]] <- mymat
}#obtengo una lista de de dataframes

pre94_00<- bind_rows(my_list_new, .id = "column_label") #obtengo una lista de de dataframes


##################
### Años 2001-2010

file <- ("Climate/cru_ts4.05.2001.2010.pre.dat.nc")
years <- 2001:2010 #Requested year
var <- "pre" #Variable code for precipitation
#lons <- -7.420441 #Longitude of Cilacap town
#lats <- 37.376229 #Latitude of Cilacap town
#datres_96_00 <- extractcru(file, lons, lats, var, year = years)  %>% print()

my_list_new <- list()               # Create empty list
for(i in 1:nrow(ndvi_coord)) {
  mymat <- extractcru(file, lon = ndvi_coord[i,"longitud"], lat = ndvi_coord[i,"latitud"], var, year = years)
  my_list_new[[length(my_list_new) + 1]] <- mymat
}#obtengo una lista de de dataframes

pre01_10<- bind_rows(my_list_new, .id = "column_label") #obtengo una lista de de dataframes


#########################
# para los años 2010-2020
file <- ("Climate/cru_ts4.05.2011.2020.pre.dat.nc")
years <- 2011:2020 #Requested year
var <- "pre" #Variable

my_list_new <- list()               # Create empty list
for(i in 1:nrow(ndvi_coord)) {
  mymat <- extractcru(file, lon = ndvi_coord[i,"longitud"], lat = ndvi_coord[i,"latitud"], var, year = years)
  my_list_new[[length(my_list_new) + 1]] <- mymat
}#obtengo una lista de de dataframes

pre11_20<- bind_rows(my_list_new, .id = "column_label") #obtengo una lista de de dataframes


# para el año 2021
file <- ("Climate/cru_ts4.06.2021.2021.pre.dat.nc")
years <- 2021:2021 #Requested year
var <- "pre" #Variable

my_list_new <- list()               # Create empty list
for(i in 1:nrow(ndvi_coord)) {
  mymat <- extractcru(file, lon = ndvi_coord[i,"longitud"], lat = ndvi_coord[i,"latitud"], var, year = years)
  my_list_new[[length(my_list_new) + 1]] <- mymat
}#obtengo una lista de de dataframes


pre21<- bind_rows(my_list_new, .id = "column_label") #obtengo una lista de de dataframes


pre94_21 <- rbind(pre94_00, pre01_10, pre11_20, pre21) #unimos los todos dataframes
pre94_21$nm <- 1:12


clim94_21 <- merge(tmp94_21, pre94_21 %>% 
                     dplyr::select(-c(lat, lon)), by= c("column_label", "Year","Month"))

clim94_21<-clim94_21 %>%
  mutate(column_label=as.numeric(column_label)) %>%
  arrange(column_label,Year, nm)

clim94_21$station <- c("WI","WI","SP","SP","SP","SU","SU","SU","AU","AU","AU","WI")#creamos una columna con las estaciones del año

#introducimos la especie de cada plot
clim94_21 <- merge(clim94_21, ndvi %>% 
                     dplyr::select(Provincia:latitud, Sp.x) %>% 
                     rename(lon = longitud, lat =latitud), by=c("lon", "lat")) 
 

clim94_21_annual <- clim94_21 %>% #sirve para despues graficar las dinamicas del clima en el tiempo
  group_by(ID, Year) %>% 
  summarise(annual_temp = mean(tmp),
            annual_pre = sum(pre), 
            lon=mean(lon), lat=mean(lat)) %>% 
  mutate(Arid = annual_pre/(10+annual_temp)) %>% 
  group_by(Year) %>% 
  summarise(annual_temp_fin = mean(annual_temp), annual_temp_sd = sd(annual_temp),
            annual_pre_fin = mean(annual_pre), annual_pre_sd = sd(annual_pre),
            annual_Arid_fin = mean(Arid), annual_Arid_sd = sd(Arid))
  

#### Para estudiar la variabilidad en el espacio ####
#para eso nos hacen falta valores promediados por plot a lo largo del tiempo, o sea un valor por plot


clim94_21_annual_plot <- clim94_21 %>% #nos hace falta para posteriormente graficar el efecto de cada especie en el tiempo
  group_by(Sp.x, ID, Year) %>% #primero promedio para cada especie en cada mes de cada año
  summarise(plot_temp = mean(tmp),
            plot_pre = sum(pre)) %>% 
  mutate(Arid = plot_pre/(10+plot_temp)) %>% 
  group_by(Sp.x,ID) %>% 
  summarise(annual_plot_temp = mean(plot_temp),
            annual_plot_pre = mean(plot_pre),
            annual_plot_Arid = mean(Arid))
  

#### Para estudiar la variabilidad en el tiempo ####
### para eso hemos de obtener las metricas de cada especie primero por mes y despues por año
### ademas como queremos hacer calculos con otras variables climaticas como las estaciones tb lo podemos
### subdividir

clim94_21_annual_sp <- clim94_21 %>% #nos hace falta para posteriormente graficar el efecto de cada especie en el tiempo
  group_by(Sp.x, Year, Month) %>% #primero promedio para cada especie en cada mes de cada año
  summarise(annual_temp = mean(tmp),
            annual_pre = mean(pre)) %>% 
  group_by(Sp.x, Year) %>% #despues promedio para cada especie y año
  summarise(annual_temp_fin = mean(annual_temp),annual_pre_fin = sum(annual_pre)) %>% 
  mutate(Arid = (annual_pre_fin/(10+annual_temp_fin)))
  
#para las variables estacionales
clim94_21_annual_sp_st <- clim94_21 %>% 
  group_by(Sp.x, Year, ID, station) %>%
  summarise(au_pre=sum(pre),au_temp=mean(tmp),
            wi_pre=sum(pre),wi_temp=mean(tmp),
            sp_pre=sum(pre),sp_temp=mean(tmp),
            su_pre=sum(pre),su_temp=mean(tmp)) %>% 
  group_by(Sp.x, Year) %>% #despues promedio para cada especie y año
  summarise(wi_temp_fin = mean(wi_temp),wi_pre_fin = mean(wi_pre),
            sp_temp_fin = mean(sp_temp),sp_pre_fin = mean(sp_pre),
            su_temp_fin = mean(su_temp),su_pre_fin = mean(su_pre),
            au_temp_fin = mean(au_temp),au_pre_fin = mean(au_pre))

#los unimos
clim94_21_annual_sp <- merge(clim94_21_annual_sp, clim94_21_annual_sp_st, by=c("Sp.x","Year"))

#guardamos los datos de ilex
write.xlsx(clim94_21, paste(path2csv,file = "clim94_20.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "clim94_20", append = FALSE)
# write.csv(clim94_20, "CSV/clim94_20.csv", row.names=FALSE)


##Rafa quiere estudiar la tem y precipi para clasificar como seco, humedo o medio, principalmente en funcion 
##de la precipitacion 
temp_mean_94_21 <-ggplot(clim94_21_annual, aes(Year, annual_temp_fin))+
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=annual_temp_fin-annual_temp_sd, ymax=annual_temp_fin+annual_temp_sd), width=.2,
                position=position_dodge(.9))+
  theme_classic()+
  ggtitle("Mean annual temperature between 1994 and 2020")+
  labs(y="Temperature")+
  scale_fill_brewer(palette="Paired")
temp_mean_94_21


#para ver como se comporta la temperatura total de la region en el tiempo, ver la tendencia
temp_mean_94_21_regr <-ggplot(clim94_21_annual, aes(Year, annual_temp_fin))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 240,
           size = 3.5) +
  theme_classic()+
  ggtitle("Mean annual temperature between 1994 and 2020")+
  labs(y="Temperature")+
  scale_fill_brewer(palette="Paired")
temp_mean_94_21_regr



pre_mean_94_21 <-ggplot(clim94_21_annual, aes(Year, annual_pre_fin))+
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=annual_pre_fin-annual_pre_sd, ymax=annual_pre_fin+annual_pre_sd), width=.2,
                position=position_dodge(.9))+
  theme_classic()+
  ggtitle("Mean annual accumulated precipitation between 1994 and 2020")+
  labs(y="Precipitation (mm)")+
  scale_fill_brewer(palette="Paired")
pre_mean_94_21

#siendo asi decidimos establecer tres categorias climáticas en funcion de la precipitacion
#mas tarde los utilizaremos para ver si existen diferencias entre ellos
#humedos (h) años 1996, 1997, 2010
#secos (s) 1994, 2005, 2015
#medios (m) 2003, 2006, 2008

#temperatura
tmp_mean_94_21_regr <-ggplot(clim94_21_annual_sp %>%
                               mutate(Sp.x=recode(Sp.x, 'Eucalyptus camaldulensis'='E. camaldulsensis', 'Olea europaea'='O. europaea',
                                                  "Pinus halepensis" = "P. halepensis", "Pinus pinaster" = "P. pinaster",
                                                  "Pinus pinea" = "P. pinea", "Pinus nigra" = "P. nigra", 
                                                  "Pinus sylvestris" = "P. sylvestris", "Castanea sativa" = "C. sativa",
                                                  "Quercus ilex" = "Q. ilex", "Quercus suber" = "Q. suber")) %>%
                               filter(!Sp.x %in%c("Quercus faginea", "Quercus canariensis")),
                             aes(Year, annual_temp_fin))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 240,
           size = 3.5) +
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  ggtitle("Annual mean temperature between 1994 and 2021")+
  labs(y="Temperature (ºC)")+
  scale_fill_brewer(palette="Paired")+
  theme_bar+
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 18 ),
        axis.title = element_text( size = 14, face = "bold" ),
        legend.position="none",
        strip.text = element_text(size = 14))


tmp_mean_94_21_regr

ggsave(tmp_mean_94_21_regr, device = "tiff", path = path2grafic, filename = "tmp_mean_94_21_regr.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(tmp_mean_94_21_regr, device = "png", path = path2grafic, filename = "tmp_mean_94_21_regr.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#juntamos en una misma grafica todas las especies


tmp_mean_regr_lines <-ggplot(clim94_21_annual_sp %>% 
                               filter(!Sp.x %in% c("Quercus faginea", "Quercus canariensis")),
                             aes(Year, annual_temp_fin, groups=Sp.x, color = Sp.x))+
  geom_smooth(method = "lm")+
  scale_colour_manual(values = colores)+
  #ggtitle("Annual mean temperature between 1994 and 2021")+
  labs(y="Temperature")+
  scale_fill_brewer(palette="Paired")+
  theme_bar+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12))
 

tmp_mean_regr_lines

ggsave(tmp_mean_regr_lines, device = "tiff", path = path2grafic, filename = "tmp_mean_regr_lines.tiff", 
       width = 10, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(tmp_mean_regr_lines, device = "png", path = path2grafic, filename = "tmp_mean_regr_lines.png", 
       width = 10, height = 6, units = 'in', dpi = 300)


#precipitacion
pre_mean_94_21_regr <-ggplot(clim94_21_annual_sp %>% 
                               mutate(Sp.x=recode(Sp.x, 'Eucalyptus camaldulensis'='E. camaldulsensis', 'Olea europaea'='O. europaea',
                                                  "Pinus halepensis" = "P. halepensis", "Pinus pinaster" = "P. pinaster",
                                                  "Pinus pinea" = "P. pinea", "Pinus nigra" = "P. nigra", 
                                                  "Pinus sylvestris" = "P. sylvestris", "Castanea sativa" = "C. sativa",
                                                  "Quercus ilex" = "Q. ilex", "Quercus suber" = "Q. suber")) %>%
                               filter(!Sp.x %in% c("Quercus faginea", "Quercus canariensis")),
                             aes(Year, annual_pre_fin))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 240,
           size = 3.5) +
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  ggtitle("Annual accumulated precipitation between 1994 and 2021")+
  labs(y="Accumulated precipitation (mm)")+
  scale_fill_brewer(palette="Paired")+
  theme_bar+
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 18 ),
        axis.title = element_text( size = 14, face = "bold" ),
        legend.position="none",
        strip.text = element_text(size = 14))

pre_mean_94_21_regr

ggsave(pre_mean_94_21_regr, device = "tiff", path = path2grafic, filename = "pre_mean_94_21_regr.tiff", 
       width = 12, height = 6, units = 'in', dpi = 300, compression = 'lzw')
ggsave(pre_mean_94_21_regr, device = "png", path = path2grafic, filename = "pre_mean_94_21_regr.png", 
       width = 12, height = 6, units = 'in', dpi = 300)


#Aridez
arid_mean_94_21_regr <- ggplot(clim94_21_annual_sp %>% 
                                mutate(Arid_Dm_i=100-Arid) %>% 
                                mutate(Sp.x=recode(Sp.x, 'Eucalyptus camaldulensis'='E. camaldulsensis', 'Olea europaea'='O. europaea',
                                                   "Pinus halepensis" = "P. halepensis", "Pinus pinaster" = "P. pinaster",
                                                   "Pinus pinea" = "P. pinea", "Pinus nigra" = "P. nigra", 
                                                   "Pinus sylvestris" = "P. sylvestris", "Castanea sativa" = "C. sativa",
                                                   "Quercus ilex" = "Q. ilex", "Quercus suber" = "Q. suber")) %>% 
                                filter(!Sp.x %in% c("Quercus faginea", "Quercus canariensis")),
                              aes(Year, Arid_Dm_i))+ #definimos la aridez
  geom_point()+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           #label.x = 1995, label.y = 240,
           size = 3.5) +
  facet_wrap(~Sp.x, scales = "free", nrow = 2)+
  ggtitle("Annual Aridity between 1994 and 2021")+
  labs(y="Aridity")+
  scale_fill_brewer(palette="Paired")+
  theme_bar+
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 18 ),
        axis.title = element_text( size = 14, face = "bold" ),
        legend.position="none",
        strip.text = element_text(size = 14))

arid_mean_94_21_regr

ggsave(arid_mean_94_21_regr, device = "tiff", path = path2grafic, filename = "arid_mean_94_21_regr.tiff", 
       width = 12, height = 16, units = 'in', dpi = 300, compression = 'lzw')
ggsave(arid_mean_94_21_regr, device = "png", path = path2grafic, filename = "arid_mean_94_21_regr.png", 
       width = 12, height = 6, units = 'in', dpi = 300)

