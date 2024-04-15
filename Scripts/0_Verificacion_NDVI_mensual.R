#vamos a comprobar que los datos que tenemos de NDVI obtenidos del GEE coinciden con los obtenidos por una imagen 
#bajada del usgs Landsat 7. 
#para eso hemos extradido los valores de NDVI (previamente construida en qgis) del mes de agosto del 2000


NDVI_sample <- read_csv("IFN/01082000_muestra_NDVI_extract.csv")
colnames(NDVI_sample)[4] <- c("NDVI")

# NDVI_sample <- NDVI_sample %>% na.omit() %>% 
#   mutate(longitud = str_sub(longitud, 1, 9), latitud = str_sub(latitud, 1, 9)) %>% 
#   as.data.frame() 
# 
# ndvi_00_sample <- ndvi_00 %>% 
#   mutate(longitud = str_sub(longitud, 1, 9), latitud = str_sub(latitud, 1, 9))
  

compar_NDVI <- merge(ndvi_00, NDVI_sample, by=c("longitud", "latitud")) %>% na.omit()
colnames(compar_NDVI)[12] <- c("agosto")

ggplot(compar_NDVI, aes(agosto, NDVI))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           size = 6.5) +
  theme_bw()


################################################################################µ
################################################################################
################################################################################

#como rafa no se ha quedado convencido, hemos obtenido todas las escenas para varios puntos en Cazorla donde 
#en el mes de agosto pueden llegar a haber hasta 4 escenas
#por ese motivo se han procesado las bandas de 4 escenas obteniendo los valores 
#de NDVI para este mes en el ano de 2000

jaen_1080 <- read_csv("IFN/Jaen1080_muestra_NDVI_extract.csv") %>% 
  na.omit()
jaen_1080 <- jaen_1080 %>% 
  rowwise() %>%
  mutate(max_ago_2000 = max(NDVI_200033_20000831_20211120_4326, NDVI_200034_20000831_20211120_4326,
                            NDVI_200033_20000815_20200917_4326, NDVI_20034_20000815_20202918_4326))
colnames(jaen_1080)[1] <- "ID"

jaen_1080_compar <- merge(ndvi_00 %>% 
                            rename(ID=...1) %>% 
                            select(ID, latitud, longitud,"2000_aug"), jaen_1080 %>% 
                            select(ID,latitud, longitud, max_ago_2000), by= c("ID", "longitud", "latitud")) 

write.xlsx(jaen_1080_compar, paste(path2csv, file = "jaen_1080_compar.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "jaen_1080_compar", append = FALSE)







