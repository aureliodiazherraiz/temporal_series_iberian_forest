#una vez que tenemos todos los datos de NDVI limpios (trend+season) podemos 
#calcular las métricas fenologicas con greenbrown sabiendoq ue teien conflicto
#con dplyr


#usamos ndvi_allsp_clean que proviene del script de limpieza, para eso antes de 
#transformarlo en una serie temporal habremos de preparar el formato

dt.clean <- ndvi_allsp_clean %>%  
  filter(Sp.x %in% c("Quercus faginea", 
                     "Quercus canariensis",
                     "Castanea sativa","Pinus halepensis",
                     "Pinus pinea", "Pinus pinaster",
                     "Pinus sylvestris", "Pinus nigra", 
                     "Eucalyptus camaldulensis", "Quercus ilex", 
                     "Quercus suber", "Olea europaea")) %>% 
  dplyr::select(ID,jan_1994:dec_2021)%>% 
  rownames_to_column() %>% 
  pivot_longer(cols=c(jan_1994:dec_2021),
               names_to='N_month',
               values_to='NDVI_clear_month') %>% 
dplyr::select(-rowname, -N_month)

#volvemos a empezar haciendo una lista para poder aplicar la funcion phenology con el NDVI limpio

split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[, col])

dt.clean <- split_tibble(dt.clean,"ID")


#ahora creamos el archivo temporal de cada uno de ellos 
dt.clean.list.ts <- lapply(dt.clean, 
                                   function(x) lapply(x, 
                                                      function(y) ts(y, freq=12, start=c(1994,1))))

dt.clean.list.ts[[1]][[2]]#para verificar que es una serie temporal


#aqui hemos tenido problemas para ajustar los argumentos del tsgf donde splines es el que mayores valores y menos Nas produce
#ademas dentro del approach hemos probado con White segun la literatura, Deriv y por ultimo Trs, en la cual deja alterar el argumento trs para 
#0.5 consiguiendo asi ampliar el LOS, mientras que los dos approach anteriores no aceptan trs, sea 0.1 sea 0.5 el resultado es el mismo
#existe un script que en parapelo ha estudiado todas las combinacinoes posibles. Resta estudiar si en el caso de las caducas este argumento
#deberia ser cambiado por White

my_list <- list()               # Create empty list
for(i in 1:length(dt.clean.list.ts)) {
  mymat <- Phenology(dt.clean.list.ts[[i]][[2]], 
                     tsgf="TSGFspline", approach="Deriv")
  my_list[[length(my_list) + 1]] <- mymat#podemos dejar approach="White" para las especies caducas mientras que para las perennes dejariamos "Deriv"
  #ademas tsgf="TSGFspline" nos devuelve valores mayores de LOS
}#obtengo una lista de dataframes

my_list[[1]][["los"]]

#ahora deberemos extraer las metricas para cada tipo de vegetacion

pheno_sos <- lapply(my_list, function(x) (x[["sos"]]))#extraemos variable una a una
pheno_sos <- as.data.frame(do.call("rbind", pheno_sos)) %>% 
  mutate(ID= names(dt.clean), var="sos") #transformamos las listas en dataframes juntando las columnas de cada plot mediante rbind, 
#ademas le introducimos los nombres de las parcelas mediante ID, sacamos las 9 variables fenologicas

pheno_eos <- lapply(my_list, function(x) (x[["eos"]]))
pheno_eos <- as.data.frame(do.call("rbind", pheno_eos)) %>% 
  mutate(ID= names(dt.clean), var="eos")

pheno_los <- lapply(my_list, function(x) (x[["los"]]))
pheno_los <- as.data.frame(do.call("rbind", pheno_los)) %>% 
  mutate(ID= names(dt.clean), var="los")

pheno_pop <- lapply(my_list, function(x) (x[["pop"]]))
pheno_pop <- as.data.frame(do.call("rbind", pheno_pop)) %>% 
  mutate(ID= names(dt.clean), var="pop")

pheno_peak <- lapply(my_list, function(x) (x[["peak"]]))
pheno_peak <- as.data.frame(do.call("rbind", pheno_peak)) %>% 
  mutate(ID= names(dt.clean), var="peak")

pheno_msp <- lapply(my_list, function(x) (x[["msp"]]))
pheno_msp <- as.data.frame(do.call("rbind", pheno_msp)) %>% 
  mutate(ID= names(dt.clean), var="msp")

pheno_mau <- lapply(my_list, function(x) (x[["mau"]]))
pheno_mau <- as.data.frame(do.call("rbind", pheno_mau)) %>% 
  mutate(ID= names(dt.clean), var="mau")

pheno_pot <- lapply(my_list, function(x) (x[["pot"]]))
pheno_pot <- as.data.frame(do.call("rbind", pheno_pot)) %>% 
  mutate(ID= names(dt.clean), var="pot")

pheno_trough <- lapply(my_list, function(x) (x[["trough"]]))
pheno_trough <- as.data.frame(do.call("rbind", pheno_trough)) %>% 
  mutate(ID= names(dt.clean), var="trough")


#correccion de los DoY (script especifico)??

pheno_metr <- rbind(pheno_eos, pheno_los, pheno_sos, 
                    pheno_pop, pheno_peak, pheno_msp, 
                    pheno_mau, pheno_pot, pheno_trough)#juntamos todas las variables

colnames(pheno_metr)[1:28]<- c(1994:2021)

pheno_metr <- merge(ndvi_allsp_clean %>% 
                            dplyr::select(ID, Sp.x), pheno_metr, by="ID")#los cruzamos
#con el dataframe que incorpora la especie para poder obtener las principales métricas
#por especie


####################ESTADISTICA CIRCULAR#####################################
#vamos explorar si algunas metricas deben ser corregidas:
#casos de sos y eos negativos (+365) o superiores a 365 (-365)
#Ademas vamos a transformarlos en medidas angulares para poder aplicar estadistica circular y asi poder compararlos

pheno_metr_sp_p <- pheno_metr %>% 
  dplyr::select(ID:var) %>% 
  pivot_longer(cols= -c(ID,Sp.x,var), names_to = c("year"), values_to = "value") %>% 
  pivot_wider(names_from = var, values_from = value) %>% #actualizamos los valores mediante correccion de sos y eos
  dplyr::mutate(eos=ifelse(eos<0, eos+365, ifelse(eos>365, eos-365, eos))) %>% 
  dplyr::mutate(sos=ifelse(sos<0, sos+365, ifelse(sos>365, sos-365, sos))) %>% #transformamos los datos en grados para poder aplicar estadistica circular
  mutate(eos_grados = eos*360/365, sos_grados = sos*360/365,
         pot_grados = pot*360/365, pop_grados = pop*360/365,
         los_grados = los*360/365,
         eos_rads = eos*2*pi/365, sos_rads = sos*2*pi/365,
         pot_rads = pot*2*pi/365, pop_rads = pop*2*pi/365,
         los_rads = los*2*pi/365) %>% 
  na.omit()


#aqui debemos aplicar la estadística circular para poder comparar valores ciclicos entre especies 
#para eso crearemos una lista y despues aplicaremos la funcion pàra cada uno de las observaciones
#trabajamos con grados puesto que hemos comprobado via excell el mismo resultado

dt.circ <- split_tibble(pheno_metr_sp_p %>% 
                          dplyr::select(ID, eos_grados:los_grados),"ID")

dt.circ <- lapply(dt.circ, #operamos en cada una de las listas con la funcion circular
                           function(x) lapply(x, 
                                              function(y) circ.summary(y, plot = F)))

#extraemos las metricas circulares de cada variable fenologica a lo largo del tiempo (1994:2021) para cada plot
#eos_circ
pheno_eos_circ_mesos <- lapply(dt.circ, function(x) (x[["eos_grados"]][["mesos"]]))
pheno_eos_circ_mesos <- as.data.frame(do.call("rbind", pheno_eos_circ_mesos)) %>% 
  rownames_to_column("ID") %>% 
  rename(eos_mesos=V1)

pheno_eos_circstd <- lapply(dt.circ, function(x) (x[["eos_grados"]][["circstd"]]))
pheno_eos_circstd <- as.data.frame(do.call("rbind", pheno_eos_circstd)) %>% 
  rownames_to_column("ID") %>% 
  rename(eos_circstd=V1)

#sos_circ
pheno_sos_circ_mesos <- lapply(dt.circ, function(x) (x[["sos_grados"]][["mesos"]]))
pheno_sos_circ_mesos <- as.data.frame(do.call("rbind", pheno_sos_circ_mesos)) %>% 
  rownames_to_column("ID") %>% 
  rename(sos_mesos=V1)

pheno_sos_circstd <- lapply(dt.circ, function(x) (x[["sos_grados"]][["circstd"]]))
pheno_sos_circstd <- as.data.frame(do.call("rbind", pheno_sos_circstd)) %>% 
  rownames_to_column("ID") %>% 
  rename(sos_circstd=V1)

#los_circ
pheno_los_circ_mesos <- lapply(dt.circ, function(x) (x[["los_grados"]][["mesos"]]))
pheno_los_circ_mesos <- as.data.frame(do.call("rbind", pheno_los_circ_mesos)) %>% 
  rownames_to_column("ID") %>% 
  rename(los_mesos=V1)

pheno_los_circstd <- lapply(dt.circ, function(x) (x[["los_grados"]][["circstd"]]))
pheno_los_circstd <- as.data.frame(do.call("rbind", pheno_los_circstd)) %>% 
  rownames_to_column("ID") %>% 
  rename(los_circstd=V1)

#pop_circ
pheno_pop_circ_mesos <- lapply(dt.circ, function(x) (x[["pop_grados"]][["mesos"]]))
pheno_pop_circ_mesos <- as.data.frame(do.call("rbind", pheno_pop_circ_mesos)) %>% 
  rownames_to_column("ID") %>% 
  rename(pop_mesos=V1)

pheno_pop_circstd <- lapply(dt.circ, function(x) (x[["pop_grados"]][["circstd"]]))
pheno_pop_circstd <- as.data.frame(do.call("rbind", pheno_pop_circstd)) %>% 
  rownames_to_column("ID") %>% 
  rename(pop_circstd=V1)

#pot_circ
pheno_pot_circ_mesos <- lapply(dt.circ, function(x) (x[["pot_grados"]][["mesos"]]))
pheno_pot_circ_mesos <- as.data.frame(do.call("rbind", pheno_pot_circ_mesos)) %>% 
  rownames_to_column("ID") %>% 
  rename(pot_mesos=V1)

pheno_pot_circstd <- lapply(dt.circ, function(x) (x[["pot_grados"]][["circstd"]]))
pheno_pot_circstd <- as.data.frame(do.call("rbind", pheno_pot_circstd)) %>% 
  rownames_to_column("ID") %>% 
  rename(pot_circstd=V1)


#ahora los juntamos 

pheno_circ <- Reduce(merge, list(pheno_eos_circ_mesos, pheno_eos_circstd,
                                 pheno_sos_circ_mesos, pheno_sos_circstd,
                                 pheno_los_circ_mesos, pheno_los_circstd,
                                 pheno_pop_circ_mesos, pheno_pop_circstd,
                                 pheno_pot_circ_mesos, pheno_pot_circstd))

#usamos el dataframe filtrado por las especies que nos hacen falta
ndvi_coord <- ndvi %>% 
  dplyr::select(Provincia:latitud, Sp.x) %>% 
  filter(Sp.x %in% c("Castanea sativa", "Eucalyptus camaldulensis", "Olea europaea",
                     "Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                     "Pinus sylvestris", "Pinus nigra", "Quercus canariensis", 
                     "Quercus faginea","Quercus ilex", "Quercus suber"))


pheno_circ <- merge(ndvi_coord, pheno_circ, by="ID")


#preparamos una lista de especies para poder operar en cada una de ellas y obtener las metricas angulares una vez mas
#esta vez una para cada especie
dt.circ.sp <- split_tibble(pheno_circ %>% 
                          dplyr::select(Sp.x, eos_mesos, sos_mesos,
                                        los_mesos, pop_mesos, pot_mesos),"Sp.x")

dt.circ.sp.eos <- lapply(dt.circ.sp, #operamos en cada una de las listas con la funcion circular
                  function(y) circ.summary(y$eos_mesos, plot = F))
dt.circ.sp.eos <- as.data.frame(do.call("rbind", dt.circ.sp.eos)) %>% 
  dplyr::select(mesos, circstd) %>% 
  rename(eos_mesos = mesos, eos_circstd = circstd)

dt.circ.sp.sos <- lapply(dt.circ.sp, #operamos en cada una de las listas con la funcion circular
                         function(y) circ.summary(y$sos_mesos, plot = F))
dt.circ.sp.sos <- as.data.frame(do.call("rbind", dt.circ.sp.sos)) %>% 
  dplyr::select(mesos, circstd)%>% 
  rename(sos_mesos = mesos, sos_circstd = circstd)

dt.circ.sp.los <- lapply(dt.circ.sp, #operamos en cada una de las listas con la funcion circular
                         function(y) circ.summary(y$los_mesos, plot = F))
dt.circ.sp.los <- as.data.frame(do.call("rbind", dt.circ.sp.los)) %>% 
  dplyr::select(mesos, circstd)%>% 
  rename(los_mesos = mesos, los_circstd = circstd)

dt.circ.sp.pot <- lapply(dt.circ.sp, #operamos en cada una de las listas con la funcion circular
                         function(y) circ.summary(y$pot_mesos, plot = F))
dt.circ.sp.pot <- as.data.frame(do.call("rbind", dt.circ.sp.pot)) %>% 
  dplyr::select(mesos, circstd)%>% 
  rename(pot_mesos = mesos, pot_circstd = circstd)

dt.circ.sp.pop <- lapply(dt.circ.sp, #operamos en cada una de las listas con la funcion circular
                         function(y) circ.summary(y$pop_mesos, plot = F))
dt.circ.sp.pop <- as.data.frame(do.call("rbind", dt.circ.sp.pop)) %>% 
  dplyr::select(mesos, circstd)%>% 
  rename(pop_mesos = mesos, pop_circstd = circstd) 

#finalmente tenemos un resumen por especie de las principales metricas fenologicas
dt.circ.sp.final <- cbind(dt.circ.sp.sos, dt.circ.sp.eos,
                                dt.circ.sp.los, dt.circ.sp.pot,
                                dt.circ.sp.pop)

dt.circ.sp.final$sos_mesos <- as.numeric(as.character(dt.circ.sp.final$sos_mesos)) #el formato es raro (AsIs), no lo consigo transformar directamente en dataframe
dt.circ.sp.final$eos_mesos <- as.numeric(as.character(dt.circ.sp.final$eos_mesos))
dt.circ.sp.final$los_mesos <- as.numeric(as.character(dt.circ.sp.final$los_mesos))
dt.circ.sp.final$pot_mesos <- as.numeric(as.character(dt.circ.sp.final$pot_mesos))
dt.circ.sp.final$pop_mesos <- as.numeric(as.character(dt.circ.sp.final$pop_mesos))

dt.circ.sp.final$sos_circstd <- as.numeric(as.character(dt.circ.sp.final$sos_circstd))
dt.circ.sp.final$eos_circstd <- as.numeric(as.character(dt.circ.sp.final$eos_circstd))
dt.circ.sp.final$los_circstd <- as.numeric(as.character(dt.circ.sp.final$los_circstd))
dt.circ.sp.final$pot_circstd <- as.numeric(as.character(dt.circ.sp.final$pot_circstd))
dt.circ.sp.final$pop_circstd <- as.numeric(as.character(dt.circ.sp.final$pop_circstd))

#ahora lo pasamos a dias de año 
dt.circ.sp.final <- dt.circ.sp.final %>% 
  mutate(sos_mesos_doy = sos_mesos*365/360,
         eos_mesos_doy = eos_mesos*365/360,
         los_mesos_doy = los_mesos*365/360,
         pot_mesos_doy = pot_mesos*365/360,
         pop_mesos_doy = pop_mesos*365/360,
         sos_circstd_doy = sos_circstd*365/360,
         eos_circstd_doy = eos_circstd*365/360,
         los_circstd_doy = los_circstd*365/360,
         pop_circstd_doy = pop_circstd*365/360,
         pot_circstd_doy = pot_circstd*365/360)

dt.circ.sp.final <- round(dt.circ.sp.final,2) %>% 
  rownames_to_column("Sp.x")


#####################################
#ahora calculamos las métricas principales como media, min y max para cada plot a lo largo del tiempo para despues hacerlo por especie
#pero estas metricas apenas servirán para las variables relativas al NDVI y al LOS (en dias absolutos)??
#puesto que el resto son angulares

pheno_metr$max <- apply(pheno_metr[3:30], 1, max, na.rm=TRUE)
pheno_metr$min <- apply(pheno_metr[3:30], 1, min, na.rm=TRUE)
pheno_metr$mean <- apply(pheno_metr[3:30], 1, mean, na.rm=TRUE)
#pheno_metr$sd <- apply(pheno_metr[3:30], 1, sd, na.rm=TRUE)


#ahora trabajamos con los valores por especie para completar el cuadro resumen
pheno_metr_final <- pheno_metr %>% 
  dplyr::select(Sp.x, var, max, min, mean) %>% 
  group_by(Sp.x, var) %>% 
  summarize(mean_var= mean(mean, na.rm=TRUE), max_var= max(max, na.rm=TRUE),
            min_var= min(min, na.rm=TRUE), sd_var=sd(mean,na.rm=TRUE))

#para poder incorporarlo a las metricas del NDVI lo pivoteamos
pheno_metr_final_p <- pheno_metr_final %>% 
  pivot_wider(names_from = var, values_from = c(mean_var:sd_var))

#lo juntoamos con los datos circulares que habiamos calculado previamente

pheno_metr_final_p <- merge(dt.circ.sp.final %>% 
                              dplyr::select(Sp.x,sos_mesos_doy:pop_mesos_doy),
                            pheno_metr_final_p %>% 
                              dplyr::select(Sp.x:mean_var_trough),
                            by="Sp.x")

#lo unimos
final_species <- cbind(pheno_metr_final_p, res_final %>% 
                         dplyr::select(-Sp.x))

#lo guardamos
write.xlsx(list(pheno_metr, pheno_metr_final,final_species, dt.circ.sp.final), paste(path2csv, file = "pheno_metr_Deriv.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = c("metrics","specie","final", "circular"), append = FALSE)


#### ANALISIS ESPACIAL DE LAS METRICAS FENOLOGICAS RESPECTO A FACTORES ABIOTICOS PARA CADA ESPECIE ####

pheno_metr_p <- pheno_metr %>% 
  dplyr::select(ID, Sp.x, var, mean) %>% 
  pivot_wider(names_from = var, values_from = mean)#ojo cuidado, hace falta introd
#el ID para separar todos las especies sino se vuelve loco

#ahora lo juntamos con las metricas circulares promediadas de cada plot a lo largo del tiempo, habra que transformarlas a dias

pheno_metr_p <- merge(pheno_metr_p, pheno_circ %>% 
                        dplyr::select(ID, eos_mesos, sos_mesos, los_mesos,
                                      pop_mesos, pot_mesos) %>% 
                        mutate(sos_mesos_doy=sos_mesos*365/360,
                               eos_mesos_doy=eos_mesos*365/360,
                               los_mesos_doy=los_mesos*365/360,
                               pot_mesos_doy=pot_mesos*365/360,
                               pop_mesos_doy=pop_mesos*365/360), by="ID")


pheno_metr_p_clima <- merge(pheno_metr_p %>% 
                              dplyr::select(ID,Sp.x, sos_mesos_doy, eos_mesos_doy, 
                                            los_mesos_doy, los, 
                                            pop_mesos_doy, pot_mesos_doy, peak, trough, 
                                            mau, msp,eos_mesos, sos_mesos, los_mesos,
                                            pop_mesos, pot_mesos), ndvi %>% 
                              dplyr::select(ID, Arid_Dm_i, elevation), by="ID") %>% 
  na.omit()

#ojo le incorporamos nuestras metricas anuales climaticas por plot promediados en el tiempo

pheno_metr_p_clima <- merge(pheno_metr_p_clima, clim94_21_annual_plot, by=c("ID", "Sp.x"))


#ahora podemos vincular las metricas climáticas de cada punto para poder hacer un PCA
#y ver la posible relacion con las metricas fenologicas
#### ANALISIS PCA CLIMA ####
#es posible que tb utilizemos cada variable por separado




pca_metric_sativa <- prcomp(pheno_metr_p_clima %>% 
                              filter(Sp.x=="Castanea sativa") %>% 
                              dplyr::select(mean_sos:elevation), scale. = T)

summary(pca_metric)#el primer con 38 y el segundo con 31 de varianza acumulando un 70

plot(pca_metric)

ggbiplot(pca_metric, obs.scale = 1, var.scale = 1, groups = pheno_metr_p_clima$Sp.x, ellipse = F)


library(FactoMineR)

pca_metric_sativa <- PCA(pheno_metr_p_clima %>% 
                      dplyr::select(mean_sos:elevation), scale. = T)

#tambien podemos hacer un mapa de correlaciones para cada una de las especies

######### CORRPLOT ############

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

par(mfrow=c(1,1))

#castanea sativa
cor_metrics_sativa <- cor(pheno_metr_p_clima %>% 
                            filter(Sp.x=="Castanea sativa") %>% 
                            dplyr::select(mean_sos:elevation))
testRes_sativa = cor.mtest(cor_metrics_sativa, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="faginea.png")
corrplot(cor_metrics_sativa, method="color", col=col(200),  
         type="upper", order="alphabet", title = "C. sativa",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 0.8,tl.cex = 0.8,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()

#pinus halepensis
cor_metrics_halep <- cor(pheno_metr_p_clima %>% 
                            filter(Sp.x=="Pinus halepensis") %>% 
                            dplyr::select(mean_sos:elevation))
testRes_halep = cor.mtest(cor_metrics_halep, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="hale.png")
corrplot(cor_metrics_halep, method="color", col=col(200),  
         type="upper", order="alphabet", title = "P. halep",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 1,tl.cex = 1,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()


#quercus ilex
cor_metrics_ilex <- cor(pheno_metr_p_clima %>% 
                           filter(Sp.x=="Quercus ilex") %>% 
                           dplyr::select(mean_sos:elevation))
testRes_ilex = cor.mtest(cor_metrics_ilex, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="ilex.png")
corrplot(cor_metrics_ilex, method="color", col=col(200),  
         type="upper", order="alphabet", title= "Quercus ilex",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 1,tl.cex = 1,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()


#pinus pinea
cor_metrics_pinea <- cor(pheno_metr_p_clima %>% 
                           filter(Sp.x=="Pinus pinea") %>% 
                           dplyr::select(mean_sos:elevation))
testRes_pinea = cor.mtest(cor_metrics_pinea, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="pinea.png")
corrplot(cor_metrics_pinea, title= "Pinus pinea", method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 1, tl.cex = 1,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()

#pinus pinaster
cor_metrics_pinaster <- cor(pheno_metr_p_clima %>% 
                           filter(Sp.x=="Pinus pinaster") %>% 
                           dplyr::select(mean_sos:elevation))
testRes_pinaster = cor.mtest(cor_metrics_pinaster, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="pinaster.png")
corrplot(cor_metrics_pinaster, title= "Pinus pinaster", method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 1,tl.cex = 1,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()

#pinus nigra
cor_metrics_nigra <- cor(pheno_metr_p_clima %>% 
                              filter(Sp.x=="Pinus nigra") %>% 
                              dplyr::select(mean_sos:elevation))
testRes_nigra = cor.mtest(cor_metrics_nigra, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="nigra.png")
corrplot(cor_metrics_nigra, title= "Pinus nigra", method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 1,tl.cex = 1,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()

#quercus suber
cor_metrics_suber <- cor(pheno_metr_p_clima %>% 
                          filter(Sp.x=="Quercus suber") %>% 
                          dplyr::select(mean_sos:elevation))
testRes_suber = cor.mtest(cor_metrics_suber, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="suber.png")
corrplot(cor_metrics_suber, title= "Quercus suber", method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 1,tl.cex = 1,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()

#pinus sylvestris
cor_metrics_sylvestris <- cor(pheno_metr_p_clima %>% 
                           filter(Sp.x=="Pinus sylvestris") %>% 
                           dplyr::select(mean_sos:elevation))
testRes_sylvestris = cor.mtest(cor_metrics_sylvestris, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="sylvestris.png")
corrplot(cor_metrics_sylvestris, title= "Pinus sylvestris", method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 1,tl.cex = 1,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()

#Olea europaea
cor_metrics_euro <- cor(pheno_metr_p_clima %>% 
                                filter(Sp.x=="Olea europaea") %>% 
                                dplyr::select(mean_sos:elevation))
testRes_euro = cor.mtest(cor_metrics_euro, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="olea.png")
euro <- corrplot(cor_metrics_euro, title= "Olea europaea", method="color", col=col(200),  
                 type="upper", order="alphabet",
                 addCoef.col = "black", # Add coefficient of correlation
                 tl.col="black", tl.srt=45, #Text label color and rotation
                 sig.level = 0.01, insig = "blank", 
                 number.cex = 1,tl.cex = 1,
                 diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()

#Eucaliptus camaldulensis
cor_metrics_euca <- cor(pheno_metr_p_clima %>% 
                          filter(Sp.x=="Eucalyptus camaldulensis") %>% 
                          dplyr::select(mean_sos:elevation))
testRes_euca = cor.mtest(cor_metrics_euca, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="euca.png")
corrplot(cor_metrics_euca, title= "Eucalyptus camaldulensis", method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 1,tl.cex = 1,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()

#quercus canariensis
cor_metrics_canariensis <- cor(pheno_metr_p_clima %>% 
                           filter(Sp.x=="Quercus canariensis") %>% 
                           dplyr::select(mean_sos:elevation))
testRes_canariensis = cor.mtest(cor_metrics_canariensis, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="canariensis.png")
canariensis <- corrplot(cor_metrics_canariensis, title= "Quercus canariensis", method="color", col=col(200),  
                        type="upper", order="alphabet",
                        addCoef.col = "black", # Add coefficient of correlation
                        tl.col="black", tl.srt=45, #Text label color and rotation
                        sig.level = 0.01, insig = "blank", 
                        number.cex = 1,tl.cex = 1,
                        diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()

#quercus faginea
cor_metrics_faginea <- cor(pheno_metr_p_clima %>% 
                           filter(Sp.x=="Quercus faginea") %>% 
                           dplyr::select(mean_sos:elevation))
testRes_faginea = cor.mtest(cor_metrics_faginea, conf.level = 0.95)

png(height=600, width=700, pointsize=15, file="faginea.png")
corrplot(cor_metrics_faginea, title= "Quercus faginea", method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", 
         number.cex = 1,tl.cex = 1,
         diag=FALSE, mar= c(2, 1, 3, 1))
dev.off()



