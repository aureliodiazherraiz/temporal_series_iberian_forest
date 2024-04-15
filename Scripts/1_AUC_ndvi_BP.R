#### CALCULO DEL AUC DE LA MEDIA DEL NDVI ####

getwd()
setwd("C:/Aurelio_documentos/digital.agri")

path2csv <- "./CSV/"
path2grafic <- "./Graficos/"

ndvi_94<-read_excel("IFN/NDVI_mensuales_94.xlsx")
colnames(ndvi_94)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set","out", "nov", "dez")
colnames(ndvi_94)[4:15] <- paste("1994", colnames(ndvi_94[, 4:15]), sep = "_")

ndvi_95<-read_excel("IFN/NDVI_mensuales_95.xlsx")
colnames(ndvi_95)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set","out", "nov", "dez")
colnames(ndvi_95)[4:15] <- paste("1995", colnames(ndvi_95[, 4:15]), sep = "_")

ndvi_96<-read_excel("IFN/NDVI_mensuales_96.xlsx")
colnames(ndvi_96)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_96)[4:15] <- paste("1996", colnames(ndvi_96[, 4:15]), sep = "_")

ndvi_97<-read_excel("IFN/NDVI_mensuales_97.xlsx")
colnames(ndvi_97)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_97)[4:15] <- paste("1997", colnames(ndvi_97[, 4:15]), sep = "_")

ndvi_98<-read_excel("IFN/NDVI_mensuales_98.xlsx")
colnames(ndvi_98)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_98)[4:15] <- paste("1998", colnames(ndvi_98[, 4:15]), sep = "_")

ndvi_99<-read_excel("IFN/NDVI_mensuales_99.xlsx")
colnames(ndvi_99)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_99)[4:15] <- paste("1999", colnames(ndvi_99[, 4:15]), sep = "_")

ndvi_00<-read_excel("IFN/NDVI_mensuales_00.xlsx")
colnames(ndvi_00)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_00)[4:15] <- paste("2000", colnames(ndvi_00[, 4:15]), sep = "_")

ndvi_01<-read_excel("IFN/NDVI_mensuales_01.xlsx")
colnames(ndvi_01)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_01)[4:15] <- paste("2001", colnames(ndvi_01[, 4:15]), sep = "_")

ndvi_02<-read_excel("IFN/NDVI_mensuales_02.xlsx")
colnames(ndvi_02)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_02)[4:15] <- paste("2002", colnames(ndvi_02[, 4:15]), sep = "_")

ndvi_03<-read_excel("IFN/NDVI_mensuales_03_date.xlsx")
colnames(ndvi_03)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_03)[4:15] <- paste("2003", colnames(ndvi_03[, 4:15]), sep = "_")

ndvi_04<-read_excel("IFN/NDVI_mensuales_04.xlsx")
colnames(ndvi_04)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_04)[4:15] <- paste("2004", colnames(ndvi_04[, 4:15]), sep = "_")

ndvi_05<-read_excel("IFN/NDVI_mensuales_05.xlsx")
colnames(ndvi_05)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_05)[4:15] <- paste("2005", colnames(ndvi_05[, 4:15]), sep = "_")

ndvi_06<-read_excel("IFN/NDVI_mensuales_06.xlsx")
colnames(ndvi_06)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_06)[4:15] <- paste("2006", colnames(ndvi_06[, 4:15]), sep = "_")

ndvi_07<-read_excel("IFN/NDVI_mensuales_07.xlsx")
colnames(ndvi_07)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_07)[4:15] <- paste("2007", colnames(ndvi_07[, 4:15]), sep = "_")

ndvi_08<-read_excel("IFN/NDVI_mensuales_08.xlsx")
colnames(ndvi_08)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_08)[4:15] <- paste("2008", colnames(ndvi_08[, 4:15]), sep = "_")

ndvi_09<-read_excel("IFN/NDVI_mensuales_09.xlsx")
colnames(ndvi_09)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_09)[4:15] <- paste("2009", colnames(ndvi_09[, 4:15]), sep = "_")

ndvi_10<-read_excel("IFN/NDVI_mensuales_10.xlsx")
colnames(ndvi_10)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_10)[4:15] <- paste("2010", colnames(ndvi_10[, 4:15]), sep = "_")

ndvi_11<-read_excel("IFN/NDVI_mensuales_11.xlsx")
colnames(ndvi_11)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_11)[4:15] <- paste("2011", colnames(ndvi_11[, 4:15]), sep = "_")

ndvi_12<-read_excel("IFN/NDVI_mensuales_12.xlsx")
colnames(ndvi_12)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_12)[4:15] <- paste("2012", colnames(ndvi_12[, 4:15]), sep = "_")

ndvi_13<-read_excel("IFN/NDVI_mensuales_13.xlsx")
colnames(ndvi_13)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_13)[4:15] <- paste("2013", colnames(ndvi_13[, 4:15]), sep = "_")

ndvi_14<-read_excel("IFN/NDVI_mensuales_14.xlsx")
colnames(ndvi_14)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_14)[4:15] <- paste("2014", colnames(ndvi_14[, 4:15]), sep = "_")

ndvi_15<-read_excel("IFN/NDVI_mensuales_15.xlsx")
colnames(ndvi_15)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_15)[4:15] <- paste("2015", colnames(ndvi_15[, 4:15]), sep = "_")

ndvi_16<-read_excel("IFN/NDVI_mensuales_16.xlsx")
colnames(ndvi_16)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_16)[4:15] <- paste("2016", colnames(ndvi_16[, 4:15]), sep = "_")

ndvi_17<-read_excel("IFN/NDVI_mensuales_17.xlsx")
colnames(ndvi_17)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_17)[4:15] <- paste("2017", colnames(ndvi_17[, 4:15]), sep = "_")

ndvi_18<-read_excel("IFN/NDVI_mensuales_18.xlsx")
colnames(ndvi_18)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_18)[4:15] <- paste("2018", colnames(ndvi_18[, 4:15]), sep = "_")

ndvi_18 <- merge(ndvi_18, ndvi_rares, by=c("longitud","latitud"), all.x=T)
colnames(ndvi_18)[3]<-"id"
ndvi_18$`2018_mar`[ndvi_18$id==ndvi_18$ID] <- NA#ojo, hay valores negativos estraños que detectamos mas adelante
#eliminamos solo las parcelas FILTRADAS por densidad y abundancia de Suber y canariensis que mostraron
#datos anormales

ndvi_18 <- ndvi_18 %>% 
  rename(...1=id) %>% 
  select(longitud:`2018_dez`)


ndvi_19<-read_excel("IFN/NDVI_mensuales_19.xlsx")
colnames(ndvi_19)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_19)[4:15] <- paste("2019", colnames(ndvi_19[, 4:15]), sep = "_")

ndvi_20<-read_excel("IFN/NDVI_mensuales_20.xlsx")
colnames(ndvi_20)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_20)[4:15] <- paste("2020", colnames(ndvi_20[, 4:15]), sep = "_")

ndvi_21<-read_excel("IFN/NDVI_mensuales_21.xlsx")
colnames(ndvi_21)[4:15] <- c("jan", "fev", "mar", "abr", "may", "jun", "jul","aug", "set", "out","nov", "dez")
colnames(ndvi_21)[4:15] <- paste("2021", colnames(ndvi_21[, 4:15]), sep = "_")

#juntamos todos los ndvi anuales en un unico dataframe para conseguir interpolar los puntos
ndvi_total <- Reduce(merge, list(ndvi_94, ndvi_95, ndvi_96, ndvi_97, ndvi_98,
                                 ndvi_99, ndvi_00, ndvi_01, ndvi_02, ndvi_03, 
                                 ndvi_04, ndvi_05, ndvi_06, ndvi_07, ndvi_08,
                                 ndvi_09, ndvi_10, ndvi_11, ndvi_12, ndvi_13,
                                 ndvi_14, ndvi_15, ndvi_16, ndvi_17, ndvi_18,
                                 ndvi_19, ndvi_20, ndvi_21))
colnames(ndvi_total)[1] <- "ID"
ndvi_total <- ndvi_total %>% 
  column_to_rownames("ID")

ndvi_total_t<-as.data.frame(t(ndvi_total %>% 
                                dplyr::select(-latitud, -longitud)))#el dataframe se transpone para poder realizar la interpolacion pues sino lo hará por columnas

ndvi_total_i_<-na.interpolation(ndvi_total_t, option = "linear")#interpolamos para encontrar los NAs

ndvi_total<-as.data.frame(t(ndvi_total_i_)) %>% 
  rownames_to_column("ID")


ndvi_total <- merge(ndvi_00 %>% 
                      rename(ID=...1) %>% 
                      select(ID:latitud), ndvi_total, by=("ID"))



write.csv(ndvi_total, paste(path2csv,file = "ndvi_maxmen_interpolados_94_21.csv"))



