

#operamos los datos que tenemos previamente

dt.ant <- read_csv("CSV/datos_brutos_medias_250121.csv") %>% 
  select(Provincia_3:Y,Tree_dens2, Tree_dens, slope:hillshade, Sp.x, tf:ph, AB3_Mgha_perc, AB2_Mgha_perc) %>% 
  rename(Provincia=Provincia_3, Estadillo=Estadillo_3, Tree_dens3=Tree_dens)#datos de los cuales 
#apenas vamos a sacar algunas de las variables que nos interesan, la aridez y la biomasa son
#variables distintas a las que usaremos sacandolas del proximo dataframe

dt.raw <- read_excel("CSV/Andalucia_alive.xlsx")

dt.raw <- merge(dt.raw, dt.ant, by=c("Provincia", "Estadillo")) %>% mutate(Arid_Dm_i = 150-Aridity_Dm)

ndvi <-  read_csv("CSV/ndvi_maxmen_interpolados.csv") %>% 
  dplyr::select(-c(...1, X, Y)) %>% 
  rename(Provincia=Provincia_3, Estadillo=Estadillo_3)

#actualizamos el daytaframe con las nuevas series temporales actualizadas hasta el 2021
ndvi <- merge(ndvi %>% 
                dplyr::select(ID,Provincia, Estadillo), ndvi_total, by="ID")

#####datos que salva está pidiendo
ndvi_salva <- merge(ndvi, dt.raw %>% 
                             dplyr::select(Provincia, Estadillo, Sp.x, Tree_dens, Tree_dens2, Tree_dens3,
                                           preci, tempe, Arid_Dm_i, elevation, AB2_Mgha_perc, AB3_Mgha_perc), by=c("Provincia", "Estadillo")) %>% 
  filter(Sp.x=="Quercus ilex")#ojo, aqui filtarmos por la desnidad del IFN3 para los que tienen mas
# de 200 individuos por hectarea


write.csv(ndvi_salva, paste(path2csv,file = "ndvi_salva.csv"))


###### continuamos con el trabajo

ndvi<- merge(ndvi, dt.raw %>% 
               dplyr::select(Provincia, Estadillo, Sp.x, Tree_dens, Tree_dens2, Tree_dens3,
                      preci, tempe, Arid_Dm_i, elevation, AB2_Mgha_perc, AB3_Mgha_perc), by=c("Provincia", "Estadillo")) %>% 
  filter(Tree_dens3>150 & AB3_Mgha_perc > 90)#ojo, aqui filtarmos por la desnidad del IFN3 para los que tienen mas
# de 200 individuos por hectarea


hist_100 <- ggplot(ndvi %>% 
         filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                            "Pinus sylvestris", "Pinus nigra", "Quercus ilex", 
                            "Quercus suber", "Quercus faginea", "Olea europaea")), aes(Arid_Dm_i))+
  geom_histogram()+
  theme_bw()+
  facet_wrap(~Sp.x)  

plots_3 <- ndvi %>% group_by(Sp.x) %>% count() %>% arrange(-n)
plots_3


write.xlsx(ndvi, paste(path2csv, file = "ndvi_final_94_21.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "ndvi", append = FALSE)

ggplot(ndvi %>% 
         filter(Sp.x %in% c("Pinus halepensis", "Pinus pinea", "Pinus pinaster",
                            "Pinus sylvestris", "Pinus nigra", "Quercus ilex", 
                            "Quercus suber", "Quercus faginea", "Olea europaea")), aes(x=Arid_Dm_i, Tree_dens))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~Sp.x)


ndvi_pivot <- ndvi %>% 
  pivot_longer(cols=c("1994_jan":"2021_dez"),
               names_to='Year_month',
               values_to='NDVI_month') %>% 
  as.data.frame() #ojo, aqui hemos quitado el primer año de 94 puesto que al interpolar los datos
#ha cogido valores elevados fuera de rango

ndvi_pivot$nm <- c(1:12)
ndvi_pivot$m <- c("jan","feb","mar","apr","may",
                   "jun","jul","aug","sep","oct",
                   "nov","dec" )
ndvi_pivot$year <- str_sub(ndvi_pivot$Year_month,1,4)

ndvi_pivot <- ndvi_pivot %>% 
    mutate(cicle=as.numeric(recode(nm, "1"="05","2"="06","3"="07","4"="08","5"="09",
                             "6"="10","7"="11","8"="12","9"="01","10"="02",
                             "11"="03","12"="04"))) %>% 
  as.data.frame()#hemos cambiado el ciclo de la serie para que comienze en septiembre y acabe en agosto

ndvi_pivot$jd <- c(15,44,73,104,134,165,195,226,257,287,318,349)#introducimos los dias julianos a lo largo del año

# ndvi_pivot <- ndvi_pivot %>% 
#   mutate(date= paste(year, nm, "15", sep = "-"))





##### GRAFICOS POR ESPECIES EN EL TIEMPO #######
##### 
ex <- ggplot(ndvi_pivot %>% 
         filter(Provincia=="Huelva" & Estadillo=="0395"), aes(x=jd, y=NDVI_month, colour=year))+
  geom_line(aes(group=factor(year)))+theme_bw()+
  geom_point()+
  stat_summary(geom="line", fun = "mean", color="black", size=1, linetype="dashed")+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .3)
ex
ggplotly(ex)


#la densidad fue filtrada
halep <- ggplot(ndvi_pivot %>% 
                filter(Sp.x=="Pinus halepensis" & NDVI_month>0) %>% 
                  group_by(year),
         aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.30,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
                            "Jun", "Jul", "Aug")) +
  ggtitle("P halepensis") 
halep
ggplotly(halep)


# halep <- ggplot(ndvi_pivot %>% 
#                   filter(Sp.x=="Pinus halepensis" & NDVI_month>0),
#                 aes(x=cicle, y=NDVI_month, colour=year))+
#   geom_line(aes(group=as.factor(longitud)))+
#   theme_minimal() + facet_wrap(~year)+
#   stat_summary(geom="line", fun = "mean", color="black", size=2, linetype="dashed") +
#   stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .7)+
#   ggtitle("Seasonal NDVI P halepensis") 


ggplot(data=ndvi_pivot %>% 
              filter(Sp.x=="Pinus halepensis" & NDVI_month>0),
            aes(x=jd, y=NDVI_month, colour=year))+
    geom_line(aes(group=as.factor(longitud)))+
  theme_minimal() + 
  stat_summary(geom="line", fun = "mean", color="black", size=2, linetype="dashed") +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .7)+
  ggtitle("P halepensis") 




pinea <- ggplot(ndvi_pivot %>% 
                  filter(Sp.x=="Pinus pinea" & NDVI_month>0),
                aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.43,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  ggtitle("P pinea")

pinea


pinaster <- ggplot(ndvi_pivot %>% 
                  filter(Sp.x=="Pinus pinaster" & NDVI_month>0),
                aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.43,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  ggtitle("P pinaster")

pinaster


nigra <- ggplot(ndvi_pivot %>% 
                     filter(Sp.x=="Pinus nigra" & NDVI_month>0),
                   aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.43,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  ggtitle("P nigra")

nigra


sylvestris <- ggplot(ndvi_pivot %>% 
                     filter(Sp.x=="Pinus sylvestris" & NDVI_month>0),
                   aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.43,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  ggtitle("P sylvestris")

sylvestris


ilex <- ggplot(ndvi_pivot %>% 
                     filter(Sp.x=="Quercus ilex" & NDVI_month>0),
                   aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.43,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  ggtitle("Q ilex")

ilex


suber <- ggplot(ndvi_pivot %>% 
                 filter(Sp.x=="Quercus suber" & NDVI_month>0),
               aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.43,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  ggtitle("Q suber")

suber


faginea <- ggplot(ndvi_pivot %>% 
                  filter(Sp.x=="Quercus faginea" & NDVI_month>0),
                aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.43,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  ggtitle("Q faginea")
faginea

canariensis <- ggplot(ndvi_pivot %>% 
                    filter(Sp.x=="Quercus canariensis" & NDVI_month>0),
                  aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.43,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  ggtitle("Q canariensis")
canariensis

olea <- ggplot(ndvi_pivot %>% 
                    filter(Sp.x=="Olea europaea" & NDVI_month>0),
                  aes(x=cicle, y=NDVI_month, colour=year))+
  #geom_line(aes(group=as.factor(latitud)))+
  theme_bar2 +
  #scale_y_continuous(limits = c(0.43,0.57))+
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .2, size=1)+
  stat_summary(geom="line", fun = "mean", color="black", size=3, linetype="dashed") +
  xlim(c("Set", "Oct", "Nov", "Dec", "Jan", "Fev", "Mar", "Apr","May",
         "Jun", "Jul", "Aug")) +
  ggtitle("O europaea")
olea



sp_temp_ndvi <- ggarrange(halep + rremove("x.text"), pinaster +rremove("x.text"), 
                          nigra + rremove("x.text"), pinea +rremove("x.text"), 
                          sylvestris + rremove("x.text"), ilex + rremove("x.text"), 
                          suber , faginea, olea,
                          labels = c("A)", "B)", "C)", "D)", "E)", "F)", "G)", "H)", "I)", "F)"),
                          common.legend = T,
                          legend = "top",
                          align = "hv",
                          widths = 1,
                          heights = 1,
                          ncol = 3, nrow = 3)
sp_temp_ndvi


ggsave(sp_temp_ndvi, device = "tiff", path = path2grafic, filename = "sp_temp_ndvi.tiff", 
       width = 24, height = 18, units = 'in', dpi = 300, compression = 'lzw')
ggsave(sp_temp_ndvi, device = "png", path = path2grafic, filename = "sp_temp_ndvi.png", 
       width = 24, height = 18, units = 'in', dpi = 300)


#### cALCULAMOS LOS MAXIMOS Y MINIMOS ####
#estudiamos las series año a año para ver el start of season
ndvi_94 <- ndvi %>%
  select(Provincia, Estadillo, longitud, latitud, Sp.x, `00_94_01`:`00_94_12`, preci:Arid_Dm_i) %>%  
  rename("jan"=`00_94_01`,"fev"=`00_94_02`,"mar"=`00_94_03`,"apr"=`00_94_04`,"may"= `00_94_05`,
         "jun"=`00_94_06`,"jul"=`00_94_07`,"aug"=`00_94_08`,"sep"=`00_94_09`,"out"=`00_94_10`,
         "nov"=`00_94_11`,"dic"=`00_94_12`) %>%  
  rowwise() %>%
  mutate(max_1994 = max(jan:dic)) %>% 
  as.data.frame()
 
ndvi_94$min_1994 <- apply(X=ndvi_94[6:17], MARGIN=1, FUN=min)
ndvi_94$n_max_1994 <- max.col(ndvi_94[,6:17])

#manda pelotas, no existe min.col por lo que hay que definirla para poder ser aplicada
min.col <- function(m, ...) max.col(-m, ...)

ndvi_94$n_min_1994 <- min.col(ndvi_94[,6:17])


ndvi_95 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x,`01_95_01`:`01_95_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`01_95_01`:`01_95_12`)) 
ndvi_95$min <- apply(X=ndvi_95[6:17], MARGIN=1, FUN=min)
ndvi_95$n_max <- max.col(ndvi_95[,6:17])
ndvi_95$n_min <- min.col(ndvi_95[,6:17])
ndvi_95$year<-1995


ndvi_96 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x, `02_96_01`:`02_96_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`02_96_01`:`02_96_12`))
ndvi_96$min <- apply(X=ndvi_96[6:17], MARGIN=1, FUN=min)
ndvi_96$n_max <- max.col(ndvi_96[,6:17])
ndvi_96$n_min <- min.col(ndvi_96[,6:17])
ndvi_96$year<-1996

ndvi_97 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x,`03_97_01`:`03_97_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`03_97_01`:`03_97_12`))
ndvi_97$min <- apply(X=ndvi_97[6:17], MARGIN=1, FUN=min)
ndvi_97$n_max <- max.col(ndvi_97[,6:17])
ndvi_97$n_min <- min.col(ndvi_97[,6:17])
ndvi_97$year<-1997

ndvi_98 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x,`04_98_01`:`04_98_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`04_98_01`:`04_98_12`))
ndvi_98$min <- apply(X=ndvi_98[6:17], MARGIN=1, FUN=min)
ndvi_98$n_max <- max.col(ndvi_98[,6:17])
ndvi_98$n_min <- min.col(ndvi_98[,6:17])
ndvi_98$year<-1998

ndvi_99 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x,`05_99_01`:`05_99_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`05_99_01`:`05_99_12`))
ndvi_99$min <- apply(X=ndvi_99[6:17], MARGIN=1, FUN=min)
ndvi_99$n_max <- max.col(ndvi_99[,6:17])
ndvi_99$n_min <- min.col(ndvi_99[,6:17])
ndvi_99$year<-1999

ndvi_00 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x,`06_00_01`:`06_00_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`06_00_01`:`06_00_12`)) 
ndvi_00$min <- apply(X=ndvi_00[6:17], MARGIN=1, FUN=min)
ndvi_00$n_max <- max.col(ndvi_00[,6:17])
ndvi_00$n_min <- min.col(ndvi_00[,6:17])
ndvi_00$year<-2000

ndvi_01 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x,`07_01_01`:`07_01_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`07_01_01`:`07_01_12`)) 
ndvi_01$min <- apply(X=ndvi_01[6:17], MARGIN=1, FUN=min)
ndvi_01$n_max <- max.col(ndvi_01[,6:17])
ndvi_01$n_min <- min.col(ndvi_01[,6:17])
ndvi_01$year<-2001

ndvi_02 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x,`08_02_01`:`08_02_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`08_02_01`:`08_02_12`))
ndvi_02$min <- apply(X=ndvi_02[6:17], MARGIN=1, FUN=min)
ndvi_02$n_max <- max.col(ndvi_02[,6:17])
ndvi_02$n_min <- min.col(ndvi_02[,6:17])
ndvi_02$year<-2002

ndvi_03 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x, `09_03_01`:`09_03_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`09_03_01`:`09_03_12`)) 
ndvi_03$min <- apply(X=ndvi_03[6:17], MARGIN=1, FUN=min)
ndvi_03$n_max <- max.col(ndvi_03[,6:17])
ndvi_03$n_min <- min.col(ndvi_03[,6:17])
ndvi_03$year<-2003

ndvi_04 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x, `10_04_01`:`10_04_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`10_04_01`:`10_04_12`)) 
ndvi_04$min <- apply(X=ndvi_04[6:17], MARGIN=1, FUN=min)
ndvi_04$n_max <- max.col(ndvi_04[,6:17])
ndvi_04$n_min <- min.col(ndvi_04[,6:17])
ndvi_04$year<-2004

ndvi_05 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x, `11_05_01`:`11_05_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`11_05_01`:`11_05_12`))
ndvi_05$min <- apply(X=ndvi_05[6:17], MARGIN=1, FUN=min)
ndvi_05$n_max <- max.col(ndvi_05[,6:17])
ndvi_05$n_min <- min.col(ndvi_05[,6:17])
ndvi_05$year<-2005

ndvi_06 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x, `12_06_01`:`12_06_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`12_06_01`:`12_06_12`)) 
ndvi_06$min <- apply(X=ndvi_06[6:17], MARGIN=1, FUN=min)
ndvi_06$n_max <- max.col(ndvi_06[,6:17])
ndvi_06$n_min <- min.col(ndvi_06[,6:17])
ndvi_06$year<-2006

ndvi_07 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x, `13_07_01`:`13_07_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`13_07_01`:`13_07_12`))
ndvi_07$min <- apply(X=ndvi_07[6:17], MARGIN=1, FUN=min)
ndvi_07$n_max <- max.col(ndvi_07[,6:17])
ndvi_07$n_min <- min.col(ndvi_07[,6:17])
ndvi_07$year<-2007

ndvi_08 <- ndvi %>% 
  select(Provincia, Estadillo, longitud, latitud, Sp.x, `14_08_01`:`14_08_12`, preci:Arid_Dm_i) %>% 
  rowwise() %>%
  mutate (max = max(`14_08_01`:`14_08_12`))
ndvi_08$min <- apply(X=ndvi_08[6:17], MARGIN=1, FUN=min)
ndvi_08$n_max <- max.col(ndvi_08[,6:17])
ndvi_08$n_min <- min.col(ndvi_08[,6:17])
ndvi_08$year<-2008


#juntamos todos los dataframes
ndvi_fin <- rbind(ndvi_95 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_96 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_97 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_98 %>%
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_99 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_00 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_01 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_02 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_03 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_04 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_05 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_06 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_07 %>% 
                    select(Provincia:Sp.x, preci:n_min, year), ndvi_08 %>% 
                    select(Provincia:Sp.x, preci:n_min, year)) %>% 
  as.data.frame() 
ndvi_fin <- ndvi_fin %>% 
  mutate(mes_min = as.factor(recode(n_min,"1"="jan", "2"="fev", "3"="mar","4"="apr",
                        "5"="may","6"="jun","7"="jul",
                        "8"="aug","9"="sep","10"="out","11"="nov","12"="dic")), 
         mes_max = as.factor(recode(n_max,"1"="jan", "2"="fev", "3"="mar","4"="apr",
                "5"="may","6"="jun","7"="jul",
                "8"="aug","9"="sep","10"="out","11"="nov","12"="dic"))) %>% 
  na.omit %>% 
  as.data.frame()#como cojones es posible que haya NA 
#en las variables climáticas?????

write.xlsx(ndvi_fin, paste(path2csv, file = "ndvi_mes.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "ndvi_mes", append = FALSE)

# ndvi_fin_halep <- ndvi_fin %>%
#   filter(Sp.x=="Pinus halepensis") %>%
#   group_by(year) %>%
#   summarise(mean_preci= mean(preci), mean_tempe= mean(tempe),
#             mean_Arid= mean(Arid_Dm_i), m_max_val = mean(max), m_min_val= mean(min),
#             mean_max= mean(n_max), mean_min= mean(n_min))


#segun metodologia descrita por rafa, los puntos inferiores al 10% del maximo de ndvi debem ser filtrados
summary(ndvi_fin %>% filter(Sp.x=="Pinus halepensis"))#max de 0.91

hale_prec <- ggplot(data=ndvi_fin %>%
         filter(Sp.x=="Pinus halepensis") %>% 
           group_by(latitud) %>% 
           summarise(m_max=mean(max), preci),aes(x=preci, y=m_max))+
  geom_point()+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 600, label.y = 0.2, size = 5) +
  stat_regline_equation(label.x = 600, label.y = 0.25, size = 5)+
  geom_smooth(method = "lm")+theme_minimal()+
  ggtitle("P. halepensis Max NDVI and precipitation")
  
hale_prec

pinaster_prec <- ggplot(data=ndvi_fin %>%
                      filter(Sp.x=="Pinus pinaster") %>% 
                      group_by(latitud) %>% 
                      summarise(m_max=mean(max), preci),aes(x=preci, y=m_max))+
  geom_point()+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 600, label.y = 0.2, size = 5) +
  stat_regline_equation(label.x = 600, label.y = 0.25, size = 5)+
  geom_smooth(method = "lm")+theme_minimal()+
  ggtitle("P. pinaster Max NDVI and precipitation")
pinaster_prec


ilex_prec <- ggplot(data=ndvi_fin %>%
                      filter(Sp.x=="Quercus ilex") %>% 
                      group_by(latitud) %>% 
                      summarise(m_max=mean(max), preci),aes(x=preci, y=m_max))+
  geom_point()+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 600, label.y = 0.2, size = 5) +
  stat_regline_equation(label.x = 600, label.y = 0.25, size = 5)+
  geom_smooth(method="lm")+theme_minimal()+
  ggtitle("Q. ilex Max NDVI and precipitation")
ilex_prec



faginea_prec <- ggplot(data=ndvi_fin %>%
                         filter(Sp.x=="Quercus faginea") %>% 
                         group_by(latitud) %>% 
                         summarise(m_max=mean(max), preci),aes(x=preci, y=m_max))+
  geom_point()+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 600, label.y = 0.2, size = 5) +
  stat_regline_equation(label.x = 600, label.y = 0.25, size = 5)+
  geom_smooth(method="lm")+theme_minimal()+
  ggtitle("Q. faginea Max NDVI and precipitation")
faginea_prec

ndvi_precip_plots <- ggarrange(hale_prec, pinaster_prec, ilex_prec, faginea_prec,
                               labels = c("A)", "B)", "C)"),
                               common.legend = T,
                               legend = "top",
                               align = "hv",
                               widths = 1,
                               heights = 1,
                               ncol = 2, nrow = 2)
ndvi_precip_plots

ggsave(ndvi_precip_plots, device = "tiff", path = path2grafic, filename = "ndvi_precip_plots.tiff", 
       width = 10, height = 10, units = 'in', dpi = 300, compression = 'lzw')

ggsave(ndvi_precip_plots, device = "png", path = path2grafic, filename = "ndvi_precip_plots.png", 
       width = 10, height = 10, units = 'in', dpi = 300)



dt.raw <- merge(dt.raw, ndvi, by=c("Provincia", "Estadillo"))




# para el calculo de ilex
# #los datos provienen de Clima_pr_tmp_ndvi_especies
# ilexaridless <- dt.raw %>%
#   na.omit() %>% 
#   filter(Sp.x == "Quercus ilex") %>% 
#   top_n(-200, Arid_Dm_i) %>% 
#   as.data.frame() #hay que transformarlos en dataframe sino da error en el bucle
# 
# ilexaridplus <- dt.raw %>% 
#   na.omit() %>% 
#   filter(Sp.x == "Quercus ilex") %>% 
#   top_n(200, Arid_Dm_i) %>% 
#   as.data.frame()

# ilex3arid <- dt.raw %>% 
#   na.omit() %>% filter(Sp.x == "Quercus ilex") %>% arrange(Arid_Dm)

