#correciones 2 rafa
#rafa desea calcular comparaciones no parametricas de cada métrica fenologica 
#partiendo de los valores medios de cada especie por categorias
#para eso  usaremos dt. temp

##########################################################
##SOS promediado en el tiempo (un valor medio por parcela)

#camaldulensis
eucal_sos_outliers_catg <- merge(spain_sel_camaldulensis %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

eucal_sos_outliers_catg_mean <- eucal_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))


kw.eucal.sos <- kruskal.test(mean_time_sos ~ Catg,
                       data = eucal_sos_outliers_catg)

kw.eucal.sos

DT.kw.eucal.sos = dunnTest(mean_time_sos ~ Catg,
                  data = eucal_sos_outliers_catg,
                  method="bh")#library(FSA)

DT.kw.eucal.sos = DT.kw.eucal.sos$res
letters_eucal.sos <- cldList(comparison = DT.kw.eucal.sos$Comparison,
                       p.value    = DT.kw.eucal.sos$P.adj,
                       threshold  = 0.05)


colnames(letters_eucal.sos)[1:2] <- c("Species","sos_letters")

letters_eucal.sos <- letters_eucal.sos[,1:2]
colnames(letters_eucal.sos)[2]<-"E. camaldulensis"

letters_eucal.sos



#sativa
sativa_sos_outliers_catg <- merge(spain_sel_sativa %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))


sativa_sos_outliers_catg_mean <- sativa_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))

kw.sativa.sos <- kruskal.test(mean_time_sos ~ Catg,
                             data = sativa_sos_outliers_catg)

kw.sativa.sos

DT.kw.sativa.sos = dunnTest(mean_time_sos ~ Catg,
                           data = sativa_sos_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.sativa.sos = DT.kw.sativa.sos$res
letters_sativa.sos <- cldList(comparison = DT.kw.sativa.sos$Comparison,
                             p.value    = DT.kw.sativa.sos$P.adj,
                             threshold  = 0.05)


colnames(letters_sativa.sos)[1:2] <- c("Species","sos_letters")

letters_sativa.sos <- letters_sativa.sos[,1:2]
colnames(letters_sativa.sos)[2]<-"C. sativa"

letters_sativa.sos


#europaea
euro_sos_outliers_catg <- merge(spain_sel_europaea %>% 
                                    rename("Sp.x"="Sp.x.x") %>% 
                                    select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                    select(-Arid_Dm_i), 
                                  by=c("Sp.x", "ID"))

euro_sos_outliers_catg_mean <- euro_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))


kw.euro.sos <- kruskal.test(mean_time_sos ~ Catg,
                              data = euro_sos_outliers_catg)

kw.euro.sos

DT.kw.euro.sos = dunnTest(mean_time_sos ~ Catg,
                            data = euro_sos_outliers_catg,
                            method="bh")#library(FSA)

DT.kw.euro.sos = DT.kw.euro.sos$res
letters_euro.sos <- cldList(comparison = DT.kw.euro.sos$Comparison,
                              p.value    = DT.kw.euro.sos$P.adj,
                              threshold  = 0.05)


colnames(letters_euro.sos)[1:2] <- c("Species","sos_letters")

letters_euro.sos <- letters_euro.sos[,1:2]
colnames(letters_euro.sos)[2]<-"C. euro"

letters_euro.sos


#halepensis
halep_sos_outliers_catg <- merge(spain_sel_halep %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

halep_sos_outliers_catg_mean <- halep_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))

kw.halep.sos <- kruskal.test(mean_time_sos ~ Catg,
                            data = halep_sos_outliers_catg)

kw.halep.sos

DT.kw.halep.sos = dunnTest(mean_time_sos ~ Catg,
                          data = halep_sos_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.halep.sos = DT.kw.halep.sos$res
letters_halep.sos <- cldList(comparison = DT.kw.halep.sos$Comparison,
                            p.value    = DT.kw.halep.sos$P.adj,
                            threshold  = 0.05)


colnames(letters_halep.sos)[1:2] <- c("Species","sos_letters")

letters_halep.sos <- letters_halep.sos[,1:2]
colnames(letters_halep.sos)[2]<-"P. halepensis"

letters_halep.sos


#pinaster
pinaster_sos_outliers_catg <- merge(spain_sel_pinaster %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

pinaster_sos_outliers_catg_mean <- pinaster_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))

kw.pinaster.sos <- kruskal.test(mean_time_sos ~ Catg,
                             data = pinaster_sos_outliers_catg)

kw.pinaster.sos

DT.kw.pinaster.sos = dunnTest(mean_time_sos ~ Catg,
                           data = pinaster_sos_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.pinaster.sos = DT.kw.pinaster.sos$res
letters_pinaster.sos <- cldList(comparison = DT.kw.pinaster.sos$Comparison,
                             p.value    = DT.kw.pinaster.sos$P.adj,
                             threshold  = 0.05)


colnames(letters_pinaster.sos)[1:2] <- c("Species","sos_letters")

letters_pinaster.sos <- letters_pinaster.sos[,1:2]
colnames(letters_pinaster.sos)[2]<-"P. pinaster"

letters_pinaster.sos



#pinea
pinea_sos_outliers_catg <- merge(spain_sel_pinea %>% 
                                      rename("Sp.x"="Sp.x.x") %>% 
                                      select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                      select(-Arid_Dm_i), 
                                    by=c("Sp.x", "ID"))

pinea_sos_outliers_catg_mean <- pinea_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))

kw.pinea.sos <- kruskal.test(mean_time_sos ~ Catg,
                                data = pinea_sos_outliers_catg)

kw.pinea.sos

DT.kw.pinea.sos = dunnTest(mean_time_sos ~ Catg,
                              data = pinea_sos_outliers_catg,
                              method="bh")#library(FSA)

DT.kw.pinea.sos = DT.kw.pinea.sos$res
letters_pinea.sos <- cldList(comparison = DT.kw.pinea.sos$Comparison,
                                p.value    = DT.kw.pinea.sos$P.adj,
                                threshold  = 0.05)


colnames(letters_pinea.sos)[1:2] <- c("Species","sos_letters")

letters_pinea.sos <- letters_pinea.sos[,1:2]
colnames(letters_pinea.sos)[2]<-"P. pinea"

letters_pinea.sos



#nigra
nigra_sos_outliers_catg <- merge(spain_sel_nigra %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

nigra_sos_outliers_catg_mean <- nigra_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))


kw.nigra.sos <- kruskal.test(mean_time_sos ~ Catg,
                             data = nigra_sos_outliers_catg)

kw.nigra.sos

DT.kw.nigra.sos = dunnTest(mean_time_sos ~ Catg,
                           data = nigra_sos_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.nigra.sos = DT.kw.nigra.sos$res
letters_nigra.sos <- cldList(comparison = DT.kw.nigra.sos$Comparison,
                             p.value    = DT.kw.nigra.sos$P.adj,
                             threshold  = 0.05)


colnames(letters_nigra.sos)[1:2] <- c("Species","sos_letters")

letters_nigra.sos <- letters_nigra.sos[,1:2]
colnames(letters_nigra.sos)[2]<-"P. nigra"

letters_nigra.sos



#sylvestris
sylvestris_sos_outliers_catg <- merge(spain_sel_sylvestris %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

sylvestris_sos_outliers_catg_mean <- sylvestris_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))


kw.sylvestris.sos <- kruskal.test(mean_time_sos ~ Catg,
                             data = sylvestris_sos_outliers_catg)

kw.sylvestris.sos

DT.kw.sylvestris.sos = dunnTest(mean_time_sos ~ Catg,
                           data = sylvestris_sos_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.sylvestris.sos = DT.kw.sylvestris.sos$res
letters_sylvestris.sos <- cldList(comparison = DT.kw.sylvestris.sos$Comparison,
                             p.value    = DT.kw.sylvestris.sos$P.adj,
                             threshold  = 0.05)


colnames(letters_sylvestris.sos)[1:2] <- c("Species","sos_letters")

letters_sylvestris.sos <- letters_sylvestris.sos[,1:2]
colnames(letters_sylvestris.sos)[2]<-"P. sylvestris"

letters_sylvestris.sos




#ilex
ilex_sos_outliers_catg <- merge(spain_sel_ilex %>% 
                                        rename("Sp.x"="Sp.x.x") %>% 
                                        select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                        select(-Arid_Dm_i), 
                                      by=c("Sp.x", "ID"))


ilex_sos_outliers_catg_mean <- ilex_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))

kw.ilex.sos <- kruskal.test(mean_time_sos ~ Catg,
                                  data = ilex_sos_outliers_catg)

kw.ilex.sos

DT.kw.ilex.sos = dunnTest(mean_time_sos ~ Catg,
                                data = ilex_sos_outliers_catg,
                                method="bh")#library(FSA)

DT.kw.ilex.sos = DT.kw.ilex.sos$res
letters_ilex.sos <- cldList(comparison = DT.kw.ilex.sos$Comparison,
                                  p.value    = DT.kw.ilex.sos$P.adj,
                                  threshold  = 0.05)


colnames(letters_ilex.sos)[1:2] <- c("Species","sos_letters")

letters_ilex.sos <- letters_ilex.sos[,1:2]
colnames(letters_ilex.sos)[2]<-"Q. ilex"

letters_ilex.sos


#suber
suber_sos_outliers_catg <- merge(spain_sel_suber %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.sos.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

suber_sos_outliers_catg_mean <- suber_sos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_sos=mean(mean_time_sos))


kw.suber.sos <- kruskal.test(mean_time_sos ~ Catg,
                            data = suber_sos_outliers_catg)

kw.suber.sos

DT.kw.suber.sos = dunnTest(mean_time_sos ~ Catg,
                          data = suber_sos_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.suber.sos = DT.kw.suber.sos$res
letters_suber.sos <- cldList(comparison = DT.kw.suber.sos$Comparison,
                            p.value    = DT.kw.suber.sos$P.adj,
                            threshold  = 0.05)


colnames(letters_suber.sos)[1:2] <- c("Species","sos_letters")

letters_suber.sos <- letters_suber.sos[,1:2]
colnames(letters_suber.sos)[2]<-"Q. suber"

letters_suber.sos

letters_sos <- cbind(letters_eucal.sos, letters_euro.sos[2],
                     letters_sativa.sos[2], letters_halep.sos[2],
                     letters_pinaster.sos[2], letters_pinea.sos[2],
                     letters_nigra.sos[2], letters_sylvestris.sos[2], 
                     letters_ilex.sos[2], letters_suber.sos[2])
letters_sos$metric<- c("SOS", "SOS","SOS")




###################################################################
##EOS


#camaldulensis
eucal_eos_outliers_catg <- merge(spain_sel_camaldulensis %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

eucal_eos_outliers_catg_mean <- eucal_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))


kw.eucal.eos <- kruskal.test(mean_time_eos ~ Catg,
                             data = eucal_eos_outliers_catg)

kw.eucal.eos

DT.kw.eucal.eos = dunnTest(mean_time_eos ~ Catg,
                           data = eucal_eos_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.eucal.eos = DT.kw.eucal.eos$res
letters_eucal.eos <- cldList(comparison = DT.kw.eucal.eos$Comparison,
                             p.value    = DT.kw.eucal.eos$P.adj,
                             threshold  = 0.05)


colnames(letters_eucal.eos)[1:2] <- c("Species","eos_letters")

letters_eucal.eos <- letters_eucal.eos[,1:2]
colnames(letters_eucal.eos)[2]<-"E. camaldulensis"

letters_eucal.eos



#sativa
sativa_eos_outliers_catg <- merge(spain_sel_sativa %>% 
                                    rename("Sp.x"="Sp.x.x") %>% 
                                    select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                    select(-Arid_Dm_i), 
                                  by=c("Sp.x", "ID"))


sativa_eos_outliers_catg_mean <- sativa_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))

kw.sativa.eos <- kruskal.test(mean_time_eos ~ Catg,
                              data = sativa_eos_outliers_catg)

kw.sativa.eos

DT.kw.sativa.eos = dunnTest(mean_time_eos ~ Catg,
                            data = sativa_eos_outliers_catg,
                            method="bh")#library(FSA)

DT.kw.sativa.eos = DT.kw.sativa.eos$res
letters_sativa.eos <- cldList(comparison = DT.kw.sativa.eos$Comparison,
                              p.value    = DT.kw.sativa.eos$P.adj,
                              threshold  = 0.05)


colnames(letters_sativa.eos)[1:2] <- c("Species","eos_letters")

letters_sativa.eos <- letters_sativa.eos[,1:2]
colnames(letters_sativa.eos)[2]<-"C. sativa"

letters_sativa.eos


#europaea
euro_eos_outliers_catg <- merge(spain_sel_europaea %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

euro_eos_outliers_catg_mean <- euro_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))


kw.euro.eos <- kruskal.test(mean_time_eos ~ Catg,
                            data = euro_eos_outliers_catg)

kw.euro.eos

DT.kw.euro.eos = dunnTest(mean_time_eos ~ Catg,
                          data = euro_eos_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.euro.eos = DT.kw.euro.eos$res
letters_euro.eos <- cldList(comparison = DT.kw.euro.eos$Comparison,
                            p.value    = DT.kw.euro.eos$P.adj,
                            threshold  = 0.05)


colnames(letters_euro.eos)[1:2] <- c("Species","eos_letters")

letters_euro.eos <- letters_euro.eos[,1:2]
colnames(letters_euro.eos)[2]<-"C. euro"

letters_euro.eos


#halepensis
halep_eos_outliers_catg <- merge(spain_sel_halep %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

halep_eos_outliers_catg_mean <- halep_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))

kw.halep.eos <- kruskal.test(mean_time_eos ~ Catg,
                             data = halep_eos_outliers_catg)

kw.halep.eos

DT.kw.halep.eos = dunnTest(mean_time_eos ~ Catg,
                           data = halep_eos_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.halep.eos = DT.kw.halep.eos$res
letters_halep.eos <- cldList(comparison = DT.kw.halep.eos$Comparison,
                             p.value    = DT.kw.halep.eos$P.adj,
                             threshold  = 0.05)


colnames(letters_halep.eos)[1:2] <- c("Species","eos_letters")

letters_halep.eos <- letters_halep.eos[,1:2]
colnames(letters_halep.eos)[2]<-"P. halepensis"

letters_halep.eos


#pinaster
pinaster_eos_outliers_catg <- merge(spain_sel_pinaster %>% 
                                      rename("Sp.x"="Sp.x.x") %>% 
                                      select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                      select(-Arid_Dm_i), 
                                    by=c("Sp.x", "ID"))

pinaster_eos_outliers_catg_mean <- pinaster_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))

kw.pinaster.eos <- kruskal.test(mean_time_eos ~ Catg,
                                data = pinaster_eos_outliers_catg)

kw.pinaster.eos

DT.kw.pinaster.eos = dunnTest(mean_time_eos ~ Catg,
                              data = pinaster_eos_outliers_catg,
                              method="bh")#library(FSA)

DT.kw.pinaster.eos = DT.kw.pinaster.eos$res
letters_pinaster.eos <- cldList(comparison = DT.kw.pinaster.eos$Comparison,
                                p.value    = DT.kw.pinaster.eos$P.adj,
                                threshold  = 0.05)


colnames(letters_pinaster.eos)[1:2] <- c("Species","eos_letters")

letters_pinaster.eos <- letters_pinaster.eos[,1:2]
colnames(letters_pinaster.eos)[2]<-"P. pinaster"

letters_pinaster.eos



#pinea
pinea_eos_outliers_catg <- merge(spain_sel_pinea %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

pinea_eos_outliers_catg_mean <- pinea_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))

kw.pinea.eos <- kruskal.test(mean_time_eos ~ Catg,
                             data = pinea_eos_outliers_catg)

kw.pinea.eos

DT.kw.pinea.eos = dunnTest(mean_time_eos ~ Catg,
                           data = pinea_eos_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.pinea.eos = DT.kw.pinea.eos$res
letters_pinea.eos <- cldList(comparison = DT.kw.pinea.eos$Comparison,
                             p.value    = DT.kw.pinea.eos$P.adj,
                             threshold  = 0.05)


colnames(letters_pinea.eos)[1:2] <- c("Species","eos_letters")

letters_pinea.eos <- letters_pinea.eos[,1:2]
colnames(letters_pinea.eos)[2]<-"P. pinea"

letters_pinea.eos



#nigra
nigra_eos_outliers_catg <- merge(spain_sel_nigra %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

nigra_eos_outliers_catg_mean <- nigra_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))


kw.nigra.eos <- kruskal.test(mean_time_eos ~ Catg,
                             data = nigra_eos_outliers_catg)

kw.nigra.eos

DT.kw.nigra.eos = dunnTest(mean_time_eos ~ Catg,
                           data = nigra_eos_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.nigra.eos = DT.kw.nigra.eos$res
letters_nigra.eos <- cldList(comparison = DT.kw.nigra.eos$Comparison,
                             p.value    = DT.kw.nigra.eos$P.adj,
                             threshold  = 0.05)


colnames(letters_nigra.eos)[1:2] <- c("Species","eos_letters")

letters_nigra.eos <- letters_nigra.eos[,1:2]
colnames(letters_nigra.eos)[2]<-"P. nigra"

letters_nigra.eos



#sylvestris
sylvestris_eos_outliers_catg <- merge(spain_sel_sylvestris %>% 
                                        rename("Sp.x"="Sp.x.x") %>% 
                                        select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                        select(-Arid_Dm_i), 
                                      by=c("Sp.x", "ID"))

sylvestris_eos_outliers_catg_mean <- sylvestris_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))


kw.sylvestris.eos <- kruskal.test(mean_time_eos ~ Catg,
                                  data = sylvestris_eos_outliers_catg)

kw.sylvestris.eos

DT.kw.sylvestris.eos = dunnTest(mean_time_eos ~ Catg,
                                data = sylvestris_eos_outliers_catg,
                                method="bh")#library(FSA)

DT.kw.sylvestris.eos = DT.kw.sylvestris.eos$res
letters_sylvestris.eos <- cldList(comparison = DT.kw.sylvestris.eos$Comparison,
                                  p.value    = DT.kw.sylvestris.eos$P.adj,
                                  threshold  = 0.05)


colnames(letters_sylvestris.eos)[1:2] <- c("Species","eos_letters")

letters_sylvestris.eos <- letters_sylvestris.eos[,1:2]
colnames(letters_sylvestris.eos)[2]<-"P. sylvestris"

letters_sylvestris.eos




#ilex
ilex_eos_outliers_catg <- merge(spain_sel_ilex %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))


ilex_eos_outliers_catg_mean <- ilex_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))

kw.ilex.eos <- kruskal.test(mean_time_eos ~ Catg,
                            data = ilex_eos_outliers_catg)

kw.ilex.eos

DT.kw.ilex.eos = dunnTest(mean_time_eos ~ Catg,
                          data = ilex_eos_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.ilex.eos = DT.kw.ilex.eos$res
letters_ilex.eos <- cldList(comparison = DT.kw.ilex.eos$Comparison,
                            p.value    = DT.kw.ilex.eos$P.adj,
                            threshold  = 0.05)


colnames(letters_ilex.eos)[1:2] <- c("Species","eos_letters")

letters_ilex.eos <- letters_ilex.eos[,1:2]
colnames(letters_ilex.eos)[2]<-"Q. ilex"

letters_ilex.eos


#suber
suber_eos_outliers_catg <- merge(spain_sel_suber %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.eos.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

suber_eos_outliers_catg_mean <- suber_eos_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_eos=mean(mean_time_eos))


kw.suber.eos <- kruskal.test(mean_time_eos ~ Catg,
                             data = suber_eos_outliers_catg)

kw.suber.eos

DT.kw.suber.eos = dunnTest(mean_time_eos ~ Catg,
                           data = suber_eos_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.suber.eos = DT.kw.suber.eos$res
letters_suber.eos <- cldList(comparison = DT.kw.suber.eos$Comparison,
                             p.value    = DT.kw.suber.eos$P.adj,
                             threshold  = 0.05)


colnames(letters_suber.eos)[1:2] <- c("Species","eos_letters")

letters_suber.eos <- letters_suber.eos[,1:2]
colnames(letters_suber.eos)[2]<-"Q. suber"

letters_suber.eos

letters_eos <- cbind(letters_eucal.eos, letters_euro.eos[2],
                     letters_sativa.eos[2], letters_halep.eos[2],
                     letters_pinaster.eos[2], letters_pinea.eos[2],
                     letters_nigra.eos[2], letters_sylvestris.eos[2], 
                     letters_ilex.eos[2], letters_suber.eos[2])
letters_eos$metric<- c("eos", "eos","eos")


#############################################################################
##LOS

#camaldulensis
eucal_los_outliers_catg <- merge(spain_sel_camaldulensis %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

eucal_los_outliers_catg_mean <- eucal_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))


kw.eucal.los <- kruskal.test(mean_time_los ~ Catg,
                             data = eucal_los_outliers_catg)

kw.eucal.los

DT.kw.eucal.los = dunnTest(mean_time_los ~ Catg,
                           data = eucal_los_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.eucal.los = DT.kw.eucal.los$res
letters_eucal.los <- cldList(comparison = DT.kw.eucal.los$Comparison,
                             p.value    = DT.kw.eucal.los$P.adj,
                             threshold  = 0.05)


colnames(letters_eucal.los)[1:2] <- c("Species","los_letters")

letters_eucal.los <- letters_eucal.los[,1:2]
colnames(letters_eucal.los)[2]<-"E. camaldulensis"

letters_eucal.los



#sativa
sativa_los_outliers_catg <- merge(spain_sel_sativa %>% 
                                    rename("Sp.x"="Sp.x.x") %>% 
                                    select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                    select(-Arid_Dm_i), 
                                  by=c("Sp.x", "ID"))


sativa_los_outliers_catg_mean <- sativa_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))

kw.sativa.los <- kruskal.test(mean_time_los ~ Catg,
                              data = sativa_los_outliers_catg)

kw.sativa.los

DT.kw.sativa.los = dunnTest(mean_time_los ~ Catg,
                            data = sativa_los_outliers_catg,
                            method="bh")#library(FSA)

DT.kw.sativa.los = DT.kw.sativa.los$res
letters_sativa.los <- cldList(comparison = DT.kw.sativa.los$Comparison,
                              p.value    = DT.kw.sativa.los$P.adj,
                              threshold  = 0.05)


colnames(letters_sativa.los)[1:2] <- c("Species","los_letters")

letters_sativa.los <- letters_sativa.los[,1:2]
colnames(letters_sativa.los)[2]<-"C. sativa"

letters_sativa.los


#europaea
euro_los_outliers_catg <- merge(spain_sel_europaea %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

euro_los_outliers_catg_mean <- euro_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))


kw.euro.los <- kruskal.test(mean_time_los ~ Catg,
                            data = euro_los_outliers_catg)

kw.euro.los

DT.kw.euro.los = dunnTest(mean_time_los ~ Catg,
                          data = euro_los_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.euro.los = DT.kw.euro.los$res
letters_euro.los <- cldList(comparison = DT.kw.euro.los$Comparison,
                            p.value    = DT.kw.euro.los$P.adj,
                            threshold  = 0.05)


colnames(letters_euro.los)[1:2] <- c("Species","los_letters")

letters_euro.los <- letters_euro.los[,1:2]
colnames(letters_euro.los)[2]<-"C. euro"

letters_euro.los


#halepensis
halep_los_outliers_catg <- merge(spain_sel_halep %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

halep_los_outliers_catg_mean <- halep_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))

kw.halep.los <- kruskal.test(mean_time_los ~ Catg,
                             data = halep_los_outliers_catg)

kw.halep.los

DT.kw.halep.los = dunnTest(mean_time_los ~ Catg,
                           data = halep_los_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.halep.los = DT.kw.halep.los$res
letters_halep.los <- cldList(comparison = DT.kw.halep.los$Comparison,
                             p.value    = DT.kw.halep.los$P.adj,
                             threshold  = 0.05)


colnames(letters_halep.los)[1:2] <- c("Species","los_letters")

letters_halep.los <- letters_halep.los[,1:2]
colnames(letters_halep.los)[2]<-"P. halepensis"

letters_halep.los


#pinaster
pinaster_los_outliers_catg <- merge(spain_sel_pinaster %>% 
                                      rename("Sp.x"="Sp.x.x") %>% 
                                      select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                      select(-Arid_Dm_i), 
                                    by=c("Sp.x", "ID"))

pinaster_los_outliers_catg_mean <- pinaster_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))

kw.pinaster.los <- kruskal.test(mean_time_los ~ Catg,
                                data = pinaster_los_outliers_catg)

kw.pinaster.los

DT.kw.pinaster.los = dunnTest(mean_time_los ~ Catg,
                              data = pinaster_los_outliers_catg,
                              method="bh")#library(FSA)

DT.kw.pinaster.los = DT.kw.pinaster.los$res
letters_pinaster.los <- cldList(comparison = DT.kw.pinaster.los$Comparison,
                                p.value    = DT.kw.pinaster.los$P.adj,
                                threshold  = 0.05)


colnames(letters_pinaster.los)[1:2] <- c("Species","los_letters")

letters_pinaster.los <- letters_pinaster.los[,1:2]
colnames(letters_pinaster.los)[2]<-"P. pinaster"

letters_pinaster.los



#pinea
pinea_los_outliers_catg <- merge(spain_sel_pinea %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

pinea_los_outliers_catg_mean <- pinea_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))

kw.pinea.los <- kruskal.test(mean_time_los ~ Catg,
                             data = pinea_los_outliers_catg)

kw.pinea.los

DT.kw.pinea.los = dunnTest(mean_time_los ~ Catg,
                           data = pinea_los_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.pinea.los = DT.kw.pinea.los$res
letters_pinea.los <- cldList(comparison = DT.kw.pinea.los$Comparison,
                             p.value    = DT.kw.pinea.los$P.adj,
                             threshold  = 0.05)


colnames(letters_pinea.los)[1:2] <- c("Species","los_letters")

letters_pinea.los <- letters_pinea.los[,1:2]
colnames(letters_pinea.los)[2]<-"P. pinea"

letters_pinea.los



#nigra
nigra_los_outliers_catg <- merge(spain_sel_nigra %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

nigra_los_outliers_catg_mean <- nigra_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))


kw.nigra.los <- kruskal.test(mean_time_los ~ Catg,
                             data = nigra_los_outliers_catg)

kw.nigra.los

DT.kw.nigra.los = dunnTest(mean_time_los ~ Catg,
                           data = nigra_los_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.nigra.los = DT.kw.nigra.los$res
letters_nigra.los <- cldList(comparison = DT.kw.nigra.los$Comparison,
                             p.value    = DT.kw.nigra.los$P.adj,
                             threshold  = 0.05)


colnames(letters_nigra.los)[1:2] <- c("Species","los_letters")

letters_nigra.los <- letters_nigra.los[,1:2]
colnames(letters_nigra.los)[2]<-"P. nigra"

letters_nigra.los



#sylvestris
sylvestris_los_outliers_catg <- merge(spain_sel_sylvestris %>% 
                                        rename("Sp.x"="Sp.x.x") %>% 
                                        select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                        select(-Arid_Dm_i), 
                                      by=c("Sp.x", "ID"))

sylvestris_los_outliers_catg_mean <- sylvestris_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))


kw.sylvestris.los <- kruskal.test(mean_time_los ~ Catg,
                                  data = sylvestris_los_outliers_catg)

kw.sylvestris.los

DT.kw.sylvestris.los = dunnTest(mean_time_los ~ Catg,
                                data = sylvestris_los_outliers_catg,
                                method="bh")#library(FSA)

DT.kw.sylvestris.los = DT.kw.sylvestris.los$res
letters_sylvestris.los <- cldList(comparison = DT.kw.sylvestris.los$Comparison,
                                  p.value    = DT.kw.sylvestris.los$P.adj,
                                  threshold  = 0.05)


colnames(letters_sylvestris.los)[1:2] <- c("Species","los_letters")

letters_sylvestris.los <- letters_sylvestris.los[,1:2]
colnames(letters_sylvestris.los)[2]<-"P. sylvestris"

letters_sylvestris.los




#ilex
ilex_los_outliers_catg <- merge(spain_sel_ilex %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))


ilex_los_outliers_catg_mean <- ilex_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))

kw.ilex.los <- kruskal.test(mean_time_los ~ Catg,
                            data = ilex_los_outliers_catg)

kw.ilex.los

DT.kw.ilex.los = dunnTest(mean_time_los ~ Catg,
                          data = ilex_los_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.ilex.los = DT.kw.ilex.los$res
letters_ilex.los <- cldList(comparison = DT.kw.ilex.los$Comparison,
                            p.value    = DT.kw.ilex.los$P.adj,
                            threshold  = 0.05)


colnames(letters_ilex.los)[1:2] <- c("Species","los_letters")

letters_ilex.los <- letters_ilex.los[,1:2]
colnames(letters_ilex.los)[2]<-"Q. ilex"

letters_ilex.los


#suber
suber_los_outliers_catg <- merge(spain_sel_suber %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.los.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

suber_los_outliers_catg_mean <- suber_los_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_los=mean(mean_time_los))


kw.suber.los <- kruskal.test(mean_time_los ~ Catg,
                             data = suber_los_outliers_catg)

kw.suber.los

DT.kw.suber.los = dunnTest(mean_time_los ~ Catg,
                           data = suber_los_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.suber.los = DT.kw.suber.los$res
letters_suber.los <- cldList(comparison = DT.kw.suber.los$Comparison,
                             p.value    = DT.kw.suber.los$P.adj,
                             threshold  = 0.05)


colnames(letters_suber.los)[1:2] <- c("Species","los_letters")

letters_suber.los <- letters_suber.los[,1:2]
colnames(letters_suber.los)[2]<-"Q. suber"

letters_suber.los

letters_los <- cbind(letters_eucal.los, letters_euro.los[2],
                     letters_sativa.los[2], letters_halep.los[2],
                     letters_pinaster.los[2], letters_pinea.los[2],
                     letters_nigra.los[2], letters_sylvestris.los[2], 
                     letters_ilex.los[2], letters_suber.los[2])
letters_los$metric<- c("los", "los","los")


##############################################################################
##POT

#camaldulensis
eucal_pot_outliers_catg <- merge(spain_sel_camaldulensis %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

eucal_pot_outliers_catg_mean <- eucal_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))


kw.eucal.pot <- kruskal.test(mean_time_pot ~ Catg,
                             data = eucal_pot_outliers_catg)

kw.eucal.pot

DT.kw.eucal.pot = dunnTest(mean_time_pot ~ Catg,
                           data = eucal_pot_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.eucal.pot = DT.kw.eucal.pot$res
letters_eucal.pot <- cldList(comparison = DT.kw.eucal.pot$Comparison,
                             p.value    = DT.kw.eucal.pot$P.adj,
                             threshold  = 0.05)


colnames(letters_eucal.pot)[1:2] <- c("Species","pot_letters")

letters_eucal.pot <- letters_eucal.pot[,1:2]
colnames(letters_eucal.pot)[2]<-"E. camaldulensis"

letters_eucal.pot



#sativa
sativa_pot_outliers_catg <- merge(spain_sel_sativa %>% 
                                    rename("Sp.x"="Sp.x.x") %>% 
                                    select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                    select(-Arid_Dm_i), 
                                  by=c("Sp.x", "ID"))


sativa_pot_outliers_catg_mean <- sativa_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))

kw.sativa.pot <- kruskal.test(mean_time_pot ~ Catg,
                              data = sativa_pot_outliers_catg)

kw.sativa.pot

DT.kw.sativa.pot = dunnTest(mean_time_pot ~ Catg,
                            data = sativa_pot_outliers_catg,
                            method="bh")#library(FSA)

DT.kw.sativa.pot = DT.kw.sativa.pot$res
letters_sativa.pot <- cldList(comparison = DT.kw.sativa.pot$Comparison,
                              p.value    = DT.kw.sativa.pot$P.adj,
                              threshold  = 0.05)


colnames(letters_sativa.pot)[1:2] <- c("Species","pot_letters")

letters_sativa.pot <- letters_sativa.pot[,1:2]
colnames(letters_sativa.pot)[2]<-"C. sativa"

letters_sativa.pot


#europaea
euro_pot_outliers_catg <- merge(spain_sel_europaea %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

euro_pot_outliers_catg_mean <- euro_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))


kw.euro.pot <- kruskal.test(mean_time_pot ~ Catg,
                            data = euro_pot_outliers_catg)

kw.euro.pot

DT.kw.euro.pot = dunnTest(mean_time_pot ~ Catg,
                          data = euro_pot_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.euro.pot = DT.kw.euro.pot$res
letters_euro.pot <- cldList(comparison = DT.kw.euro.pot$Comparison,
                            p.value    = DT.kw.euro.pot$P.adj,
                            threshold  = 0.05)


colnames(letters_euro.pot)[1:2] <- c("Species","pot_letters")

letters_euro.pot <- letters_euro.pot[,1:2]
colnames(letters_euro.pot)[2]<-"C. euro"

letters_euro.pot


#halepensis
halep_pot_outliers_catg <- merge(spain_sel_halep %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

halep_pot_outliers_catg_mean <- halep_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))

kw.halep.pot <- kruskal.test(mean_time_pot ~ Catg,
                             data = halep_pot_outliers_catg)

kw.halep.pot

DT.kw.halep.pot = dunnTest(mean_time_pot ~ Catg,
                           data = halep_pot_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.halep.pot = DT.kw.halep.pot$res
letters_halep.pot <- cldList(comparison = DT.kw.halep.pot$Comparison,
                             p.value    = DT.kw.halep.pot$P.adj,
                             threshold  = 0.05)


colnames(letters_halep.pot)[1:2] <- c("Species","pot_letters")

letters_halep.pot <- letters_halep.pot[,1:2]
colnames(letters_halep.pot)[2]<-"P. halepensis"

letters_halep.pot


#pinaster
pinaster_pot_outliers_catg <- merge(spain_sel_pinaster %>% 
                                      rename("Sp.x"="Sp.x.x") %>% 
                                      select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                      select(-Arid_Dm_i), 
                                    by=c("Sp.x", "ID"))

pinaster_pot_outliers_catg_mean <- pinaster_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))

kw.pinaster.pot <- kruskal.test(mean_time_pot ~ Catg,
                                data = pinaster_pot_outliers_catg)

kw.pinaster.pot

DT.kw.pinaster.pot = dunnTest(mean_time_pot ~ Catg,
                              data = pinaster_pot_outliers_catg,
                              method="bh")#library(FSA)

DT.kw.pinaster.pot = DT.kw.pinaster.pot$res
letters_pinaster.pot <- cldList(comparison = DT.kw.pinaster.pot$Comparison,
                                p.value    = DT.kw.pinaster.pot$P.adj,
                                threshold  = 0.05)


colnames(letters_pinaster.pot)[1:2] <- c("Species","pot_letters")

letters_pinaster.pot <- letters_pinaster.pot[,1:2]
colnames(letters_pinaster.pot)[2]<-"P. pinaster"

letters_pinaster.pot



#pinea
pinea_pot_outliers_catg <- merge(spain_sel_pinea %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

pinea_pot_outliers_catg_mean <- pinea_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))

kw.pinea.pot <- kruskal.test(mean_time_pot ~ Catg,
                             data = pinea_pot_outliers_catg)

kw.pinea.pot

DT.kw.pinea.pot = dunnTest(mean_time_pot ~ Catg,
                           data = pinea_pot_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.pinea.pot = DT.kw.pinea.pot$res
letters_pinea.pot <- cldList(comparison = DT.kw.pinea.pot$Comparison,
                             p.value    = DT.kw.pinea.pot$P.adj,
                             threshold  = 0.05)


colnames(letters_pinea.pot)[1:2] <- c("Species","pot_letters")

letters_pinea.pot <- letters_pinea.pot[,1:2]
colnames(letters_pinea.pot)[2]<-"P. pinea"

letters_pinea.pot



#nigra
nigra_pot_outliers_catg <- merge(spain_sel_nigra %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

nigra_pot_outliers_catg_mean <- nigra_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))


kw.nigra.pot <- kruskal.test(mean_time_pot ~ Catg,
                             data = nigra_pot_outliers_catg)

kw.nigra.pot

DT.kw.nigra.pot = dunnTest(mean_time_pot ~ Catg,
                           data = nigra_pot_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.nigra.pot = DT.kw.nigra.pot$res
letters_nigra.pot <- cldList(comparison = DT.kw.nigra.pot$Comparison,
                             p.value    = DT.kw.nigra.pot$P.adj,
                             threshold  = 0.05)


colnames(letters_nigra.pot)[1:2] <- c("Species","pot_letters")

letters_nigra.pot <- letters_nigra.pot[,1:2]
colnames(letters_nigra.pot)[2]<-"P. nigra"

letters_nigra.pot



#sylvestris
sylvestris_pot_outliers_catg <- merge(spain_sel_sylvestris %>% 
                                        rename("Sp.x"="Sp.x.x") %>% 
                                        select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                        select(-Arid_Dm_i), 
                                      by=c("Sp.x", "ID"))

sylvestris_pot_outliers_catg_mean <- sylvestris_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))


kw.sylvestris.pot <- kruskal.test(mean_time_pot ~ Catg,
                                  data = sylvestris_pot_outliers_catg)

kw.sylvestris.pot

DT.kw.sylvestris.pot = dunnTest(mean_time_pot ~ Catg,
                                data = sylvestris_pot_outliers_catg,
                                method="bh")#library(FSA)

DT.kw.sylvestris.pot = DT.kw.sylvestris.pot$res
letters_sylvestris.pot <- cldList(comparison = DT.kw.sylvestris.pot$Comparison,
                                  p.value    = DT.kw.sylvestris.pot$P.adj,
                                  threshold  = 0.05)


colnames(letters_sylvestris.pot)[1:2] <- c("Species","pot_letters")

letters_sylvestris.pot <- letters_sylvestris.pot[,1:2]
colnames(letters_sylvestris.pot)[2]<-"P. sylvestris"

letters_sylvestris.pot




#ilex
ilex_pot_outliers_catg <- merge(spain_sel_ilex %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))


ilex_pot_outliers_catg_mean <- ilex_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))

kw.ilex.pot <- kruskal.test(mean_time_pot ~ Catg,
                            data = ilex_pot_outliers_catg)

kw.ilex.pot

DT.kw.ilex.pot = dunnTest(mean_time_pot ~ Catg,
                          data = ilex_pot_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.ilex.pot = DT.kw.ilex.pot$res
letters_ilex.pot <- cldList(comparison = DT.kw.ilex.pot$Comparison,
                            p.value    = DT.kw.ilex.pot$P.adj,
                            threshold  = 0.05)


colnames(letters_ilex.pot)[1:2] <- c("Species","pot_letters")

letters_ilex.pot <- letters_ilex.pot[,1:2]
colnames(letters_ilex.pot)[2]<-"Q. ilex"

letters_ilex.pot


#suber
suber_pot_outliers_catg <- merge(spain_sel_suber %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pot.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

suber_pot_outliers_catg_mean <- suber_pot_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pot=mean(mean_time_pot))


kw.suber.pot <- kruskal.test(mean_time_pot ~ Catg,
                             data = suber_pot_outliers_catg)

kw.suber.pot

DT.kw.suber.pot = dunnTest(mean_time_pot ~ Catg,
                           data = suber_pot_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.suber.pot = DT.kw.suber.pot$res
letters_suber.pot <- cldList(comparison = DT.kw.suber.pot$Comparison,
                             p.value    = DT.kw.suber.pot$P.adj,
                             threshold  = 0.05)


colnames(letters_suber.pot)[1:2] <- c("Species","pot_letters")

letters_suber.pot <- letters_suber.pot[,1:2]
colnames(letters_suber.pot)[2]<-"Q. suber"

letters_suber.pot

letters_pot <- cbind(letters_eucal.pot, letters_euro.pot[2],
                     letters_sativa.pot[2], letters_halep.pot[2],
                     letters_pinaster.pot[2], letters_pinea.pot[2],
                     letters_nigra.pot[2], letters_sylvestris.pot[2], 
                     letters_ilex.pot[2], letters_suber.pot[2])
letters_pot$metric<- c("pot", "pot","pot")



##########################################################################
##POP

#camaldulensis
eucal_pop_outliers_catg <- merge(spain_sel_camaldulensis %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

eucal_pop_outliers_catg_mean <- eucal_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))


kw.eucal.pop <- kruskal.test(mean_time_pop ~ Catg,
                             data = eucal_pop_outliers_catg)

kw.eucal.pop

DT.kw.eucal.pop = dunnTest(mean_time_pop ~ Catg,
                           data = eucal_pop_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.eucal.pop = DT.kw.eucal.pop$res
letters_eucal.pop <- cldList(comparison = DT.kw.eucal.pop$Comparison,
                             p.value    = DT.kw.eucal.pop$P.adj,
                             threshold  = 0.05)


colnames(letters_eucal.pop)[1:2] <- c("Species","pop_letters")

letters_eucal.pop <- letters_eucal.pop[,1:2]
colnames(letters_eucal.pop)[2]<-"E. camaldulensis"

letters_eucal.pop



#sativa
sativa_pop_outliers_catg <- merge(spain_sel_sativa %>% 
                                    rename("Sp.x"="Sp.x.x") %>% 
                                    select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                    select(-Arid_Dm_i), 
                                  by=c("Sp.x", "ID"))


sativa_pop_outliers_catg_mean <- sativa_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))

kw.sativa.pop <- kruskal.test(mean_time_pop ~ Catg,
                              data = sativa_pop_outliers_catg)

kw.sativa.pop

DT.kw.sativa.pop = dunnTest(mean_time_pop ~ Catg,
                            data = sativa_pop_outliers_catg,
                            method="bh")#library(FSA)

DT.kw.sativa.pop = DT.kw.sativa.pop$res
letters_sativa.pop <- cldList(comparison = DT.kw.sativa.pop$Comparison,
                              p.value    = DT.kw.sativa.pop$P.adj,
                              threshold  = 0.05)


colnames(letters_sativa.pop)[1:2] <- c("Species","pop_letters")

letters_sativa.pop <- letters_sativa.pop[,1:2]
colnames(letters_sativa.pop)[2]<-"C. sativa"

letters_sativa.pop


#europaea
euro_pop_outliers_catg <- merge(spain_sel_europaea %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

euro_pop_outliers_catg_mean <- euro_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))


kw.euro.pop <- kruskal.test(mean_time_pop ~ Catg,
                            data = euro_pop_outliers_catg)

kw.euro.pop

DT.kw.euro.pop = dunnTest(mean_time_pop ~ Catg,
                          data = euro_pop_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.euro.pop = DT.kw.euro.pop$res
letters_euro.pop <- cldList(comparison = DT.kw.euro.pop$Comparison,
                            p.value    = DT.kw.euro.pop$P.adj,
                            threshold  = 0.05)


colnames(letters_euro.pop)[1:2] <- c("Species","pop_letters")

letters_euro.pop <- letters_euro.pop[,1:2]
colnames(letters_euro.pop)[2]<-"C. euro"

letters_euro.pop


#halepensis
halep_pop_outliers_catg <- merge(spain_sel_halep %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

halep_pop_outliers_catg_mean <- halep_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))

kw.halep.pop <- kruskal.test(mean_time_pop ~ Catg,
                             data = halep_pop_outliers_catg)

kw.halep.pop

DT.kw.halep.pop = dunnTest(mean_time_pop ~ Catg,
                           data = halep_pop_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.halep.pop = DT.kw.halep.pop$res
letters_halep.pop <- cldList(comparison = DT.kw.halep.pop$Comparison,
                             p.value    = DT.kw.halep.pop$P.adj,
                             threshold  = 0.05)


colnames(letters_halep.pop)[1:2] <- c("Species","pop_letters")

letters_halep.pop <- letters_halep.pop[,1:2]
colnames(letters_halep.pop)[2]<-"P. halepensis"

letters_halep.pop


#pinaster
pinaster_pop_outliers_catg <- merge(spain_sel_pinaster %>% 
                                      rename("Sp.x"="Sp.x.x") %>% 
                                      select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                      select(-Arid_Dm_i), 
                                    by=c("Sp.x", "ID"))

pinaster_pop_outliers_catg_mean <- pinaster_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))

kw.pinaster.pop <- kruskal.test(mean_time_pop ~ Catg,
                                data = pinaster_pop_outliers_catg)

kw.pinaster.pop

DT.kw.pinaster.pop = dunnTest(mean_time_pop ~ Catg,
                              data = pinaster_pop_outliers_catg,
                              method="bh")#library(FSA)

DT.kw.pinaster.pop = DT.kw.pinaster.pop$res
letters_pinaster.pop <- cldList(comparison = DT.kw.pinaster.pop$Comparison,
                                p.value    = DT.kw.pinaster.pop$P.adj,
                                threshold  = 0.05)


colnames(letters_pinaster.pop)[1:2] <- c("Species","pop_letters")

letters_pinaster.pop <- letters_pinaster.pop[,1:2]
colnames(letters_pinaster.pop)[2]<-"P. pinaster"

letters_pinaster.pop



#pinea
pinea_pop_outliers_catg <- merge(spain_sel_pinea %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

pinea_pop_outliers_catg_mean <- pinea_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))

kw.pinea.pop <- kruskal.test(mean_time_pop ~ Catg,
                             data = pinea_pop_outliers_catg)

kw.pinea.pop

DT.kw.pinea.pop = dunnTest(mean_time_pop ~ Catg,
                           data = pinea_pop_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.pinea.pop = DT.kw.pinea.pop$res
letters_pinea.pop <- cldList(comparison = DT.kw.pinea.pop$Comparison,
                             p.value    = DT.kw.pinea.pop$P.adj,
                             threshold  = 0.05)


colnames(letters_pinea.pop)[1:2] <- c("Species","pop_letters")

letters_pinea.pop <- letters_pinea.pop[,1:2]
colnames(letters_pinea.pop)[2]<-"P. pinea"

letters_pinea.pop



#nigra
nigra_pop_outliers_catg <- merge(spain_sel_nigra %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

nigra_pop_outliers_catg_mean <- nigra_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))


kw.nigra.pop <- kruskal.test(mean_time_pop ~ Catg,
                             data = nigra_pop_outliers_catg)

kw.nigra.pop

DT.kw.nigra.pop = dunnTest(mean_time_pop ~ Catg,
                           data = nigra_pop_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.nigra.pop = DT.kw.nigra.pop$res
letters_nigra.pop <- cldList(comparison = DT.kw.nigra.pop$Comparison,
                             p.value    = DT.kw.nigra.pop$P.adj,
                             threshold  = 0.05)


colnames(letters_nigra.pop)[1:2] <- c("Species","pop_letters")

letters_nigra.pop <- letters_nigra.pop[,1:2]
colnames(letters_nigra.pop)[2]<-"P. nigra"

letters_nigra.pop



#sylvestris
sylvestris_pop_outliers_catg <- merge(spain_sel_sylvestris %>% 
                                        rename("Sp.x"="Sp.x.x") %>% 
                                        select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                        select(-Arid_Dm_i), 
                                      by=c("Sp.x", "ID"))

sylvestris_pop_outliers_catg_mean <- sylvestris_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))


kw.sylvestris.pop <- kruskal.test(mean_time_pop ~ Catg,
                                  data = sylvestris_pop_outliers_catg)

kw.sylvestris.pop

DT.kw.sylvestris.pop = dunnTest(mean_time_pop ~ Catg,
                                data = sylvestris_pop_outliers_catg,
                                method="bh")#library(FSA)

DT.kw.sylvestris.pop = DT.kw.sylvestris.pop$res
letters_sylvestris.pop <- cldList(comparison = DT.kw.sylvestris.pop$Comparison,
                                  p.value    = DT.kw.sylvestris.pop$P.adj,
                                  threshold  = 0.05)


colnames(letters_sylvestris.pop)[1:2] <- c("Species","pop_letters")

letters_sylvestris.pop <- letters_sylvestris.pop[,1:2]
colnames(letters_sylvestris.pop)[2]<-"P. sylvestris"

letters_sylvestris.pop




#ilex
ilex_pop_outliers_catg <- merge(spain_sel_ilex %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))


ilex_pop_outliers_catg_mean <- ilex_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))

kw.ilex.pop <- kruskal.test(mean_time_pop ~ Catg,
                            data = ilex_pop_outliers_catg)

kw.ilex.pop

DT.kw.ilex.pop = dunnTest(mean_time_pop ~ Catg,
                          data = ilex_pop_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.ilex.pop = DT.kw.ilex.pop$res
letters_ilex.pop <- cldList(comparison = DT.kw.ilex.pop$Comparison,
                            p.value    = DT.kw.ilex.pop$P.adj,
                            threshold  = 0.05)


colnames(letters_ilex.pop)[1:2] <- c("Species","pop_letters")

letters_ilex.pop <- letters_ilex.pop[,1:2]
colnames(letters_ilex.pop)[2]<-"Q. ilex"

letters_ilex.pop


#suber
suber_pop_outliers_catg <- merge(spain_sel_suber %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.pop.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

suber_pop_outliers_catg_mean <- suber_pop_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_pop=mean(mean_time_pop))


kw.suber.pop <- kruskal.test(mean_time_pop ~ Catg,
                             data = suber_pop_outliers_catg)

kw.suber.pop

DT.kw.suber.pop = dunnTest(mean_time_pop ~ Catg,
                           data = suber_pop_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.suber.pop = DT.kw.suber.pop$res
letters_suber.pop <- cldList(comparison = DT.kw.suber.pop$Comparison,
                             p.value    = DT.kw.suber.pop$P.adj,
                             threshold  = 0.05)


colnames(letters_suber.pop)[1:2] <- c("Species","pop_letters")

letters_suber.pop <- letters_suber.pop[,1:2]
colnames(letters_suber.pop)[2]<-"Q. suber"

letters_suber.pop

letters_pop <- cbind(letters_eucal.pop, letters_euro.pop[2],
                     letters_sativa.pop[2], letters_halep.pop[2],
                     letters_pinaster.pop[2], letters_pinea.pop[2],
                     letters_nigra.pop[2], letters_sylvestris.pop[2], 
                     letters_ilex.pop[2], letters_suber.pop[2])
letters_pop$metric<- c("pop", "pop","pop")



####################################################################
##PEAK

#camaldulensis
eucal_peak_outliers_catg <- merge(spain_sel_camaldulensis %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

eucal_peak_outliers_catg_mean <- eucal_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))


kw.eucal.peak <- kruskal.test(mean_time_peak ~ Catg,
                             data = eucal_peak_outliers_catg)

kw.eucal.peak

DT.kw.eucal.peak = dunnTest(mean_time_peak ~ Catg,
                           data = eucal_peak_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.eucal.peak = DT.kw.eucal.peak$res
letters_eucal.peak <- cldList(comparison = DT.kw.eucal.peak$Comparison,
                             p.value    = DT.kw.eucal.peak$P.adj,
                             threshold  = 0.05)


colnames(letters_eucal.peak)[1:2] <- c("Species","peak_letters")

letters_eucal.peak <- letters_eucal.peak[,1:2]
colnames(letters_eucal.peak)[2]<-"E. camaldulensis"

letters_eucal.peak



#sativa
sativa_peak_outliers_catg <- merge(spain_sel_sativa %>% 
                                    rename("Sp.x"="Sp.x.x") %>% 
                                    select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                    select(-Arid_Dm_i), 
                                  by=c("Sp.x", "ID"))


sativa_peak_outliers_catg_mean <- sativa_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))

kw.sativa.peak <- kruskal.test(mean_time_peak ~ Catg,
                              data = sativa_peak_outliers_catg)

kw.sativa.peak

DT.kw.sativa.peak = dunnTest(mean_time_peak ~ Catg,
                            data = sativa_peak_outliers_catg,
                            method="bh")#library(FSA)

DT.kw.sativa.peak = DT.kw.sativa.peak$res
letters_sativa.peak <- cldList(comparison = DT.kw.sativa.peak$Comparison,
                              p.value    = DT.kw.sativa.peak$P.adj,
                              threshold  = 0.05)


colnames(letters_sativa.peak)[1:2] <- c("Species","peak_letters")

letters_sativa.peak <- letters_sativa.peak[,1:2]
colnames(letters_sativa.peak)[2]<-"C. sativa"

letters_sativa.peak


#europaea
euro_peak_outliers_catg <- merge(spain_sel_europaea %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

euro_peak_outliers_catg_mean <- euro_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))


kw.euro.peak <- kruskal.test(mean_time_peak ~ Catg,
                            data = euro_peak_outliers_catg)

kw.euro.peak

DT.kw.euro.peak = dunnTest(mean_time_peak ~ Catg,
                          data = euro_peak_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.euro.peak = DT.kw.euro.peak$res
letters_euro.peak <- cldList(comparison = DT.kw.euro.peak$Comparison,
                            p.value    = DT.kw.euro.peak$P.adj,
                            threshold  = 0.05)


colnames(letters_euro.peak)[1:2] <- c("Species","peak_letters")

letters_euro.peak <- letters_euro.peak[,1:2]
colnames(letters_euro.peak)[2]<-"C. euro"

letters_euro.peak


#halepensis
halep_peak_outliers_catg <- merge(spain_sel_halep %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

halep_peak_outliers_catg_mean <- halep_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))

kw.halep.peak <- kruskal.test(mean_time_peak ~ Catg,
                             data = halep_peak_outliers_catg)

kw.halep.peak

DT.kw.halep.peak = dunnTest(mean_time_peak ~ Catg,
                           data = halep_peak_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.halep.peak = DT.kw.halep.peak$res
letters_halep.peak <- cldList(comparison = DT.kw.halep.peak$Comparison,
                             p.value    = DT.kw.halep.peak$P.adj,
                             threshold  = 0.05)


colnames(letters_halep.peak)[1:2] <- c("Species","peak_letters")

letters_halep.peak <- letters_halep.peak[,1:2]
colnames(letters_halep.peak)[2]<-"P. halepensis"

letters_halep.peak


#pinaster
pinaster_peak_outliers_catg <- merge(spain_sel_pinaster %>% 
                                      rename("Sp.x"="Sp.x.x") %>% 
                                      select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                      select(-Arid_Dm_i), 
                                    by=c("Sp.x", "ID"))

pinaster_peak_outliers_catg_mean <- pinaster_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))

kw.pinaster.peak <- kruskal.test(mean_time_peak ~ Catg,
                                data = pinaster_peak_outliers_catg)

kw.pinaster.peak

DT.kw.pinaster.peak = dunnTest(mean_time_peak ~ Catg,
                              data = pinaster_peak_outliers_catg,
                              method="bh")#library(FSA)

DT.kw.pinaster.peak = DT.kw.pinaster.peak$res
letters_pinaster.peak <- cldList(comparison = DT.kw.pinaster.peak$Comparison,
                                p.value    = DT.kw.pinaster.peak$P.adj,
                                threshold  = 0.05)


colnames(letters_pinaster.peak)[1:2] <- c("Species","peak_letters")

letters_pinaster.peak <- letters_pinaster.peak[,1:2]
colnames(letters_pinaster.peak)[2]<-"P. pinaster"

letters_pinaster.peak



#pinea
pinea_peak_outliers_catg <- merge(spain_sel_pinea %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

pinea_peak_outliers_catg_mean <- pinea_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))

kw.pinea.peak <- kruskal.test(mean_time_peak ~ Catg,
                             data = pinea_peak_outliers_catg)

kw.pinea.peak

DT.kw.pinea.peak = dunnTest(mean_time_peak ~ Catg,
                           data = pinea_peak_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.pinea.peak = DT.kw.pinea.peak$res
letters_pinea.peak <- cldList(comparison = DT.kw.pinea.peak$Comparison,
                             p.value    = DT.kw.pinea.peak$P.adj,
                             threshold  = 0.05)


colnames(letters_pinea.peak)[1:2] <- c("Species","peak_letters")

letters_pinea.peak <- letters_pinea.peak[,1:2]
colnames(letters_pinea.peak)[2]<-"P. pinea"

letters_pinea.peak



#nigra
nigra_peak_outliers_catg <- merge(spain_sel_nigra %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

nigra_peak_outliers_catg_mean <- nigra_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))


kw.nigra.peak <- kruskal.test(mean_time_peak ~ Catg,
                             data = nigra_peak_outliers_catg)

kw.nigra.peak

DT.kw.nigra.peak = dunnTest(mean_time_peak ~ Catg,
                           data = nigra_peak_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.nigra.peak = DT.kw.nigra.peak$res
letters_nigra.peak <- cldList(comparison = DT.kw.nigra.peak$Comparison,
                             p.value    = DT.kw.nigra.peak$P.adj,
                             threshold  = 0.05)


colnames(letters_nigra.peak)[1:2] <- c("Species","peak_letters")

letters_nigra.peak <- letters_nigra.peak[,1:2]
colnames(letters_nigra.peak)[2]<-"P. nigra"

letters_nigra.peak



#sylvestris
sylvestris_peak_outliers_catg <- merge(spain_sel_sylvestris %>% 
                                        rename("Sp.x"="Sp.x.x") %>% 
                                        select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                        select(-Arid_Dm_i), 
                                      by=c("Sp.x", "ID"))

sylvestris_peak_outliers_catg_mean <- sylvestris_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))


kw.sylvestris.peak <- kruskal.test(mean_time_peak ~ Catg,
                                  data = sylvestris_peak_outliers_catg)

kw.sylvestris.peak

DT.kw.sylvestris.peak = dunnTest(mean_time_peak ~ Catg,
                                data = sylvestris_peak_outliers_catg,
                                method="bh")#library(FSA)

DT.kw.sylvestris.peak = DT.kw.sylvestris.peak$res
letters_sylvestris.peak <- cldList(comparison = DT.kw.sylvestris.peak$Comparison,
                                  p.value    = DT.kw.sylvestris.peak$P.adj,
                                  threshold  = 0.05)


colnames(letters_sylvestris.peak)[1:2] <- c("Species","peak_letters")

letters_sylvestris.peak <- letters_sylvestris.peak[,1:2]
colnames(letters_sylvestris.peak)[2]<-"P. sylvestris"

letters_sylvestris.peak




#ilex
ilex_peak_outliers_catg <- merge(spain_sel_ilex %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))


ilex_peak_outliers_catg_mean <- ilex_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))

kw.ilex.peak <- kruskal.test(mean_time_peak ~ Catg,
                            data = ilex_peak_outliers_catg)

kw.ilex.peak

DT.kw.ilex.peak = dunnTest(mean_time_peak ~ Catg,
                          data = ilex_peak_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.ilex.peak = DT.kw.ilex.peak$res
letters_ilex.peak <- cldList(comparison = DT.kw.ilex.peak$Comparison,
                            p.value    = DT.kw.ilex.peak$P.adj,
                            threshold  = 0.05)


colnames(letters_ilex.peak)[1:2] <- c("Species","peak_letters")

letters_ilex.peak <- letters_ilex.peak[,1:2]
colnames(letters_ilex.peak)[2]<-"Q. ilex"

letters_ilex.peak


#suber
suber_peak_outliers_catg <- merge(spain_sel_suber %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.peak.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

suber_peak_outliers_catg_mean <- suber_peak_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_peak=mean(mean_time_peak))


kw.suber.peak <- kruskal.test(mean_time_peak ~ Catg,
                             data = suber_peak_outliers_catg)

kw.suber.peak

DT.kw.suber.peak = dunnTest(mean_time_peak ~ Catg,
                           data = suber_peak_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.suber.peak = DT.kw.suber.peak$res
letters_suber.peak <- cldList(comparison = DT.kw.suber.peak$Comparison,
                             p.value    = DT.kw.suber.peak$P.adj,
                             threshold  = 0.05)


colnames(letters_suber.peak)[1:2] <- c("Species","peak_letters")

letters_suber.peak <- letters_suber.peak[,1:2]
colnames(letters_suber.peak)[2]<-"Q. suber"

letters_suber.peak

letters_peak <- cbind(letters_eucal.peak, letters_euro.peak[2],
                     letters_sativa.peak[2], letters_halep.peak[2],
                     letters_pinaster.peak[2], letters_pinea.peak[2],
                     letters_nigra.peak[2], letters_sylvestris.peak[2], 
                     letters_ilex.peak[2], letters_suber.peak[2])
letters_peak$metric<- c("peak", "peak","peak")




#######################################################################
##TROUGH

#camaldulensis
eucal_trough_outliers_catg <- merge(spain_sel_camaldulensis %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

eucal_trough_outliers_catg_mean <- eucal_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))


kw.eucal.trough <- kruskal.test(mean_time_trough ~ Catg,
                             data = eucal_trough_outliers_catg)

kw.eucal.trough

DT.kw.eucal.trough = dunnTest(mean_time_trough ~ Catg,
                           data = eucal_trough_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.eucal.trough = DT.kw.eucal.trough$res
letters_eucal.trough <- cldList(comparison = DT.kw.eucal.trough$Comparison,
                             p.value    = DT.kw.eucal.trough$P.adj,
                             threshold  = 0.05)


colnames(letters_eucal.trough)[1:2] <- c("Species","trough_letters")

letters_eucal.trough <- letters_eucal.trough[,1:2]
colnames(letters_eucal.trough)[2]<-"E. camaldulensis"

letters_eucal.trough



#sativa
sativa_trough_outliers_catg <- merge(spain_sel_sativa %>% 
                                    rename("Sp.x"="Sp.x.x") %>% 
                                    select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                    select(-Arid_Dm_i), 
                                  by=c("Sp.x", "ID"))


sativa_trough_outliers_catg_mean <- sativa_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))

kw.sativa.trough <- kruskal.test(mean_time_trough ~ Catg,
                              data = sativa_trough_outliers_catg)

kw.sativa.trough

DT.kw.sativa.trough = dunnTest(mean_time_trough ~ Catg,
                            data = sativa_trough_outliers_catg,
                            method="bh")#library(FSA)

DT.kw.sativa.trough = DT.kw.sativa.trough$res
letters_sativa.trough <- cldList(comparison = DT.kw.sativa.trough$Comparison,
                              p.value    = DT.kw.sativa.trough$P.adj,
                              threshold  = 0.05)


colnames(letters_sativa.trough)[1:2] <- c("Species","trough_letters")

letters_sativa.trough <- letters_sativa.trough[,1:2]
colnames(letters_sativa.trough)[2]<-"C. sativa"

letters_sativa.trough


#europaea
euro_trough_outliers_catg <- merge(spain_sel_europaea %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

euro_trough_outliers_catg_mean <- euro_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))


kw.euro.trough <- kruskal.test(mean_time_trough ~ Catg,
                            data = euro_trough_outliers_catg)

kw.euro.trough

DT.kw.euro.trough = dunnTest(mean_time_trough ~ Catg,
                          data = euro_trough_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.euro.trough = DT.kw.euro.trough$res
letters_euro.trough <- cldList(comparison = DT.kw.euro.trough$Comparison,
                            p.value    = DT.kw.euro.trough$P.adj,
                            threshold  = 0.05)


colnames(letters_euro.trough)[1:2] <- c("Species","trough_letters")

letters_euro.trough <- letters_euro.trough[,1:2]
colnames(letters_euro.trough)[2]<-"C. euro"

letters_euro.trough


#halepensis
halep_trough_outliers_catg <- merge(spain_sel_halep %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

halep_trough_outliers_catg_mean <- halep_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))

kw.halep.trough <- kruskal.test(mean_time_trough ~ Catg,
                             data = halep_trough_outliers_catg)

kw.halep.trough

DT.kw.halep.trough = dunnTest(mean_time_trough ~ Catg,
                           data = halep_trough_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.halep.trough = DT.kw.halep.trough$res
letters_halep.trough <- cldList(comparison = DT.kw.halep.trough$Comparison,
                             p.value    = DT.kw.halep.trough$P.adj,
                             threshold  = 0.05)


colnames(letters_halep.trough)[1:2] <- c("Species","trough_letters")

letters_halep.trough <- letters_halep.trough[,1:2]
colnames(letters_halep.trough)[2]<-"P. halepensis"

letters_halep.trough


#pinaster
pinaster_trough_outliers_catg <- merge(spain_sel_pinaster %>% 
                                      rename("Sp.x"="Sp.x.x") %>% 
                                      select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                      select(-Arid_Dm_i), 
                                    by=c("Sp.x", "ID"))

pinaster_trough_outliers_catg_mean <- pinaster_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))

kw.pinaster.trough <- kruskal.test(mean_time_trough ~ Catg,
                                data = pinaster_trough_outliers_catg)

kw.pinaster.trough

DT.kw.pinaster.trough = dunnTest(mean_time_trough ~ Catg,
                              data = pinaster_trough_outliers_catg,
                              method="bh")#library(FSA)

DT.kw.pinaster.trough = DT.kw.pinaster.trough$res
letters_pinaster.trough <- cldList(comparison = DT.kw.pinaster.trough$Comparison,
                                p.value    = DT.kw.pinaster.trough$P.adj,
                                threshold  = 0.05)


colnames(letters_pinaster.trough)[1:2] <- c("Species","trough_letters")

letters_pinaster.trough <- letters_pinaster.trough[,1:2]
colnames(letters_pinaster.trough)[2]<-"P. pinaster"

letters_pinaster.trough



#pinea
pinea_trough_outliers_catg <- merge(spain_sel_pinea %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

pinea_trough_outliers_catg_mean <- pinea_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))

kw.pinea.trough <- kruskal.test(mean_time_trough ~ Catg,
                             data = pinea_trough_outliers_catg)

kw.pinea.trough

DT.kw.pinea.trough = dunnTest(mean_time_trough ~ Catg,
                           data = pinea_trough_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.pinea.trough = DT.kw.pinea.trough$res
letters_pinea.trough <- cldList(comparison = DT.kw.pinea.trough$Comparison,
                             p.value    = DT.kw.pinea.trough$P.adj,
                             threshold  = 0.05)


colnames(letters_pinea.trough)[1:2] <- c("Species","trough_letters")

letters_pinea.trough <- letters_pinea.trough[,1:2]
colnames(letters_pinea.trough)[2]<-"P. pinea"

letters_pinea.trough



#nigra
nigra_trough_outliers_catg <- merge(spain_sel_nigra %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

nigra_trough_outliers_catg_mean <- nigra_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))


kw.nigra.trough <- kruskal.test(mean_time_trough ~ Catg,
                             data = nigra_trough_outliers_catg)

kw.nigra.trough

DT.kw.nigra.trough = dunnTest(mean_time_trough ~ Catg,
                           data = nigra_trough_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.nigra.trough = DT.kw.nigra.trough$res
letters_nigra.trough <- cldList(comparison = DT.kw.nigra.trough$Comparison,
                             p.value    = DT.kw.nigra.trough$P.adj,
                             threshold  = 0.05)


colnames(letters_nigra.trough)[1:2] <- c("Species","trough_letters")

letters_nigra.trough <- letters_nigra.trough[,1:2]
colnames(letters_nigra.trough)[2]<-"P. nigra"

letters_nigra.trough



#sylvestris
sylvestris_trough_outliers_catg <- merge(spain_sel_sylvestris %>% 
                                        rename("Sp.x"="Sp.x.x") %>% 
                                        select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                        select(-Arid_Dm_i), 
                                      by=c("Sp.x", "ID"))

sylvestris_trough_outliers_catg_mean <- sylvestris_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))


kw.sylvestris.trough <- kruskal.test(mean_time_trough ~ Catg,
                                  data = sylvestris_trough_outliers_catg)

kw.sylvestris.trough

DT.kw.sylvestris.trough = dunnTest(mean_time_trough ~ Catg,
                                data = sylvestris_trough_outliers_catg,
                                method="bh")#library(FSA)

DT.kw.sylvestris.trough = DT.kw.sylvestris.trough$res
letters_sylvestris.trough <- cldList(comparison = DT.kw.sylvestris.trough$Comparison,
                                  p.value    = DT.kw.sylvestris.trough$P.adj,
                                  threshold  = 0.05)


colnames(letters_sylvestris.trough)[1:2] <- c("Species","trough_letters")

letters_sylvestris.trough <- letters_sylvestris.trough[,1:2]
colnames(letters_sylvestris.trough)[2]<-"P. sylvestris"

letters_sylvestris.trough




#ilex
ilex_trough_outliers_catg <- merge(spain_sel_ilex %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))


ilex_trough_outliers_catg_mean <- ilex_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))

kw.ilex.trough <- kruskal.test(mean_time_trough ~ Catg,
                            data = ilex_trough_outliers_catg)

kw.ilex.trough

DT.kw.ilex.trough = dunnTest(mean_time_trough ~ Catg,
                          data = ilex_trough_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.ilex.trough = DT.kw.ilex.trough$res
letters_ilex.trough <- cldList(comparison = DT.kw.ilex.trough$Comparison,
                            p.value    = DT.kw.ilex.trough$P.adj,
                            threshold  = 0.05)


colnames(letters_ilex.trough)[1:2] <- c("Species","trough_letters")

letters_ilex.trough <- letters_ilex.trough[,1:2]
colnames(letters_ilex.trough)[2]<-"Q. ilex"

letters_ilex.trough


#suber
suber_trough_outliers_catg <- merge(spain_sel_suber %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.trough.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

suber_trough_outliers_catg_mean <- suber_trough_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_trough=mean(mean_time_trough))


kw.suber.trough <- kruskal.test(mean_time_trough ~ Catg,
                             data = suber_trough_outliers_catg)

kw.suber.trough

DT.kw.suber.trough = dunnTest(mean_time_trough ~ Catg,
                           data = suber_trough_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.suber.trough = DT.kw.suber.trough$res
letters_suber.trough <- cldList(comparison = DT.kw.suber.trough$Comparison,
                             p.value    = DT.kw.suber.trough$P.adj,
                             threshold  = 0.05)


colnames(letters_suber.trough)[1:2] <- c("Species","trough_letters")

letters_suber.trough <- letters_suber.trough[,1:2]
colnames(letters_suber.trough)[2]<-"Q. suber"

letters_suber.trough

letters_trough <- cbind(letters_eucal.trough, letters_euro.trough[2],
                     letters_sativa.trough[2], letters_halep.trough[2],
                     letters_pinaster.trough[2], letters_pinea.trough[2],
                     letters_nigra.trough[2], letters_sylvestris.trough[2], 
                     letters_ilex.trough[2], letters_suber.trough[2])
letters_trough$metric<- c("trough", "trough","trough")



####################################################################
##MSP
#camaldulensis
eucal_msp_outliers_catg <- merge(spain_sel_camaldulensis %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

eucal_msp_outliers_catg_mean <- eucal_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))


kw.eucal.msp <- kruskal.test(mean_time_msp ~ Catg,
                             data = eucal_msp_outliers_catg)

kw.eucal.msp

DT.kw.eucal.msp = dunnTest(mean_time_msp ~ Catg,
                           data = eucal_msp_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.eucal.msp = DT.kw.eucal.msp$res
letters_eucal.msp <- cldList(comparison = DT.kw.eucal.msp$Comparison,
                             p.value    = DT.kw.eucal.msp$P.adj,
                             threshold  = 0.05)


colnames(letters_eucal.msp)[1:2] <- c("Species","msp_letters")

letters_eucal.msp <- letters_eucal.msp[,1:2]
colnames(letters_eucal.msp)[2]<-"E. camaldulensis"

letters_eucal.msp



#sativa
sativa_msp_outliers_catg <- merge(spain_sel_sativa %>% 
                                    rename("Sp.x"="Sp.x.x") %>% 
                                    select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                    select(-Arid_Dm_i), 
                                  by=c("Sp.x", "ID"))


sativa_msp_outliers_catg_mean <- sativa_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))

kw.sativa.msp <- kruskal.test(mean_time_msp ~ Catg,
                              data = sativa_msp_outliers_catg)

kw.sativa.msp

DT.kw.sativa.msp = dunnTest(mean_time_msp ~ Catg,
                            data = sativa_msp_outliers_catg,
                            method="bh")#library(FSA)

DT.kw.sativa.msp = DT.kw.sativa.msp$res
letters_sativa.msp <- cldList(comparison = DT.kw.sativa.msp$Comparison,
                              p.value    = DT.kw.sativa.msp$P.adj,
                              threshold  = 0.05)


colnames(letters_sativa.msp)[1:2] <- c("Species","msp_letters")

letters_sativa.msp <- letters_sativa.msp[,1:2]
colnames(letters_sativa.msp)[2]<-"C. sativa"

letters_sativa.msp


#europaea
euro_msp_outliers_catg <- merge(spain_sel_europaea %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

euro_msp_outliers_catg_mean <- euro_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))


kw.euro.msp <- kruskal.test(mean_time_msp ~ Catg,
                            data = euro_msp_outliers_catg)

kw.euro.msp

DT.kw.euro.msp = dunnTest(mean_time_msp ~ Catg,
                          data = euro_msp_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.euro.msp = DT.kw.euro.msp$res
letters_euro.msp <- cldList(comparison = DT.kw.euro.msp$Comparison,
                            p.value    = DT.kw.euro.msp$P.adj,
                            threshold  = 0.05)


colnames(letters_euro.msp)[1:2] <- c("Species","msp_letters")

letters_euro.msp <- letters_euro.msp[,1:2]
colnames(letters_euro.msp)[2]<-"C. euro"

letters_euro.msp


#halepensis
halep_msp_outliers_catg <- merge(spain_sel_halep %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

halep_msp_outliers_catg_mean <- halep_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))

kw.halep.msp <- kruskal.test(mean_time_msp ~ Catg,
                             data = halep_msp_outliers_catg)

kw.halep.msp

DT.kw.halep.msp = dunnTest(mean_time_msp ~ Catg,
                           data = halep_msp_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.halep.msp = DT.kw.halep.msp$res
letters_halep.msp <- cldList(comparison = DT.kw.halep.msp$Comparison,
                             p.value    = DT.kw.halep.msp$P.adj,
                             threshold  = 0.05)


colnames(letters_halep.msp)[1:2] <- c("Species","msp_letters")

letters_halep.msp <- letters_halep.msp[,1:2]
colnames(letters_halep.msp)[2]<-"P. halepensis"

letters_halep.msp


#pinaster
pinaster_msp_outliers_catg <- merge(spain_sel_pinaster %>% 
                                      rename("Sp.x"="Sp.x.x") %>% 
                                      select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                      select(-Arid_Dm_i), 
                                    by=c("Sp.x", "ID"))

pinaster_msp_outliers_catg_mean <- pinaster_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))

kw.pinaster.msp <- kruskal.test(mean_time_msp ~ Catg,
                                data = pinaster_msp_outliers_catg)

kw.pinaster.msp

DT.kw.pinaster.msp = dunnTest(mean_time_msp ~ Catg,
                              data = pinaster_msp_outliers_catg,
                              method="bh")#library(FSA)

DT.kw.pinaster.msp = DT.kw.pinaster.msp$res
letters_pinaster.msp <- cldList(comparison = DT.kw.pinaster.msp$Comparison,
                                p.value    = DT.kw.pinaster.msp$P.adj,
                                threshold  = 0.05)


colnames(letters_pinaster.msp)[1:2] <- c("Species","msp_letters")

letters_pinaster.msp <- letters_pinaster.msp[,1:2]
colnames(letters_pinaster.msp)[2]<-"P. pinaster"

letters_pinaster.msp



#pinea
pinea_msp_outliers_catg <- merge(spain_sel_pinea %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

pinea_msp_outliers_catg_mean <- pinea_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))

kw.pinea.msp <- kruskal.test(mean_time_msp ~ Catg,
                             data = pinea_msp_outliers_catg)

kw.pinea.msp

DT.kw.pinea.msp = dunnTest(mean_time_msp ~ Catg,
                           data = pinea_msp_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.pinea.msp = DT.kw.pinea.msp$res
letters_pinea.msp <- cldList(comparison = DT.kw.pinea.msp$Comparison,
                             p.value    = DT.kw.pinea.msp$P.adj,
                             threshold  = 0.05)


colnames(letters_pinea.msp)[1:2] <- c("Species","msp_letters")

letters_pinea.msp <- letters_pinea.msp[,1:2]
colnames(letters_pinea.msp)[2]<-"P. pinea"

letters_pinea.msp



#nigra
nigra_msp_outliers_catg <- merge(spain_sel_nigra %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

nigra_msp_outliers_catg_mean <- nigra_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))


kw.nigra.msp <- kruskal.test(mean_time_msp ~ Catg,
                             data = nigra_msp_outliers_catg)

kw.nigra.msp

DT.kw.nigra.msp = dunnTest(mean_time_msp ~ Catg,
                           data = nigra_msp_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.nigra.msp = DT.kw.nigra.msp$res
letters_nigra.msp <- cldList(comparison = DT.kw.nigra.msp$Comparison,
                             p.value    = DT.kw.nigra.msp$P.adj,
                             threshold  = 0.05)


colnames(letters_nigra.msp)[1:2] <- c("Species","msp_letters")

letters_nigra.msp <- letters_nigra.msp[,1:2]
colnames(letters_nigra.msp)[2]<-"P. nigra"

letters_nigra.msp



#sylvestris
sylvestris_msp_outliers_catg <- merge(spain_sel_sylvestris %>% 
                                        rename("Sp.x"="Sp.x.x") %>% 
                                        select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                        select(-Arid_Dm_i), 
                                      by=c("Sp.x", "ID"))

sylvestris_msp_outliers_catg_mean <- sylvestris_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))


kw.sylvestris.msp <- kruskal.test(mean_time_msp ~ Catg,
                                  data = sylvestris_msp_outliers_catg)

kw.sylvestris.msp

DT.kw.sylvestris.msp = dunnTest(mean_time_msp ~ Catg,
                                data = sylvestris_msp_outliers_catg,
                                method="bh")#library(FSA)

DT.kw.sylvestris.msp = DT.kw.sylvestris.msp$res
letters_sylvestris.msp <- cldList(comparison = DT.kw.sylvestris.msp$Comparison,
                                  p.value    = DT.kw.sylvestris.msp$P.adj,
                                  threshold  = 0.05)


colnames(letters_sylvestris.msp)[1:2] <- c("Species","msp_letters")

letters_sylvestris.msp <- letters_sylvestris.msp[,1:2]
colnames(letters_sylvestris.msp)[2]<-"P. sylvestris"

letters_sylvestris.msp




#ilex
ilex_msp_outliers_catg <- merge(spain_sel_ilex %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))


ilex_msp_outliers_catg_mean <- ilex_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))

kw.ilex.msp <- kruskal.test(mean_time_msp ~ Catg,
                            data = ilex_msp_outliers_catg)

kw.ilex.msp

DT.kw.ilex.msp = dunnTest(mean_time_msp ~ Catg,
                          data = ilex_msp_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.ilex.msp = DT.kw.ilex.msp$res
letters_ilex.msp <- cldList(comparison = DT.kw.ilex.msp$Comparison,
                            p.value    = DT.kw.ilex.msp$P.adj,
                            threshold  = 0.05)


colnames(letters_ilex.msp)[1:2] <- c("Species","msp_letters")

letters_ilex.msp <- letters_ilex.msp[,1:2]
colnames(letters_ilex.msp)[2]<-"Q. ilex"

letters_ilex.msp


#suber
suber_msp_outliers_catg <- merge(spain_sel_suber %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.msp.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

suber_msp_outliers_catg_mean <- suber_msp_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_msp=mean(mean_time_msp))


kw.suber.msp <- kruskal.test(mean_time_msp ~ Catg,
                             data = suber_msp_outliers_catg)

kw.suber.msp

DT.kw.suber.msp = dunnTest(mean_time_msp ~ Catg,
                           data = suber_msp_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.suber.msp = DT.kw.suber.msp$res
letters_suber.msp <- cldList(comparison = DT.kw.suber.msp$Comparison,
                             p.value    = DT.kw.suber.msp$P.adj,
                             threshold  = 0.05)


colnames(letters_suber.msp)[1:2] <- c("Species","msp_letters")

letters_suber.msp <- letters_suber.msp[,1:2]
colnames(letters_suber.msp)[2]<-"Q. suber"

letters_suber.msp

letters_msp <- cbind(letters_eucal.msp, letters_euro.msp[2],
                     letters_sativa.msp[2], letters_halep.msp[2],
                     letters_pinaster.msp[2], letters_pinea.msp[2],
                     letters_nigra.msp[2], letters_sylvestris.msp[2], 
                     letters_ilex.msp[2], letters_suber.msp[2])
letters_msp$metric<- c("msp", "msp","msp")



#############################################################################
##MAU

#camaldulensis
eucal_mau_outliers_catg <- merge(spain_sel_camaldulensis %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

eucal_mau_outliers_catg_mean <- eucal_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))


kw.eucal.mau <- kruskal.test(mean_time_mau ~ Catg,
                             data = eucal_mau_outliers_catg)

kw.eucal.mau

DT.kw.eucal.mau = dunnTest(mean_time_mau ~ Catg,
                           data = eucal_mau_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.eucal.mau = DT.kw.eucal.mau$res
letters_eucal.mau <- cldList(comparison = DT.kw.eucal.mau$Comparison,
                             p.value    = DT.kw.eucal.mau$P.adj,
                             threshold  = 0.05)


colnames(letters_eucal.mau)[1:2] <- c("Species","mau_letters")

letters_eucal.mau <- letters_eucal.mau[,1:2]
colnames(letters_eucal.mau)[2]<-"E. camaldulensis"

letters_eucal.mau



#sativa
sativa_mau_outliers_catg <- merge(spain_sel_sativa %>% 
                                    rename("Sp.x"="Sp.x.x") %>% 
                                    select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                    select(-Arid_Dm_i), 
                                  by=c("Sp.x", "ID"))


sativa_mau_outliers_catg_mean <- sativa_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))

kw.sativa.mau <- kruskal.test(mean_time_mau ~ Catg,
                              data = sativa_mau_outliers_catg)

kw.sativa.mau

DT.kw.sativa.mau = dunnTest(mean_time_mau ~ Catg,
                            data = sativa_mau_outliers_catg,
                            method="bh")#library(FSA)

DT.kw.sativa.mau = DT.kw.sativa.mau$res
letters_sativa.mau <- cldList(comparison = DT.kw.sativa.mau$Comparison,
                              p.value    = DT.kw.sativa.mau$P.adj,
                              threshold  = 0.05)


colnames(letters_sativa.mau)[1:2] <- c("Species","mau_letters")

letters_sativa.mau <- letters_sativa.mau[,1:2]
colnames(letters_sativa.mau)[2]<-"C. sativa"

letters_sativa.mau


#europaea
euro_mau_outliers_catg <- merge(spain_sel_europaea %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))

euro_mau_outliers_catg_mean <- euro_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))


kw.euro.mau <- kruskal.test(mean_time_mau ~ Catg,
                            data = euro_mau_outliers_catg)

kw.euro.mau

DT.kw.euro.mau = dunnTest(mean_time_mau ~ Catg,
                          data = euro_mau_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.euro.mau = DT.kw.euro.mau$res
letters_euro.mau <- cldList(comparison = DT.kw.euro.mau$Comparison,
                            p.value    = DT.kw.euro.mau$P.adj,
                            threshold  = 0.05)


colnames(letters_euro.mau)[1:2] <- c("Species","mau_letters")

letters_euro.mau <- letters_euro.mau[,1:2]
colnames(letters_euro.mau)[2]<-"C. euro"

letters_euro.mau


#halepensis
halep_mau_outliers_catg <- merge(spain_sel_halep %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

halep_mau_outliers_catg_mean <- halep_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))

kw.halep.mau <- kruskal.test(mean_time_mau ~ Catg,
                             data = halep_mau_outliers_catg)

kw.halep.mau

DT.kw.halep.mau = dunnTest(mean_time_mau ~ Catg,
                           data = halep_mau_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.halep.mau = DT.kw.halep.mau$res
letters_halep.mau <- cldList(comparison = DT.kw.halep.mau$Comparison,
                             p.value    = DT.kw.halep.mau$P.adj,
                             threshold  = 0.05)


colnames(letters_halep.mau)[1:2] <- c("Species","mau_letters")

letters_halep.mau <- letters_halep.mau[,1:2]
colnames(letters_halep.mau)[2]<-"P. halepensis"

letters_halep.mau


#pinaster
pinaster_mau_outliers_catg <- merge(spain_sel_pinaster %>% 
                                      rename("Sp.x"="Sp.x.x") %>% 
                                      select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                      select(-Arid_Dm_i), 
                                    by=c("Sp.x", "ID"))

pinaster_mau_outliers_catg_mean <- pinaster_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))

kw.pinaster.mau <- kruskal.test(mean_time_mau ~ Catg,
                                data = pinaster_mau_outliers_catg)

kw.pinaster.mau

DT.kw.pinaster.mau = dunnTest(mean_time_mau ~ Catg,
                              data = pinaster_mau_outliers_catg,
                              method="bh")#library(FSA)

DT.kw.pinaster.mau = DT.kw.pinaster.mau$res
letters_pinaster.mau <- cldList(comparison = DT.kw.pinaster.mau$Comparison,
                                p.value    = DT.kw.pinaster.mau$P.adj,
                                threshold  = 0.05)


colnames(letters_pinaster.mau)[1:2] <- c("Species","mau_letters")

letters_pinaster.mau <- letters_pinaster.mau[,1:2]
colnames(letters_pinaster.mau)[2]<-"P. pinaster"

letters_pinaster.mau



#pinea
pinea_mau_outliers_catg <- merge(spain_sel_pinea %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

pinea_mau_outliers_catg_mean <- pinea_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))

kw.pinea.mau <- kruskal.test(mean_time_mau ~ Catg,
                             data = pinea_mau_outliers_catg)

kw.pinea.mau

DT.kw.pinea.mau = dunnTest(mean_time_mau ~ Catg,
                           data = pinea_mau_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.pinea.mau = DT.kw.pinea.mau$res
letters_pinea.mau <- cldList(comparison = DT.kw.pinea.mau$Comparison,
                             p.value    = DT.kw.pinea.mau$P.adj,
                             threshold  = 0.05)


colnames(letters_pinea.mau)[1:2] <- c("Species","mau_letters")

letters_pinea.mau <- letters_pinea.mau[,1:2]
colnames(letters_pinea.mau)[2]<-"P. pinea"

letters_pinea.mau



#nigra
nigra_mau_outliers_catg <- merge(spain_sel_nigra %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

nigra_mau_outliers_catg_mean <- nigra_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))


kw.nigra.mau <- kruskal.test(mean_time_mau ~ Catg,
                             data = nigra_mau_outliers_catg)

kw.nigra.mau

DT.kw.nigra.mau = dunnTest(mean_time_mau ~ Catg,
                           data = nigra_mau_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.nigra.mau = DT.kw.nigra.mau$res
letters_nigra.mau <- cldList(comparison = DT.kw.nigra.mau$Comparison,
                             p.value    = DT.kw.nigra.mau$P.adj,
                             threshold  = 0.05)


colnames(letters_nigra.mau)[1:2] <- c("Species","mau_letters")

letters_nigra.mau <- letters_nigra.mau[,1:2]
colnames(letters_nigra.mau)[2]<-"P. nigra"

letters_nigra.mau



#sylvestris
sylvestris_mau_outliers_catg <- merge(spain_sel_sylvestris %>% 
                                        rename("Sp.x"="Sp.x.x") %>% 
                                        select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                        select(-Arid_Dm_i), 
                                      by=c("Sp.x", "ID"))

sylvestris_mau_outliers_catg_mean <- sylvestris_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))


kw.sylvestris.mau <- kruskal.test(mean_time_mau ~ Catg,
                                  data = sylvestris_mau_outliers_catg)

kw.sylvestris.mau

DT.kw.sylvestris.mau = dunnTest(mean_time_mau ~ Catg,
                                data = sylvestris_mau_outliers_catg,
                                method="bh")#library(FSA)

DT.kw.sylvestris.mau = DT.kw.sylvestris.mau$res
letters_sylvestris.mau <- cldList(comparison = DT.kw.sylvestris.mau$Comparison,
                                  p.value    = DT.kw.sylvestris.mau$P.adj,
                                  threshold  = 0.05)


colnames(letters_sylvestris.mau)[1:2] <- c("Species","mau_letters")

letters_sylvestris.mau <- letters_sylvestris.mau[,1:2]
colnames(letters_sylvestris.mau)[2]<-"P. sylvestris"

letters_sylvestris.mau




#ilex
ilex_mau_outliers_catg <- merge(spain_sel_ilex %>% 
                                  rename("Sp.x"="Sp.x.x") %>% 
                                  select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                  select(-Arid_Dm_i), 
                                by=c("Sp.x", "ID"))


ilex_mau_outliers_catg_mean <- ilex_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))

kw.ilex.mau <- kruskal.test(mean_time_mau ~ Catg,
                            data = ilex_mau_outliers_catg)

kw.ilex.mau

DT.kw.ilex.mau = dunnTest(mean_time_mau ~ Catg,
                          data = ilex_mau_outliers_catg,
                          method="bh")#library(FSA)

DT.kw.ilex.mau = DT.kw.ilex.mau$res
letters_ilex.mau <- cldList(comparison = DT.kw.ilex.mau$Comparison,
                            p.value    = DT.kw.ilex.mau$P.adj,
                            threshold  = 0.05)


colnames(letters_ilex.mau)[1:2] <- c("Species","mau_letters")

letters_ilex.mau <- letters_ilex.mau[,1:2]
colnames(letters_ilex.mau)[2]<-"Q. ilex"

letters_ilex.mau


#suber
suber_mau_outliers_catg <- merge(spain_sel_suber %>% 
                                   rename("Sp.x"="Sp.x.x") %>% 
                                   select(ID:Sp.x, Tree_dens:elevation), dt.mau.time %>% 
                                   select(-Arid_Dm_i), 
                                 by=c("Sp.x", "ID"))

suber_mau_outliers_catg_mean <- suber_mau_outliers_catg %>% 
  group_by(Catg) %>% 
  summarise(mean_time_mau=mean(mean_time_mau))


kw.suber.mau <- kruskal.test(mean_time_mau ~ Catg,
                             data = suber_mau_outliers_catg)

kw.suber.mau

DT.kw.suber.mau = dunnTest(mean_time_mau ~ Catg,
                           data = suber_mau_outliers_catg,
                           method="bh")#library(FSA)

DT.kw.suber.mau = DT.kw.suber.mau$res
letters_suber.mau <- cldList(comparison = DT.kw.suber.mau$Comparison,
                             p.value    = DT.kw.suber.mau$P.adj,
                             threshold  = 0.05)


colnames(letters_suber.mau)[1:2] <- c("Species","mau_letters")

letters_suber.mau <- letters_suber.mau[,1:2]
colnames(letters_suber.mau)[2]<-"Q. suber"

letters_suber.mau

letters_mau <- cbind(letters_eucal.mau, letters_euro.mau[2],
                     letters_sativa.mau[2], letters_halep.mau[2],
                     letters_pinaster.mau[2], letters_pinea.mau[2],
                     letters_nigra.mau[2], letters_sylvestris.mau[2], 
                     letters_ilex.mau[2], letters_suber.mau[2])
letters_mau$metric<- c("mau", "mau","mau")



####################
#finalmente
final_letters <- rbind(letters_sos, letters_eos, letters_los,
                       letters_pot, letters_pop, letters_peak,
                       letters_trough, letters_msp, letters_mau)



#sos
final_mean_catg_sos <- cbind(eucal_sos_outliers_catg_mean, 
                            sativa_sos_outliers_catg_mean[2],
                            euro_sos_outliers_catg_mean[2],
                            halep_sos_outliers_catg_mean[2],
                            pinaster_sos_outliers_catg_mean[2],
                            pinea_sos_outliers_catg_mean[2],
                            nigra_sos_outliers_catg_mean[2],
                            sylvestris_sos_outliers_catg_mean[2],
                            ilex_sos_outliers_catg_mean[2],
                            suber_sos_outliers_catg_mean[2])

final_mean_catg_sos$var <- "sos"
names(final_mean_catg_sos)[2:11]<-c("E. camaldulensis", " C. sativa", "O. europaea",
                                "P. halepensis", "P. pinaster", "P. pinea", "P. nigra",
                                "P. sylvestris", "Q. ilex", "Q. suber") 



#eos
final_mean_catg_eos <- cbind(eucal_eos_outliers_catg_mean, 
                             sativa_eos_outliers_catg_mean[2],
                             euro_eos_outliers_catg_mean[2],
                             halep_eos_outliers_catg_mean[2],
                             pinaster_eos_outliers_catg_mean[2],
                             pinea_eos_outliers_catg_mean[2],
                             nigra_eos_outliers_catg_mean[2],
                             sylvestris_eos_outliers_catg_mean[2],
                             ilex_eos_outliers_catg_mean[2],
                             suber_eos_outliers_catg_mean[2])

final_mean_catg_eos$var <- "eos"
names(final_mean_catg_eos)[2:11]<-c("E. camaldulensis", " C. sativa", "O. europaea",
                                    "P. halepensis", "P. pinaster", "P. pinea", "P. nigra",
                                    "P. sylvestris", "Q. ilex", "Q. suber") 


#los
final_mean_catg_los <- cbind(eucal_los_outliers_catg_mean, 
                             sativa_los_outliers_catg_mean[2],
                             euro_los_outliers_catg_mean[2],
                             halep_los_outliers_catg_mean[2],
                             pinaster_los_outliers_catg_mean[2],
                             pinea_los_outliers_catg_mean[2],
                             nigra_los_outliers_catg_mean[2],
                             sylvestris_los_outliers_catg_mean[2],
                             ilex_los_outliers_catg_mean[2],
                             suber_los_outliers_catg_mean[2])

final_mean_catg_los$var <- "los"
names(final_mean_catg_los)[2:11]<-c("E. camaldulensis", " C. sativa", "O. europaea",
                                    "P. halepensis", "P. pinaster", "P. pinea", "P. nigra",
                                    "P. sylvestris", "Q. ilex", "Q. suber") 


#pot
final_mean_catg_pot <- cbind(eucal_pot_outliers_catg_mean, 
                             sativa_pot_outliers_catg_mean[2],
                             euro_pot_outliers_catg_mean[2],
                             halep_pot_outliers_catg_mean[2],
                             pinaster_pot_outliers_catg_mean[2],
                             pinea_pot_outliers_catg_mean[2],
                             nigra_pot_outliers_catg_mean[2],
                             sylvestris_pot_outliers_catg_mean[2],
                             ilex_pot_outliers_catg_mean[2],
                             suber_pot_outliers_catg_mean[2])

final_mean_catg_pot$var <- "pot"
names(final_mean_catg_pot)[2:11]<-c("E. camaldulensis", " C. sativa", "O. europaea",
                                    "P. halepensis", "P. pinaster", "P. pinea", "P. nigra",
                                    "P. sylvestris", "Q. ilex", "Q. suber") 

#pop
final_mean_catg_pop <- cbind(eucal_pop_outliers_catg_mean, 
                             sativa_pop_outliers_catg_mean[2],
                             euro_pop_outliers_catg_mean[2],
                             halep_pop_outliers_catg_mean[2],
                             pinaster_pop_outliers_catg_mean[2],
                             pinea_pop_outliers_catg_mean[2],
                             nigra_pop_outliers_catg_mean[2],
                             sylvestris_pop_outliers_catg_mean[2],
                             ilex_pop_outliers_catg_mean[2],
                             suber_pop_outliers_catg_mean[2])

final_mean_catg_pop$var <- "pop"
names(final_mean_catg_pop)[2:11]<-c("E. camaldulensis", " C. sativa", "O. europaea",
                                    "P. halepensis", "P. pinaster", "P. pinea", "P. nigra",
                                    "P. sylvestris", "Q. ilex", "Q. suber") 


#peak
final_mean_catg_peak <- cbind(eucal_peak_outliers_catg_mean, 
                             sativa_peak_outliers_catg_mean[2],
                             euro_peak_outliers_catg_mean[2],
                             halep_peak_outliers_catg_mean[2],
                             pinaster_peak_outliers_catg_mean[2],
                             pinea_peak_outliers_catg_mean[2],
                             nigra_peak_outliers_catg_mean[2],
                             sylvestris_peak_outliers_catg_mean[2],
                             ilex_peak_outliers_catg_mean[2],
                             suber_peak_outliers_catg_mean[2])

final_mean_catg_peak$var <- "peak"
names(final_mean_catg_peak)[2:11]<-c("E. camaldulensis", " C. sativa", "O. europaea",
                                    "P. halepensis", "P. pinaster", "P. pinea", "P. nigra",
                                    "P. sylvestris", "Q. ilex", "Q. suber") 


#trough
final_mean_catg_trough <- cbind(eucal_trough_outliers_catg_mean, 
                              sativa_trough_outliers_catg_mean[2],
                              euro_trough_outliers_catg_mean[2],
                              halep_trough_outliers_catg_mean[2],
                              pinaster_trough_outliers_catg_mean[2],
                              pinea_trough_outliers_catg_mean[2],
                              nigra_trough_outliers_catg_mean[2],
                              sylvestris_trough_outliers_catg_mean[2],
                              ilex_trough_outliers_catg_mean[2],
                              suber_trough_outliers_catg_mean[2])

final_mean_catg_trough$var <- "trough"
names(final_mean_catg_trough)[2:11]<-c("E. camaldulensis", " C. sativa", "O. europaea",
                                    "P. halepensis", "P. pinaster", "P. pinea", "P. nigra",
                                    "P. sylvestris", "Q. ilex", "Q. suber") 


#msp
final_mean_catg_msp <- cbind(eucal_msp_outliers_catg_mean, 
                                sativa_msp_outliers_catg_mean[2],
                                euro_msp_outliers_catg_mean[2],
                                halep_msp_outliers_catg_mean[2],
                                pinaster_msp_outliers_catg_mean[2],
                                pinea_msp_outliers_catg_mean[2],
                                nigra_msp_outliers_catg_mean[2],
                                sylvestris_msp_outliers_catg_mean[2],
                                ilex_msp_outliers_catg_mean[2],
                                suber_msp_outliers_catg_mean[2])

final_mean_catg_msp$var <- "msp"
names(final_mean_catg_msp)[2:11]<-c("E. camaldulensis", " C. sativa", "O. europaea",
                                    "P. halepensis", "P. pinaster", "P. pinea", "P. nigra",
                                    "P. sylvestris", "Q. ilex", "Q. suber") 

#mau
final_mean_catg_mau <- cbind(eucal_mau_outliers_catg_mean, 
                             sativa_mau_outliers_catg_mean[2],
                             euro_mau_outliers_catg_mean[2],
                             halep_mau_outliers_catg_mean[2],
                             pinaster_mau_outliers_catg_mean[2],
                             pinea_mau_outliers_catg_mean[2],
                             nigra_mau_outliers_catg_mean[2],
                             sylvestris_mau_outliers_catg_mean[2],
                             ilex_mau_outliers_catg_mean[2],
                             suber_mau_outliers_catg_mean[2])

final_mean_catg_mau$var <- "mau"
names(final_mean_catg_mau)[2:11]<-c("E. camaldulensis", " C. sativa", "O. europaea",
                                    "P. halepensis", "P. pinaster", "P. pinea", "P. nigra",
                                    "P. sylvestris", "Q. ilex", "Q. suber") 



final_mean_catg <- rbind(final_mean_catg_sos, final_mean_catg_eos, final_mean_catg_los, 
                         final_mean_catg_pot, final_mean_catg_pop, final_mean_catg_peak,
                         final_mean_catg_trough, final_mean_catg_msp, final_mean_catg_mau)

final_mean_catg[2:11] <- round(final_mean_catg[2:11],2)
  


write.xlsx(list(final_letters, final_mean_catg), paste(path2csv, file = "final_letters_metrics_sp.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = c("metrics_sp", "mean_sp"), append = FALSE)






