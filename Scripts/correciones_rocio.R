### partiendo de los datos de dt.temp donde han sido eliminados los outliers, 
#original del script 5.1 outliers temporales vamos a montar un modelo lme

#creamos un dtframe con las variables y sus valores por columna
dt.rocio <- dt.temp %>% 
  select(ID:trough) %>% 
  pivot_longer(cols=c("pop":"trough"),
               names_to='metric',
               values_to='value') %>% 
  filter(!Sp.x %in% c("Quercus faginea", "Quercus canariensis"))
str(dt.rocio)


write.xlsx(dt.rocio, paste(path2csv, file = "dtrocio.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "dtrocio", append = FALSE)


#aplicamos el modelo lineal mixto
library(lme4)

model_rocio <- lmer(value ~ Sp.x * metric + year + (1|Sp.x:year), data = dt.rocio)
summary(model_rocio)




#referente a la relacion entre aridez y el resto de las métricas
#introducimos la variable aridez


dt.rocio.arid <- merge(dt.rocio, pheno_metr_p_clima %>% 
                         select(ID, Arid_Dm_i), by="ID")

dt.rocio.arid <- dt.rocio.arid %>% 
  na.omit()

write.xlsx(dt.rocio.arid, paste(path2csv, file = "dtrocio.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "dtrocio_arid", append = TRUE)

#modelo lineal
# Ajustando un GLM
modelo_glm_rocio <- glm(value ~ year * Arid_Dm_i * Sp.x + metric, family = gaussian(), data = dt.rocio.arid)
options(max.print=1000000)
summary(modelo_glm_rocio)



#GAM no consigo correr 
library(mgcv)
modelo_gam_rocio <- gam(value ~ s(year) + s(Arid_Dm_i) + s(Sp.x) + metric, data = dt.rocio.arid)

summary(modelo_gam_rocio)


#sobre  el valor del ndvi del pixel

write.xlsx(ndvi_allsp_clean, paste(path2csv, file = "ndvi_clean.xlsx"), 
           colnames = TRUE, rownames = TRUE, 
           sheetName = "ndvi", append = FALSE)




################################################################################
#vamos a modelar para cada métrica un GLM con las especies y el tiempo
#habremos de retirar los NA

#SOS

dt.sos.rocio <- dt.rocio.arid %>% 
  filter(metric=="sos") 
fit.cont(dt.sos.rocio$value)#se comporta como uma distribución normal

glm_sos_rocio <- glm(value ~ Sp.x*year, data = dt.sos.rocio)

summary(glm_sos_rocio)


sink("glm_sos_rocio.txt")
print(summary(glm_sos_rocio))
sink()  # returns output to the console



em_sos <- emmeans(glm_sos_rocio, "Sp.x")# library emmeans
contr_sos <- contrast(em_sos, "pairwise", adjust = "Tukey")

sink("contr_sos.txt")
print(contr_sos)
sink()  # returns output to the console


lsd_sos_rocio <- LSD.test(glm_sos_rocio, "Sp.x", console=TRUE)#paquete agricolae

lsd_sos <- lsd_sos_rocio$groups

sink("lsd_sos_rocio.txt")
print(lsd_sos_rocio)
sink()  # returns output to the console


#tambien podria hacerse mediante un anova
TukeyHSD(aov(glm_sos_rocio))



##################################
#vamos a sacar el kruskall wallis y el post hoc de cada variable para todas las especies
#https://rcompanion.org/rcompanion/d_06.html

dt.sos.time <- dt.sos.rocio %>% 
  group_by(ID, Sp.x) %>% 
  summarise(mean_time_sos=mean(value), Arid_Dm_i=mean(Arid_Dm_i))


kw.sos <- kruskal.test(mean_time_sos ~ Sp.x,
                       data = dt.sos.time)

kw.sos

DT.sos = dunnTest(mean_time_sos ~ Sp.x,
                           data = dt.sos.time,
                           method="bh")#library(FSA)

DTD.sos = DT.sos$res
letters_sos <- cldList(comparison = DTD.sos$Comparison,
        p.value    = DTD.sos$P.adj,
        threshold  = 0.05)


colnames(letters_sos)[1:2] <- c("Species","sos_letters")

letters_sos <- letters_sos[,1:2]

letters_sos


#####
#EOS

dt.eos.rocio <- dt.rocio.arid %>% 
  filter(metric=="eos") 
fit.cont(dt.eos.rocio$value)#se comporta como uma distribuiçao normal
summary(dt.eos.rocio)

glm_eos_rocio <- glm(value ~ Sp.x + year + Sp.x*year, data = dt.eos.rocio)

summary(glm_eos_rocio)


sink("glm_eos_rocio.txt")
print(summary(glm_eos_rocio))
sink()  # returns output to the console

em_eos <- emmeans(glm_eos_rocio, "Sp.x")# library emmeans
contr_eos <- contrast(em_eos, "pairwise", adjust = "Tukey")

sink("contr_eos.txt")
print(contr_eos)
sink()  # returns output to the console


lsd_eos_rocio <- LSD.test(glm_eos_rocio, "Sp.x", console=TRUE)#paquete agricolae

lsd_eos <- lsd_eos_rocio$groups


sink("lsd_eos_rocio.txt")
print(lsd_eos_rocio)
sink()  # returns output to the console



##################################
#vamos a sacar el kruskall wallis y el post hoc de cada variable para todas las especies

dt.eos.time <- dt.eos.rocio %>% 
  group_by(ID, Sp.x) %>% 
  summarise(mean_time_eos=mean(value), Arid_Dm_i=mean(Arid_Dm_i))


kw.eos <- kruskal.test(mean_time_eos ~ Sp.x,
                       data = dt.eos.time)

DT.eos = dunnTest(mean_time_eos ~ Sp.x,
                  data = dt.eos.time,
                  method="bh")#library(FSA)

DTD.eos = DT.eos$res
letters_eos <- cldList(comparison = DTD.eos$Comparison,
                       p.value    = DTD.eos$P.adj,
                       threshold  = 0.05)


colnames(letters_eos)[1:2] <- c("Species","eos_letters")

letters_eos <- letters_eos[,1:2]

letters_eos


#####
#LOS

dt.los.rocio <- dt.rocio.arid %>% 
  filter(metric=="los") 
fit.cont(dt.los.rocio$value)#se comporta como uma distribuiçao normal
summary(dt.los.rocio)

glm_los_rocio <- glm(value ~ Sp.x + year + Sp.x*year, data = dt.los.rocio)

summary(glm_los_rocio)


sink("glm_los_rocio.txt")
print(summary(glm_los_rocio))
sink()  # returns output to the console


em_los <- emmeans(glm_los_rocio, "Sp.x")# library emmeans
contr_los <- contrast(em_los, "pairwise", adjust = "Tukey")

sink("contr_los.txt")
print(contr_los)
sink()  # returns output to the console


lsd_los_rocio <- LSD.test(glm_los_rocio, "Sp.x", console=TRUE)#paquete agricolae

lsd_los <- lsd_los_rocio$groups

sink("lsd_los_rocio.txt")
print(lsd_los_rocio)
sink()  # returns output to the console


##################################
#vamos a sacar el kruskall wallis y el post hoc de cada variable para todas las especies

dt.los.time <- dt.los.rocio %>% 
  group_by(ID, Sp.x) %>% 
  summarise(mean_time_los=mean(value), Arid_Dm_i=mean(Arid_Dm_i))


kw.los <- kruskal.test(mean_time_los ~ Sp.x,
                       data = dt.los.time)

DT.los = dunnTest(mean_time_los ~ Sp.x,
                  data = dt.los.time,
                  method="bh")#library(FSA)

DTD.los = DT.los$res
letters_los <- cldList(comparison = DTD.los$Comparison,
                       p.value    = DTD.los$P.adj,
                       threshold  = 0.05)


colnames(letters_los)[1:2] <- c("Species","los_letters")

letters_los <- letters_los[,1:2]

letters_los



#####
#POT

dt.pot.rocio <- dt.rocio.arid %>% 
  filter(metric=="pot") 
fit.cont(dt.pot.rocio$value)#se comporta como uma distribuiçao normal
summary(dt.pot.rocio)

glm_pot_rocio <- glm(value ~ Sp.x + year + Sp.x*year, data = dt.pot.rocio)

summary(glm_pot_rocio)


sink("glm_pot_rocio.txt")
print(summary(glm_pot_rocio))
sink()  # returns output to the console

em_pot <- emmeans(glm_pot_rocio, "Sp.x")# library emmeans
contr_pot <- contrast(em_pot, "pairwise", adjust = "Tukey")

sink("contr_pot.txt")
print(contr_pot)
sink()  # returns output to the console


lsd_pot_rocio <- LSD.test(glm_pot_rocio, "Sp.x", console=TRUE)#paquete agricolae

lsd_pot <- lsd_pot_rocio$groups

sink("lsd_pot_rocio.txt")
print(lsd_pot_rocio)
sink()  # returns output to the console

##################################
#vamos a sacar el kruskall wallis y el post hoc de cada variable para todas las especies

dt.pot.time <- dt.pot.rocio %>% 
  group_by(ID, Sp.x) %>% 
  summarise(mean_time_pot=mean(value), Arid_Dm_i=mean(Arid_Dm_i))


kw.pot <- kruskal.test(mean_time_pot ~ Sp.x,
                       data = dt.pot.time)

DT.pot = dunnTest(mean_time_pot ~ Sp.x,
                  data = dt.pot.time,
                  method="bh")#library(FSA)

DTD.pot = DT.pot$res
letters_pot <- cldList(comparison = DTD.pot$Comparison,
                       p.value    = DTD.pot$P.adj,
                       threshold  = 0.05)


colnames(letters_pot)[1:2] <- c("Species","pot_letters")

letters_pot <- letters_pot[,1:2]

letters_pot



#####
#POP

dt.pop.rocio <- dt.rocio.arid %>% 
  filter(metric=="pop") 
fit.cont(dt.pop.rocio$value)#se comporta como uma distribuiçao normal
summary(dt.pop.rocio)

glm_pop_rocio <- glm(value ~ Sp.x + year + Sp.x*year, data = dt.pop.rocio)

summary(glm_pop_rocio)


sink("glm_pop_rocio.txt")
print(summary(glm_pop_rocio))
sink()  # returns output to the console

em_pop <- emmeans(glm_pop_rocio, "Sp.x")# library emmeans
contr_pop <- contrast(em_pop, "pairwise", adjust = "Tukey")

sink("contr_pop.txt")
print(contr_pop)
sink()  # returns output to the console


lsd_pop_rocio <- LSD.test(glm_pop_rocio, "Sp.x", console=TRUE)#paquete agricolae

lsd_pop <- lsd_pop_rocio$groups

sink("lsd_pop_rocio.txt")
print(lsd_pop_rocio)
sink()  # returns output to the console


##################################
#vamos a sacar el kruskall wallis y el post hoc de cada variable para todas las especies

dt.pop.time <- dt.pop.rocio %>% 
  group_by(ID, Sp.x) %>% 
  summarise(mean_time_pop=mean(value), Arid_Dm_i=mean(Arid_Dm_i))


kw.pop <- kruskal.test(mean_time_pop ~ Sp.x,
                       data = dt.pop.time)

DT.pop = dunnTest(mean_time_pop ~ Sp.x,
                  data = dt.pop.time,
                  method="bh")#library(FSA)

DTD.pop = DT.pop$res
letters_pop <- cldList(comparison = DTD.pop$Comparison,
                       p.value    = DTD.pop$P.adj,
                       threshold  = 0.05)


colnames(letters_pop)[1:2] <- c("Species","pop_letters")

letters_pop <- letters_pop[,1:2]

letters_pop



#####
#PEAK

dt.peak.rocio <- dt.rocio.arid %>% 
  filter(metric=="peak") 
fit.cont(dt.peak.rocio$value)#se comporta como uma distribuiçao normal
summary(dt.peak.rocio)

glm_peak_rocio <- glm(value ~ Sp.x + year + Sp.x*year, data = dt.peak.rocio)

summary(glm_peak_rocio)


sink("glm_peak_rocio.txt")
print(summary(glm_peak_rocio))
sink()  # returns output to the console


em_peak <- emmeans(glm_peak_rocio, "Sp.x")# library emmeans
contr_peak <- contrast(em_peak, "pairwise", adjust = "Tukey")

sink("contr_peak.txt")
print(contr_peak)
sink()  # returns output to the console


lsd_peak_rocio <- LSD.test(glm_peak_rocio, "Sp.x", console=TRUE)#paquete agricolae

lsd_peak <- lsd_peak_rocio$groups

sink("lsd_peak_rocio.txt")
print(lsd_peak_rocio)
sink()  # returns output to the console



##################################
#vamos a sacar el kruskall wallis y el post hoc de cada variable para todas las especies

dt.peak.time <- dt.peak.rocio %>% 
  group_by(ID, Sp.x) %>% 
  summarise(mean_time_peak=mean(value), Arid_Dm_i=mean(Arid_Dm_i))


kw.peak <- kruskal.test(mean_time_peak ~ Sp.x,
                       data = dt.peak.time)

DT.peak = dunnTest(mean_time_peak ~ Sp.x,
                  data = dt.peak.time,
                  method="bh")#library(FSA)

DTD.peak = DT.peak$res
letters_peak <- cldList(comparison = DTD.peak$Comparison,
                       p.value    = DTD.peak$P.adj,
                       threshold  = 0.05)


colnames(letters_peak)[1:2] <- c("Species","peak_letters")

letters_peak <- letters_peak[,1:2]

letters_peak




#####
#TROUGH

dt.trough.rocio <- dt.rocio.arid %>% 
  filter(metric=="trough") 
fit.cont(dt.trough.rocio$value)#se comporta como uma distribuiçao normal
summary(dt.trough.rocio)

glm_trough_rocio <- glm(value ~ Sp.x + year + Sp.x*year, data = dt.trough.rocio)

summary(glm_trough_rocio)


sink("glm_trough_rocio.txt")
print(summary(glm_trough_rocio))
sink()  # returns output to the console


em_trough <- emmeans(glm_trough_rocio, "Sp.x")# library emmeans
contr_trough <- contrast(em_trough, "pairwise", adjust = "Tukey")

sink("contr_trough.txt")
print(contr_trough)
sink()  # returns output to the console


lsd_trough_rocio <- LSD.test(glm_trough_rocio, "Sp.x", console=TRUE)#paquete agricolae

lsd_trough <- lsd_trough_rocio$groups

sink("lsd_trough_rocio.txt")
print(lsd_trough_rocio)
sink()  # returns output to the console


##################################
#vamos a sacar el kruskall wallis y el post hoc de cada variable para todas las especies

dt.trough.time <- dt.trough.rocio %>% 
  group_by(ID, Sp.x) %>% 
  summarise(mean_time_trough=mean(value), Arid_Dm_i=mean(Arid_Dm_i))


kw.trough <- kruskal.test(mean_time_trough ~ Sp.x,
                       data = dt.trough.time)

DT.trough = dunnTest(mean_time_trough ~ Sp.x,
                  data = dt.trough.time,
                  method="bh")#library(FSA)

DTD.trough = DT.trough$res
letters_trough <- cldList(comparison = DTD.trough$Comparison,
                       p.value    = DTD.trough$P.adj,
                       threshold  = 0.05)


colnames(letters_trough)[1:2] <- c("Species","trough_letters")

letters_trough <- letters_trough[,1:2]

letters_trough



#####
#MSP

dt.msp.rocio <- dt.rocio.arid %>% 
  filter(metric=="msp") 
fit.cont(dt.msp.rocio$value)#se comporta como uma distribuiçao normal
summary(dt.msp.rocio)

glm_msp_rocio <- glm(value ~ Sp.x + year + Sp.x*year, data = dt.msp.rocio)

summary(glm_msp_rocio)


sink("glm_msp_rocio.txt")
print(summary(glm_msp_rocio))
sink()  # returns output to the console


em_msp <- emmeans(glm_msp_rocio, "Sp.x")# library emmeans
contr_msp <- contrast(em_msp, "pairwise", adjust = "Tukey")

sink("contr_msp.txt")
print(contr_msp)
sink()  # returns output to the console


lsd_msp_rocio <- LSD.test(glm_msp_rocio, "Sp.x", console=TRUE)#paquete agricolae

lsd_msp <- lsd_msp_rocio$groups

sink("lsd_msp_rocio.txt")
print(lsd_msp_rocio)
sink()  # returns output to the console


##################################
#vamos a sacar el kruskall wallis y el post hoc de cada variable para todas las especies

dt.msp.time <- dt.msp.rocio %>% 
  group_by(ID, Sp.x) %>% 
  summarise(mean_time_msp=mean(value), Arid_Dm_i=mean(Arid_Dm_i))


kw.msp <- kruskal.test(mean_time_msp ~ Sp.x,
                       data = dt.msp.time)

DT.msp = dunnTest(mean_time_msp ~ Sp.x,
                  data = dt.msp.time,
                  method="bh")#library(FSA)

DTD.msp = DT.msp$res
letters_msp <- cldList(comparison = DTD.msp$Comparison,
                       p.value    = DTD.msp$P.adj,
                       threshold  = 0.05)


colnames(letters_msp)[1:2] <- c("Species","msp_letters")

letters_msp <- letters_msp[,1:2]

letters_msp



#####
#MAU

dt.mau.rocio <- dt.rocio.arid %>% 
  filter(metric=="mau") 
fit.cont(dt.mau.rocio$value)#se comporta como uma distribuiçao normal
summary(dt.mau.rocio)

glm_mau_rocio <- glm(value ~ Sp.x + year + Sp.x*year, data = dt.mau.rocio)

summary(glm_mau_rocio)


sink("glm_mau_rocio.txt")
print(summary(glm_mau_rocio))
sink()  # returns output to the console


em_mau <- emmeans(glm_mau_rocio, "Sp.x")# library emmeans
contr_mau <- contrast(em_mau, "pairwise", adjust = "Tukey")

sink("contr_mau.txt")
print(contr_mau)
sink()  # returns output to the console


lsd_mau_rocio <- LSD.test(glm_mau_rocio, "Sp.x", console=TRUE)#paquete agricolae

lsd_mau <- lsd_mau_rocio$groups

sink("lsd_mau_rocio.txt")
print(lsd_mau_rocio)
sink()  # returns output to the console


##################################
#vamos a sacar el kruskall wallis y el post hoc de cada variable para todas las especies

dt.mau.time <- dt.mau.rocio %>% 
  group_by(ID, Sp.x) %>% 
  summarise(mean_time_mau=mean(value), Arid_Dm_i=mean(Arid_Dm_i))


kw.mau <- kruskal.test(mean_time_mau ~ Sp.x,
                       data = dt.mau.time)

DT.mau = dunnTest(mean_time_mau ~ Sp.x,
                  data = dt.mau.time,
                  method="bh")#library(FSA)

DTD.mau = DT.mau$res
letters_mau <- cldList(comparison = DTD.mau$Comparison,
                       p.value    = DTD.mau$P.adj,
                       threshold  = 0.05)


colnames(letters_mau)[1:2] <- c("Species","mau_letters")

letters_mau <- letters_mau[,1:2]

letters_mau


letters <- cbind(letters_sos %>% 
                   select(Species,sos_letters), letters_eos %>% 
                   select(eos_letters),letters_los %>% 
                   select(los_letters), letters_pot %>% 
                   select(pot_letters), letters_pop %>% 
                   select(pop_letters), letters_peak %>% 
                   select(peak_letters), letters_trough %>% 
                   select(trough_letters),letters_msp %>% 
                   select(msp_letters), letters_mau %>% 
                   select(mau_letters))

write.xlsx(letters, 
           paste(path2csv, file = "letters_posthoc.xlsx"), 
           col.names = TRUE, row.names = TRUE, 
           append = TRUE)




write.xlsx(list("lsd_sos"=lsd_sos, "lsd_eos" =lsd_eos, "lsd_los"=lsd_los,
                "lsd_pop"=lsd_pop, "lsd_pot" =lsd_pot, "lsd_peak"=lsd_peak,
                "lsd_trough"=lsd_trough, "lsd_msp" =lsd_msp, "lsd_mau"=lsd_mau), 
                paste(path2csv, file = "lsd_rocio.xlsx"), 
           col.names = TRUE, row.names = TRUE, 
           append = TRUE)



##############################################################################
##############################################################################
##############################################################################

#ahora vamos a crear un correlalograma entre los valores anuales del NDVi entre
#todas las especies, padra eso usaremos el dataframe sum_allsp_clean_p


rocio_cor_data <- sum_allsp_clean %>% 
                            select(Sp.x, `jan_1994_mean`:`dec_2021_mean`) %>% 
                            filter(Sp.x %in% c("Eucalyptus camaldulensis", "Olea europaea", "Castanea sativa",
                                               "Pinus halepensis", "Pinus pinaster", "Pinus pinea", "Pinus nigra", 
                                               "Pinus sylvestris", "Quercus ilex", "Quercus suber")) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number=1) #%>% 

rocio_cor_data <- sapply(rocio_cor_data, as.numeric)

rocio_cor_data <- rocio_cor_data %>% 
  as.data.frame()

rocio_cor <- cor(rocio_cor_data)

 
cor_test_mat <- corr.test(rocio_cor_data)$p    # Apply corr.test function
cor_test_mat                         # Print matrix of p-values


# cor.mtest <- function(mat, ...) {
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat<- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       tmp <- cor.test(mat[, i], mat[, j], ...)
#       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#     }
#   }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# 
# p.mat <- cor.mtest(rocio_cor)
# head(p.mat[, 1:10])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))


pdf(file = "rocio_cor.pdf")
rocio_corplot <- corrplot(rocio_cor, method="color", col=col(200),  
                      type="upper", order="hclust", p.mat = cor_test_mat,
                      insig = "p-value",
                      addCoef.col = "black", # Add coefficient of correlation
                      tl.col="black", tl.srt=45, #Text label color and rotation
                      # Combine with significance
                      sig.level = 0.01,#p.mat = p.mat, insig = "blank", 
                      # hide correlation coefficient on the principal diagonal
                      diag=FALSE) 

dev.off()


# corrplot(rocio_cor,                    # Draw corrplot with p-values
#          p.mat = cor_test_mat, type="upper", order="hclust",
#          insig = "p-value",tl.pos="n", sig.level=.001,
#          number.cex = 1, tl.cex = 1, pch.cex = 1.5,
#          col=col(200), addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45)


#hemos alterado el codigo fuente de la libreria
#trace(corrplot, edit=TRUE)
# Then replace on line 443
# 
# place_points = function(sig.locs, point) {
#   text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], 
#        labels = point, col = pch.col, cex = pch.cex, 
#        lwd = 2)
#   with:
#     
#     # adjust text(X,Y ...) according to your needs, here +0.25 is added to the Y-position    
#     place_points = function(sig.locs, point) {
#       text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
#            labels = point, col = pch.col, cex = pch.cex, 
#            lwd = 2)



pdf(file = "rocio_corp.pdf")
corrplot(rocio_cor, p.mat = cor_test_mat ,sig.level = c(.001, .01, .05), type = "upper", 
         insig = "label_sig", pch.cex = 1.5,
         tl.col = "black", method = "color", tl.srt = 28, number.cex = 0.7, tl.cex = 0.7,  addCoef.col = "black",
         pch.col = "tomato", font.main = 3, family = "serif", mar=c(0,0,1,0), cl.pos = "b")
dev.off()




































































