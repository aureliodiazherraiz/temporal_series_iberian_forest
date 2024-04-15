#librerias utilizadas

library(tidyverse)
library(ggpubr) #graficas y statr cor en ggplot
library(hrbrthemes)#para temas en ggplot
library(ggthemes)#temas para ggtplot
library(readxl)
library(readr)
library(openxlsx)#escribir en excell
library(ggh4x)
library(cowplot)
library(agricolae)
library(emmeans)
library(janitor)#para editar nombres de filas
library(reshape2)
library(psych)#para sacar valores p value en corrplot 
library(FSA)#para aplicar duncan test y comparar las medias del KW
library(rcompanion)#para tener las diferencias entre letras del KW

library(R.utils) #para descomprimir unzip
library(remotes) #$para extraer datos climáticos 
library(simpleRCRU) #para extraer datos climáticos de archivos CRU
library(zoo)#para realizar interpolaciones, na.approx()
library(imputeTS) #para realizar interpolaciones na.intepolation
library(rriskDistributions)#para ver las distribuciones de los datos fit.cont


library(Rbeast)#para limpiar el ruido de los datos de NDVI 
library(greenbrown)#ajuda a extraer las fases feologicas de la vegetacion hace falta instalar Kendall
#!!cuidado conflicto con select del dplyr

#devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")#para los graficos de PCA
library(ggbiplot)#graficas PCA
library(FactoMineR)#para PCA
library(corrplot)






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










library(vegan)# para comparar matrices de distancias
library(geodist)#para comparar distandias euclidianas y geograficas
library(sf)
library(mapview)

library(Directional)#para estadistica circular
library(circular)
library(CircOutlier)#para eliminar outliers en datos circulares

library(matrixStats)#para trabajar con operaciones en matrices 
library(plotly)#para mejorar los graficos ggplot


library(lubridate)#para transformar datos en tiempo y poder graficar
library(tseries)#series temporales
library(crayon)
library(forecast)
library(chron)
library(zoo)


# install.packages("remotes")
remotes::install_github("VeruGHub/easyclimate")
library(easyclimate)






library(cowplot)
library(gtools)# quancut: para realizar categorias de variables como factores



#install.packages("remotes")
#remotes::install_github("ahmad-alkadri/simpleRCRU")

library(rstatix)
devtools::install_github("clbustos/dominanceAnalysis")
library(dominanceanalysis)#sirve para calcular la importancia relativa de cada predictor dentro del modelo
library(parameters)#dominancia de predictores
library(variancePartition)#grafica la variancia absorbida por cada predictor
library(relaimpo)#para ver la imprtancia relativa de cada variable en los modelos
install.packages("devtools")
devtools::install_github("martinctc/rwa")
library(rwa)#para ver la imprtancia relativa de cada variable en los modelos
library(h2o)#lo mimso pero para modelos glmm
devtools::install_github("strengejacke/ggeffects")#para estudiar los efectos en modelos glmm


library(normtest)# ayuda a verificar la normalidad de los residuos, test jb
library(nortest)# ayuda a verificar la normalidad de los residuos, test jb
library(effects)# para calcular partial residuals
library(lmtest)#para la homocedasticidad de los residuos

library(corrr)#correlaciones 

library(scales)#escala, normaliza las variables
library(agricolae)#ANOVA
library(nortest)#para los test de normalidad
library(car)#los test para la homocedasticidad
library(PerformanceAnalytics) #para poder graficar las correlaciones entre variables
#como pearson
library(psych) #para graficar correlaciones tb
library(devtools)#para usar la pìecewise (Ecuaciones SEM) y select variables


library(factoextra)#para poder aplicar el analisis de PCA
library(FactoMineR)#PCA
library(clusterSim)
library(GGally)#öpara matriz de correlaciones, ggcorr
library(Hmisc)
#library(ggbiplot) #graficas

library(skimr) #resumen datos dataframe
library(mgcv) #GAM
library(performance)#metricas de los modelos
library(MuMIn)#compara desempeño de modelos

library(rJava)#hace falta para el glmulti
library(glmulti)#compara desempeño de modelos
library(rsq) #para obtener R2 de glm
library(lmodel2) #para diferentes regresiones lineales
library(lme) #para modelos LME
library(nlme)#para modelos lme (usado en el script)
library(visreg) #grafica resultados

library(flux) #para calcular el area debajo de una curva AUC
library(DescTools) #calcula el area debajo de una curva AUC

library(rpart) #decision tree
library(caret) #machine learning dividir los valores para realizar la validacion cruzada
library(rattle) #para random forest
library(rpart.plot) #graficar decision tree
library(caTools)#crea grupo test y training. entrenar modelos predictivos en decision tree

library(MASS)#trabaja con ramdomforest no categorico
library(ranger) #trabaja con los hiperparámetros de randomforest no categorico
library(ISLR)#random forest categorico

library(tidymodels)#infierno de paquete, reiniciar sesion de r y correrlo de primeras, si hace falta reinstalarlo para RF
library(doParallel)

library(RSAGA)#para analisis de datos espaciales
library(stars)
library(sp) #para poder trabajar con raster
library(raster)#para poder trabajar con imagenes geolocalizadas
library(usdm)# Analisis de incertidumbre para modelos de distribucion de especies (vif)
library(sf)
library(rgdal)
library(tmap)#grafica variables en mapas 

library(downloader)#baja datos de los repositorios directamente de internet
library(rgl) #grafica resultados en 3d
library(magick)# hace girar los gráficos en 3D

library(pls)


