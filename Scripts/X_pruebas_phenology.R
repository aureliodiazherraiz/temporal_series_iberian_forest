#una vez que tenemos todos los datos de NDVI limpios (trend+season) podemos 
#calcular las métricas fenologicas con greenbrown sabiendoq ue teien conflicto
#con dplyr


#usamos ndvi_allsp_clean que proviene del script de limpieza, para eso antes de 
#transformarlo en una serie temporal habremos de preparar el formato

dt.clean_x <- ndvi_allsp_clean %>%  
  filter(Sp.x %in% c("Pinus pinaster")) %>% 
  dplyr::select(ID,jan_1994:dec_2021)%>% 
  rownames_to_column() %>% 
  pivot_longer(cols=c(jan_1994:dec_2021),
               names_to='N_month',
               values_to='NDVI_clear_month') %>% 
  dplyr::select(-rowname, -N_month) %>% 
  filter(ID==175)

#volvemos a empezar haciendo una lista para poder aplicar la funcion phenology con el NDVI limpio

split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[, col])

dt.clean_x <- split_tibble(dt.clean_x,"ID")


#ahora creamos el archivo temporal de cada uno de ellos 
dt.clean.list.ts <- lapply(dt.clean_x, 
                           function(x) lapply(x, 
                                              function(y) ts(y, freq=12, start=c(1994,1))))

dt.clean.list.ts[[1]][[2]]#para verificar que es una serie temporal

my_list_x <- list()               # Create empty list
for(i in 1:length(dt.clean.list.ts)) {
  mymat <- Phenology(dt.clean.list.ts[[i]][[2]], 
                     tsgf="TSGFlinear", approach="White", trs=0.5)
  my_list_x[[length(my_list_x) + 1]] <- mymat#podemos dejar approach="White" para las especies caducas mientras que para las peremnes dejariamos "Deriv"
  #ademas tsgf="TSGFspline" nos devuelve valores mayores de LOS
}#obtengo una lista de de dataframes

my_list_x[[1]][["los"]]
# para tsgf="TSGFspline", approach="Deriv", method = "Trs", trs= 0.1 y 0.5 es el mismo valor
# 188 175 186 182 179 176 182 179 179 197 194 187 166 164 178 176 183 176 195  NA  NA 179 183 187 192 185 170 101
# 
# tsgf="TSGFspline", approach="White", method = "Trs", trs = 0.1 y 0.5 es el mismo valor
# 179.0 179.5 184.5 171.0 175.0 177.0 180.5 178.5 177.5 189.0 189.0 184.0 167.5 170.5 180.5 180.5 181.0 172.0 186.0    NA    NA 179.5 183.0 182.5 187.0 183.0 173.5 187.5


#tsgf="TSGFlinear", approach="Deriv", method = "Trs", trs = 0.1)
#179.0 182.0 179.5 191.0 182.0 185.5 179.5 185.0 179.5 182.0 174.0 154.0 165.0 146.0 151.5 165.5 154.0 151.5 162.0  74.0    NA 154.5 171.0 183.0 180.0 183.0 191.0 179.5


#tsgf="TSGFlinear", approach="Trs", trs=0.1)
#NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA


#tsgf="TSGFspline", approach="Trs", trs=0.5)
#144.5 155.0 166.0 178.5 221.5 235.0 245.5 256.0 268.0 291.0 319.0 324.0 338.0 359.0  NA NA NA NA NA 337.0 332.5 319.0 328.0 336.5 340.5 353.5    NA    NA

#tsgf="TSGFlinear", approach="Trs", trs=0.5)
#145.5 154.0 163.0 176.0 220.0 234.0 247.0 257.5 268.0 295.5 306.5 315.0 332.0 NA NA NA NA NA NA 319.0 295.5 307.0 316.5 331.0    NA    NA    NA    NA


