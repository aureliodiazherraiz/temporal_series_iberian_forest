#ahora estudiaremos las correlaciones cruzadas y el analisis kendall

#usaremos el dataframe clim94_20 

#ejemplo

ndvi_cross <- ndvi %>% 
  filter(ID==2) %>% 
  dplyr::select(`1994_jan`:`2020_dez`) 

ndvi_cross <- pull(ndvi_cross[1,])


clim94_20_cross <- clim94_20 %>% 
  filter(column_label==1) %>% 
  dplyr::select(-lon, -lat,-nm, -pre, -column_label) %>% 
  pivot_wider(names_from = c(Year, Month), values_from = tmp) 


ccf(ndvi_cross, clim94_20_cross)
