dt.sos <- pheno_sos %>% 
  dplyr::select(-var) 

colnames(dt.sos)[1:28]<- c(1994:2021)

dt.sos <- dt.sos %>% 
  pivot_longer(cols=c(`1994`:`2021`),
               names_to='N_year',
               values_to='sos_year') 

dt.sos <- split_tibble(dt.sos, "ID")

#ahora creamos el archivo temporal de cada uno de ellos 
dt.sos.ts <- lapply(dt.sos, 
                           function(x) lapply(x, 
                                              function(y) ts(y, freq=12, start=c(1994,1))))

dt.sos.ts.corr <- lapply(dt.sos.ts, 
                         function(x) CorrectDOY(x[["sos_year"]], check.outliers = TRUE))

dt.sos.corr <- as.data.frame(do.call("rbind", dt.sos.ts.corr)) %>% 
  rownames_to_column("ID") %>% mutate(ID = as.numeric(ID))
colnames(dt.sos.corr)[2:29]<- c(1994:2021)

summary(dt.sos.corr)#continua habiedno valores negativos y NAs


###############################
#### lo intentamos com eos ####

dt.eos <- pheno_eos %>% 
  dplyr::select(-var)

summary(dt.eos)

colnames(dt.eos)[1:28]<- c(1994:2021)

dt.eos <- dt.eos %>% 
  pivot_longer(cols=c(`1994`:`2021`),
               names_to='N_year',
               values_to='eos_year')


dt.eos <- split_tibble(dt.eos, "ID")

#ahora creamos el archivo temporal de cada uno de ellos 
dt.eos.ts <- lapply(dt.eos, 
                    function(x) lapply(x, 
                                       function(y) ts(y, freq=12, start=c(1994,1))))

dt.eos.ts.corr <- lapply(dt.eos.ts, 
                         function(x) CorrectDOY(x[["eos_year"]], check.outliers = TRUE))

dt.eos.corr <- as.data.frame(do.call("rbind", dt.eos.ts.corr)) %>% 
  rownames_to_column("ID") %>% mutate(ID = as.numeric(ID))

colnames(dt.eos.corr)[2:29]<- c(1994:2021)

summary(dt.eos.corr)#continua habiedno valores negativos y NAs












