#vamos a limpiar de outliers los datos para el analisis temporal
#partimos del dataframe pheno_metr_sp_p

#vamos a construir un dataframe con todas las metricas que faciliten la eliminacion de los outliers
dt.temp <- pheno_metr_sp_p %>% 
  mutate(pop_grados = ifelse(pop_grados<100, pop_grados+360, pop_grados)) %>% 
  group_by(Sp.x, year) %>% 
  mutate(q1los=quantile(los, .25),q3los=quantile(los, .75), IQRlos = IQR(los),
            inflos=q1los-1.5*IQRlos, suplos=q3los+1.5*IQR,
         q1peak=quantile(peak, .25),q3peak=quantile(peak, .75), IQRpeak = IQR(peak),
         infpeak=q1peak-1.5*IQRpeak, suppeak=q3peak+1.5*IQR,
         q1trough=quantile(trough, .25),q3trough=quantile(trough, .75), IQRtrough = IQR(trough),
         inftrough=q1trough-1.5*IQRtrough, suptrough=q3trough+1.5*IQR,
         q1msp=quantile(msp, .25),q3msp=quantile(msp, .75), IQRmsp = IQR(msp),
         infmsp=q1msp-1.5*IQRmsp, supmsp=q3msp+1.5*IQR,
         q1mau=quantile(mau, .25),q3mau=quantile(mau, .75), IQRmau = IQR(mau),
         infmau=q1mau-1.5*IQRmau, supmau=q3mau+1.5*IQR, 
         q1sos=quantile(sos_grados, .25),q3sos=quantile(sos_grados, .75), IQRsos = IQR(sos_grados),
         infsos=q1sos-1.5*IQRsos, supsos=q3sos+1.5*IQR,
         q1eos=quantile(eos_grados, .25),q3eos=quantile(eos_grados, .75), IQReos = IQR(eos_grados),
         infeos=q1eos-1.5*IQReos, supeos=q3eos+1.5*IQR,
         q1pot=quantile(pot_grados, .25),q3pot=quantile(pot_grados, .75), IQRpot = IQR(pot_grados),
         infpot=q1pot-1.5*IQRpot, suppot=q3pot+1.5*IQR,
         q1pop=quantile(pop_grados, .25),q3pop=quantile(pop_grados, .75), IQRpop = IQR(pop_grados),
         infpop=q1pop-1.5*IQRpop, suppop=q3pop+1.5*IQR) %>% 
  mutate(sos_grados = ifelse(infsos>sos_grados, NA, ifelse(sos_grados<supsos,sos_grados,NA)),
         eos_grados = ifelse(infeos>eos_grados, NA, ifelse(eos_grados<supeos, eos_grados, NA)),
         pot_grados = ifelse(infpot>pot_grados, NA, ifelse(pot_grados<suppot, pot_grados, NA)),
         pop_grados = ifelse(infpop>pop_grados, NA, ifelse(pop_grados<suppop, pop_grados, NA)),
         los = ifelse(inflos>los, NA, ifelse(los<suplos, los, NA)),
         peak = ifelse(infpeak>peak, NA, ifelse(peak<suppeak, peak, NA)),
         trough = ifelse(inftrough>trough, NA,ifelse(trough<suptrough, trough, NA)),
         msp = ifelse(infmsp>msp, NA, ifelse(msp<supmsp, msp, NA)),
         mau = ifelse(infmau>mau, NA,ifelse(mau<supmau, mau, NA)))
  
# si decidiesemos seguir el camino de los outliers circulares
prueva <- pheno_metr_sp_p %>% 
  filter(Sp.x=="Castanea sativa" & year==1999) %>% 
  dplyr::select(ID, sos_rads) 
Huberized(prueva$sos_rads)
