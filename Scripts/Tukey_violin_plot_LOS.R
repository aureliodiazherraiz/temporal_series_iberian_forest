#vamos a inserir las letras del analisis Tukey post-hoc: https://stackoverflow.com/questions/44712185/tukeys-post-hoc-on-ggplot-boxplot
#
##los_violin
los_plot2 <- ggplot(data = pheno_metr_p %>% 
                      filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                         "Pinus sylvestris","Pinus nigra", 
                                         "Eucalyptus camaldulensis","Quercus ilex", 
                                         "Quercus suber", "Olea europaea", "Castanea sativa")) %>% 
                      mutate(leaf=ifelse(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                                     "Pinus sylvestris","Pinus nigra", 
                                                     "Eucalyptus camaldulensis","Quercus ilex", 
                                                     "Quercus suber", "Olea europaea"), "Evergreen", "Deciduous")) %>% 
                      mutate(Species = dplyr::recode(Sp.x,"Pinus halepensis" = "P.halepenis",
                                                     "Pinus pinea" = "P. pinea",
                                                     "Pinus pinaster" = "P. pinaster",
                                                     "Pinus sylvestris"="P. sylvestris",
                                                     "Pinus nigra"="P. nigra",
                                                     "Eucalyptus camaldulensis"="E. camaldulensis",
                                                     "Quercus ilex"="Q. ilex",
                                                     "Quercus suber"="Q.suber",
                                                     "Quercus faginea"="Q.faginea",
                                                     "Quercus canariensis"="Q.canariensis",
                                                     "Olea europaea"="O. europaea",
                                                     "Castanea sativa"="C. sativa")),
                    aes(x = Species, y = los, color = leaf,)) +
  geom_violin() +
  #geom_jitter(color="black", size=0.8, alpha=0.9) +
  ylab("Number of Days of LOS")+
  theme_bw()+
  stat_compare_means()+
  ggtitle("Species Length Of Season (LOS)")+
  theme(legend.position="bottom")

los_plot2

ggsave(los_plot2, device = "tiff", path = path2grafic, filename = "los_violin_plot.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_plot2, device = "png", path = path2grafic, filename = "los_violin_plot.png", 
       width = 12, height = 5, units = 'in', dpi = 300)



#siguiendo las recomendaciones de la pagina web

generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

#obtenemos el modelo del LOS 
ANOVA=aov(los ~ Sp.x, pheno_metr_p %>% 
            filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                               "Pinus sylvestris","Pinus nigra", 
                               "Eucalyptus camaldulensis","Quercus ilex", 
                               "Quercus suber", "Olea europaea", "Castanea sativa")) %>%  
  mutate(Sp.x = dplyr::recode(Sp.x,"Pinus halepensis" = "P. halepensis",
                                 "Pinus pinea" = "P. pinea",
                                 "Pinus pinaster" = "P. pinaster",
                                 "Pinus sylvestris"="P. sylvestris",
                                 "Pinus nigra"="P. nigra",
                                 "Eucalyptus camaldulensis"="E. camaldulensis",
                                 "Quercus ilex"="Q. ilex",
                                 "Quercus suber"="Q.suber",
                                 "Olea europaea"="O. europaea",
                                 "Castanea sativa"="C. sativa")))


# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, conf.level=0.95)

library(multcompView)
labels<-generate_label_df(TUKEY, "Sp.x")#generate labels using function

names(labels)<-c('Letters','Species')#rename columns for merging

yvalue<-aggregate(.~Sp.x, data=pheno_metr_p%>% 
                    filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                                       "Pinus sylvestris","Pinus nigra", 
                                       "Eucalyptus camaldulensis","Quercus ilex", 
                                       "Quercus suber", "Olea europaea", "Castanea sativa")), mean)# obtain letter position for y axis using means

yvalue <- yvalue %>% 
  mutate(Sp.x = dplyr::recode(Sp.x,"Pinus halepensis" = "P. halepensis",
                                 "Pinus pinea" = "P. pinea",
                                 "Pinus pinaster" = "P. pinaster",
                                 "Pinus sylvestris"="P. sylvestris",
                                 "Pinus nigra"="P. nigra",
                                 "Eucalyptus camaldulensis"="E. camaldulensis",
                                 "Quercus ilex"="Q. ilex",
                                 "Quercus suber"="Q.suber",
                                 "Olea europaea"="O. europaea",
                                 "Castanea sativa"="C. sativa"))
  
final<-left_join(labels,yvalue, by=c("Species"="Sp.x")) #merge dataframes

los_plot_violin_tukey <- ggplot(pheno_metr_p %>% 
         filter(Sp.x %in% c("Pinus halepensis","Pinus pinea","Pinus pinaster",
                            "Pinus sylvestris","Pinus nigra", 
                            "Eucalyptus camaldulensis","Quercus ilex", 
                            "Quercus suber", "Olea europaea", "Castanea sativa")) %>% 
         mutate(Sp.x = dplyr::recode(Sp.x,"Pinus halepensis" = "P. halepensis",
                                        "Pinus pinea" = "P. pinea",
                                        "Pinus pinaster" = "P. pinaster",
                                        "Pinus sylvestris"="P. sylvestris",
                                        "Pinus nigra"="P. nigra",
                                        "Eucalyptus camaldulensis"="E. camaldulensis",
                                        "Quercus ilex"="Q. ilex",
                                        "Quercus suber"="Q.suber",
                                        "Olea europaea"="O. europaea",
                                        "Castanea sativa"="C. sativa")), aes(x = Sp.x, y = los)) +
  geom_blank() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = 'Species', y = 'LOS (Days)') +
  ggtitle(expression(atop(bold("Lenght of season between species"), atop(italic("(Days)"), "")))) +
  theme(plot.title = element_text(hjust = 0.5, face='bold')) +
  #geom_boxplot(fill = 'green2', stat = "boxplot") +
  geom_violin()+
  geom_text(data = final, aes(x = Species, y = los, label = Letters),vjust=-7.5,hjust=-1.2) +
  theme(plot.title = element_text(vjust=-0.6))


ggsave(los_plot_violin_tukey, device = "tiff", path = path2grafic, filename = "los_plot_violin_tukey.tiff", 
       width = 12, height = 5, units = 'in', dpi = 300, compression = 'lzw')
ggsave(los_plot_violin_tukey, device = "png", path = path2grafic, filename = "los_plot_violin_tukey.png", 
       width = 12, height = 5, units = 'in', dpi = 300)












