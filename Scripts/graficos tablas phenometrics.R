#corrplot 
cor_phenometric <- read_excel("CSV/Tablas metricas datos excel.xlsx", 
                                                   sheet = "r2", col_types = c("text", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric"))
p_phenometric <- read_excel("CSV/Tablas metricas datos excel.xlsx",
                  sheet = "p", col_types = c("text", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric"))



cor_phenometric %>% filter()

a<- as.matrix(p_phenometric[2:10])
b <- as.matrix(cor_phenometric[2:10])
table <- ifelse(a >0.05, NA, b)


a <-as.vector(unlist(cor_phenometric$specie))
rownames(table) <- a

tiff("table_cors.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
corrplot(table, na.label = " ", cl.cex=1, tl.cex=0.7, tl.col = "black", col=colorRampPalette(c("white","red", "lightblue","blue"))(100),
         cl.lim = c(0.7, 1), is.corr = FALSE)
dev.off()


corrplot.mixed(table, upper = "ellipse", lower = "number",
               tl.pos = "lt", tl.col = "black", tl.offset=1, tl.srt = 0)



##
cor_phenometric2 <- read_excel("CSV/Tablas metricas datos excel 2.xlsx", 
                                  sheet = "r2", col_types = c("text", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric"))
p_phenometric2 <- read_excel("CSV/Tablas metricas datos excel 2.xlsx", 
                                sheet = "pvalue", col_types = c("text", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric"))



cor_phenometric2 %>% filter()

a<- as.matrix(p_phenometric2[2:10])
b <- as.matrix(cor_phenometric2[2:10])
table <- ifelse(a >0.05, NA, b)


a <-as.vector(unlist(cor_phenometric2$specie))
rownames(table) <- a

tiff("table2_cors.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
corrplot(table, na.label = " ", cl.cex=1, tl.cex=0.7, tl.col = "black", col=colorRampPalette(c("white","lightblue","blue"))(100),
         cl.lim = c(0.7, 1), is.corr = FALSE)
dev.off()
