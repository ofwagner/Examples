library("pxR")
my.px.object <- read.px("mapas/0006.px")
my.px.data   <-  as.data.frame(my.px.object)
my.px.data$país.de.nacimiento<- gsub("`|\\'", "", 
                            iconv((my.px.data$país.de.nacimiento), 
                                  to="ASCII//TRANSLIT")) 


library(reshape)
my.px.data2 <- cast(my.px.data, sección+sexo~país.de.nacimiento, sum)

