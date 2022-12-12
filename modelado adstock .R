
##### Carga Librerias #####

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base", 
                      "package:RevoUtilsMath", "package:RevoUtils", "package:RevoIOQ", "package:MicrosoftR",  
                      "package:tools", "package:RUnit" )
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

library(magrittr)
library(R.utils)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(corrgram)

#CARGAMOS FUNCIONES

F_adstock <- function(v1,l,lag=0){
  ads <- stats::filter(v1, l, method = "recursive")%>%as.numeric
  ads2 <- rep(NA, length(ads)) 
  ads2[(1+lag):length(ads)] <- ads[1:(length(ads)-lag)]
  ads2   %>% return}


F_adstock_log <- function(v1,l, base_log =exp(1), adj_log=1,lag=0 ){
  ads <- stats::filter(v1, l, method = "recursive")%>%as.numeric
  ads <- log(ads*adj_log + 1, base_log)
  ads2 <- rep(NA, length(ads)) 
  ads2[(1+lag):length(ads)] <- ads[1:(length(ads)-lag)]
  ads2   %>% return}

F_adstock_tanh <- function(v1,l, adj_tanh=1,lag=0 ){
  ads <- stats::filter(v1, l, method = "recursive")%>%as.numeric
  ads <- tanh(ads/adj_tanh)
  ads2 <- rep(NA, length(ads)) 
  ads2[(1+lag):length(ads)] <- ads[1:(length(ads)-lag)]
  ads2   %>% return}

F_adstock_atan <- function(v1,l, adj_atan=1,lag=0 ){
  ads <- stats::filter(v1, l, method = "recursive")%>%as.numeric
  ads <- atan(ads/adj_atan)
  ads2 <- rep(NA, length(ads)) 
  ads2[(1+lag):length(ads)] <- ads[1:(length(ads)-lag)]
  ads2   %>% return}



#Prueba
advertising = c(117.913, 120.112, 125.828, 115.354, 177.090, 141.647, 137.892, 0.000, 0.000, 0.000, 0.000, 
                      0.000, 0.000,   0.000,   0.000,   0.000,   0.000, 158.511, 109.385, 91.084, 79.253, 102.706, 
                      78.494, 135.114, 114.549, 87.337, 107.829, 125.020, 82.956, 60.813, 83.149, 0.000, 0.000, 
                      0.000, 0.000, 0.000, 0.000, 129.515, 105.486, 111.494, 107.099, 0.000, 0.000, 0.000, 
                      0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000)


(advertising/100) %>%plot
F_adstock_tanh(advertising,l=0.5, adj_tanh=1,lag=2 ) %>%lines
F_adstock_tanh(advertising,l=0.5, adj_tanh=10,lag=2 ) %>%lines
F_adstock_tanh(advertising,l=0.5, adj_tanh=100,lag=2 ) %>%lines

(advertising/100) %>%plot
F_adstock_atan(advertising,l=0.5, adj_atan=1,lag=2 ) %>%lines
F_adstock_atan(advertising,l=0.5, adj_atan=10,lag=2 )%>%lines
F_adstock_atan(advertising,l=0.5, adj_atan=100,lag=2 )%>%lines


advertising %>%plot
F_adstock(advertising,l=0.5,lag=2  ) %>%lines
F_adstock_log(advertising,l=0.5,lag=2, base_log =1.5  ) %>%lines
F_adstock_log(advertising,l=0.5,lag=2, base_log =1.1  ) %>%lines
F_adstock_log(advertising,l=0.5,lag=2, base_log =1.05  ) %>%lines



##### Carga Tablon #####

input_df <- cbind(TABLON_df)



str(input_df)

##### Creamos Dummy para estacionalidad ####

input_df$semana <- substr(input_df$ano.semana,5,6)
input_df$mes <- substr(input_df$ano.mes,5,6)

head(input_df$semana)
head(input_df$mes)


#### Miramos principales variables ####

input_main_df <- input_df%>% dplyr::select(#VARIABLES A METER AL  MODELO
  )




#CREAMOS DUMMIES DE SEMANA Y DE MES
semana<-input_main_df$semana
mes<-input_main_df$mes
input_main_df <- (model.matrix(~semana-1)%>%as.data.frame%>%cbind(input_main_df,.))
input_main_df <- (model.matrix(~mes-1)%>%as.data.frame%>%cbind(input_main_df,.))



#correlaciones

cor(input_main_df %>%dplyr::select(-ano.semana,-semana, -mes))

corrgram(input_main_df[,1:30] %>%dplyr::select(-ano.semana,-semana, -mes), order=NULL, lower.panel=corrgram::panel.ellipse, upper.panel=panel.cor, diag.panel=panel.density)

#repetimos el Tree

library (rpart)
library(rpart.plot)

# Y es nuestro target

Y <- ...

tree2 <- rpart(data=(input_main_df %>%dplyr::select(-ano.semana,-semana, -mes)),
              formula= Y~., control = rpart.control(cp=0.001))




importancia_variables_2 <- tree2$variable.importance%>%as.data.frame%>%add_rownames("variable")%>%
  rename(importancia=".") 



#vemos como es el target en el tiempo y las otras variables más importantes

(ggplot((input_main_df %>%dplyr::select(ano.semana, Y)), aes(x=ano.semana, y=Y, group = 1)) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))%>%ggplotly






