library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ChannelAttribution)
library(markovchain)
library(devtools)
library(diagram)
library(Gmisc)
#library(igraph)
setwd("C:/Atribución")

pcb_dataset_final <- read_csv("mta_prueba.csv", 
                              col_types = cols(userId = col_character()))

df2<-pcb_dataset_final#L[sample(nrow(pcb_dataset_final), 10^6), ]


#df2$channel <- df2$channelGrouping
df2$client_id <- df2$userId
#df2$channelGrouping<-NULL
#df2$userId <-NULL
df2$conv<-ifelse(df2$transactions==1,1,0)
df2$conv_null<-ifelse(df2$transactions==1,0,1)
#df2$transactions<-NULL


# aggregating channels to the paths for each customer
df2 <- df2 %>%
  arrange(client_id, date) %>%
  group_by(client_id) %>%
  summarize(path = paste(channelGrouping, collapse = ' > '),
            numero =n(),
            conv = max(conv),
            conv_null = max(conv_null),
            conv_num=sum(conv)) %>%
  ungroup()
# Best Way
df_sum <- df2 %>%
  arrange(path) %>%
  group_by(path,numero) %>%
  summarize(eventos_num =n(),
            conv_num=sum(conv)) %>%
  ungroup()

df_sum$prob <- df_sum$conv_num/df_sum$eventos_num

#df_sum <- df_sum%>%arrange(-prob)%>%filter(eventos_num>100)
df_sum <- df_sum%>%arrange(-prob)%>%filter(numero>=1 & eventos_num >25)


# calculating the models (Markov and heuristics)
mod1 <- markov_model(df2,
                     var_path = 'path',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE,
                     order = 1,
                     ncore = 7) #aumentar el n?mero de simulaciones hace que el resultado sea m?s exacto pero tarda m?s

summary(mod1)


#podemos mejorar el modelo penalizando aquellos usuarios que necesitaron m?s eventos para convertir... 
#esto se podr?a incluso mejorar teniendo en cuenta no s?lo el n?mero de eventos sino el tiempo desde el primer evento y la conversi?n

lambda <-0.15 # valores entre 0 y 1. cuanto m?s  peque?o menor es la pena?izaci?n por numero de eventos, usar valores muy peque?itos


#para ve rla penalizacion elegida

####
vect <- 1:15
vect2 <- exp(-lambda*vect)
plot(vect, vect2)
#####

df2$conv_penaliz <- (df2$conv * exp(-lambda*(df2$numero-1)))

str(df2)

mod2 <- markov_model(df2,
                     var_path = 'path',
                     var_value = 'conv_penaliz',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE,
                     order = 1, 
                     ncore= 7) #aumentar el n?mero de simulaciones hace que el resultado sea m?s exacto pero tarda m?s

summary(mod2)

# heuristic_models(): last click, linear...

mod_h <- heuristic_models(df2,
                          var_path = 'path',
                          var_conv = 'conv'
                          # var_null = 'conv_null',
                          # out_more = TRUE,
                          # order = 1
)



resultados_modelos <- merge( mod1$result, mod2$result , by.x = 'channel_name', by.y = 'channel_name')%>%rename(markov = total_conversions.x, markov_penaliz = total_conversions.y)

resultados_modelos <- merge( resultados_modelos, mod_h , by.x = 'channel_name', by.y = 'channel_name')



resultados_modelos_2=melt(resultados_modelos %>% select(-total_conversion_value),id="channel_name")

ggplot(resultados_modelos_2, aes(variable, value, fill = channel_name)) +
  ggtitle("")+
  geom_bar(stat="identity", position = "dodge") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=14)) +
  theme(plot.title=element_text(size=18)) +
  theme(legend.title = element_blank()) +
  ylab("Atrib") +
  xlab("Method")+
  theme_economist()

ggplot(resultados_modelos_2, aes(channel_name, value, fill = variable)) +
  ggtitle("")+
  geom_bar(stat="identity", position = "dodge") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=14)) +
  theme(plot.title=element_text(size=18)) +
  theme(legend.title = element_blank()) +
  ylab("Atrib") +
  xlab("Channel")+
  theme_economist()




resultados_modelos_PCT <- resultados_modelos%>% group_by(channel_name)%>%
  transform(markov = markov/sum(markov),
            markov_penaliz = markov_penaliz/sum(markov_penaliz),
            first_touch = first_touch/sum(first_touch),
            last_touch = last_touch/sum(last_touch),
            linear_touch = linear_touch/sum(linear_touch))


resultados_modelos_PCT[2:7] <- (resultados_modelos_PCT[2:7] * 100) %>%round(., digits  =2)

write.table(resultados_modelos_PCT, "resultados_modelos_PCT.csv", sep = ";", dec = ",", row.names = FALSE, fileEncoding = "UTF8" )


#LO DIBUJAMOS

resultados_modelos_PCT_2=melt(resultados_modelos_PCT %>% select(-total_conversion_value),id="channel_name")

ggplot(resultados_modelos_PCT_2, aes(variable, value, fill = channel_name)) +
  ggtitle("")+
  geom_bar(stat="identity", position = "dodge") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=14)) +
  theme(plot.title=element_text(size=18)) +
  theme(legend.title = element_blank()) +
  ylab("%Atrib") +
  xlab("Method")+
  theme_economist()


ggplot(resultados_modelos_PCT_2, aes(channel_name, value, fill = variable)) +
  ggtitle("")+
  geom_bar(stat="identity", position = "dodge") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=14)) +
  theme(plot.title=element_text(size=18)) +
  theme(legend.title = element_blank()) +
  ylab("%Atrib") +
  xlab("Channel")+
  theme_economist()


#####Se puede calcular Markow de segundo orden... los resultados no var?an y tarda un poco m?s

mod_order_2 <- markov_model(df2,
                            var_path = 'path',
                            var_conv = 'conv',
                            var_null = 'conv_null',
                            out_more = TRUE,
                            order = 2,
                            ncore= 7) #aumentar el n?mero de simulaciones hace que el resultado sea m?s exacto pero tarda m?s

summary(mod_order_2)

resultados_modelos_or2_PCT <- mod_order_2$result%>% group_by(channel_name)%>%
  transform(total_conversions = total_conversions/sum(total_conversions))%>%
  rename(markov_2or = total_conversions)

#resultados_modelos_PCT[2:6] <- lapply(resultados_modelos_PCT[2:6], sprintf, fmt = "%1.2f%%")

resultados_modelos_or2_PCT$markov_2or<- round(resultados_modelos_or2_PCT$markov_2or*100, digits  =2)



############# TODO ESTO ES PARA DIBUJAR EL GRAFO ###########
# extracting the results of attribution
df_res1 <- mod1$result

# extracting a transition matrix
df_trans1 <- mod1$transition_matrix
df_trans1 <- dcast(df_trans1, channel_from ~ channel_to, value.var = 'transition_probability')


resultados_modelos <- merge( mod1$result, mod_h , by.x = 'channel_name', by.y = 'channel_name')%>%rename(markov = total_conversions)



### plotting the Markov graph ###
df_trans <- mod1$transition_matrix



# adding dummies in order to plot the graph
df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       transition_probability = c(0, 1, 1))
df_trans <- rbind(df_trans, df_dummy)




# ordering channels
df_trans$channel_from <- factor(df_trans$channel_from)
df_trans$channel_to <- factor(df_trans$channel_to)
df_trans <- dcast(df_trans, channel_from ~ channel_to, value.var = 'transition_probability')

# creating the markovchain object
trans_matrix <- matrix(data = as.matrix(df_trans[, -1]),
                       nrow = nrow(df_trans[, -1]), ncol = ncol(df_trans[, -1]),
                       dimnames = list(c(as.character(df_trans[, 1])), c(colnames(df_trans[, -1]))))

trans_matrix[is.na(trans_matrix)] <- 0

trans_matrix1 <- new("markovchain", transitionMatrix = trans_matrix)

# plotting the graph
#https://igraph.org/r/doc/plot.common.html


##cambiamos nombres de los nodos

df_index <- data.frame(index=c(1:14),mod1$result$channel_name)
df_index$index<-df_index$index%>%as.character()
df_index <- df_index%>%arrange(index)


trans_matrix1@states[4:17] <- df_index$mod1.result.channel_name
colnames(trans_matrix1@transitionMatrix)[4:17] <- df_index$mod1.result.channel_name
rownames(trans_matrix1@transitionMatrix)[4:17] <- df_index$mod1.result.channel_name

plot(trans_matrix1, vertex.size=4,vertex.label.dist=1.5, label.cex= .5, 
     vertex.color="green", edge.arrow.size=0.15, color= "SkyBlue2", lty="dotted", frame.color= "blue", 
     frame = TRUE, asp=1, main ="Markov Model", curved = FALSE  )

htmlTable(trans_matrix1@transitionMatrix,)

melt(trans_matrix1@transitionMatrix)

ggplot(melt(trans_matrix1@transitionMatrix), aes(Var2, Var1, fill= value)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") 








#####Se puede calcular Markow de segundo orden... los resultados no var?an y tarda un poco m?s

mod_order_10 <- markov_model(df2,
                            var_path = 'path',
                            var_conv = 'conv_penaliz',
                            var_null = 'conv_null',
                            out_more = TRUE,
                            order = 4,
                            ncore=7) #aumentar el n?mero de simulaciones hace que el resultado sea m?s exacto pero tarda m?s

summary(mod_order_10)


auto_mod <- auto_markov_model(df2,
                             var_path = 'path',
                             var_conv = 'conv_penaliz',
                             var_null = 'conv_null',
                             out_more = TRUE,
                             ncore=7) #aumentar el n?mero de simulaciones hace que el resultado sea m?s exacto pero tarda m?s

summary(auto_mod)

auto_mod$result
mod1$result