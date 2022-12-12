
#### Como crear dummies ####

## Tenemos una tabla con una columna con los dias de la semana
data <- as.data.frame(c("lunes", "martes", "miercoles", "jueves", "viernes", "sabado", "domingo")) 
colnames(data)[1] <- "weekday" # La columna se llama weekday

# Crea dummy para cada dia de la semana (ordena alfabeticamente las columnas)
data <- (model.matrix(~data$weekday-1)%>%as.data.frame%>%cbind(data,.))

# por ultimo renombrar las columnas (opcional)
names(data)[2:8] <- c("domingo", "jueves","lunes", "martes", "miercoles", "sabado", "viernes")



