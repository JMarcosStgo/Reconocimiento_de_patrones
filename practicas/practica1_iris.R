library(ggplot2)
library(dplyr)
library(tidyr)

# CHANGE
setwd("your//path")

# FILE AVAILABLE IN THE DATA FOLDER
namefile <- ".//iris.data";

#lectura de los datos:archivo sin cabecera   
dataIris <- read.table(namefile,header=FALSE,sep = ",")
summary(dataIris)
#nombres de las comlumnas de los datos
nameIrisD <- c("SepalL","sepalW","petalL","petalW","clase")
names(dataIris) <- nameIrisD
features <- nameIrisD[1:4]
print(features)
View(dataIris)
# Descripcion de datos
summary(dataIris)

# convertir clase a categorica porque esta columna de clase es categorica 
dataIris$clase <- factor(dataIris$clase)

#descipcion de los datos
summary(dataIris)

#histogramas
for (varF in features){
  p <- ggplot(dataIris,aes(x = dataIris[,varF])) + 
    geom_histogram(binwidth = 1, fill="blue",color="black") +
    labs(title = paste("Histograma de",varF),x = varF,y="Frecuencia")
  x11();
  print(p)
  sd_var <- sd(dataIris[,varF])
  mean_var <- mean(dataIris[,varF])
  cat("Feature:",varF," sd",sd_var, "media ",mean_var,"\n")
}

# histogramas: otra forma con respecto a la clase
for (varF in features){
  p <- ggplot(dataIris,aes(x = dataIris[,varF],fill=clase)) + 
    geom_histogram(binwidth = 1, alpha=0.5, position="identity") +
    labs(title = paste("Distribucion de",varF),x = varF,y="Frecuencia",fill="Group")
    scale_fill_discrete(labels= c("Setosa","Versicolor","Virginica"))
  x11();
  print(p)
  sd_var <- sd(dataIris[,varF])
  mean_var <- mean(dataIris[,varF])
  cat("Feature:",varF," sd",sd_var, "media ",mean_var,"\n")
}


# plot - densisda con respecto a la clase
for (varF in features){
  p <- ggplot(dataIris,aes(x = dataIris[,varF],y=after_stat(density),  fill=clase)) + 
    geom_density(alpha=0.5) +
    labs(title = paste("Distribucion de",varF),x = varF,y="Frecuencia",fill="Group")
  scale_fill_discrete(labels= c("Setosa","Versicolor","Virginica"))
  x11();
  print(p)
  sd_var <- sd(dataIris[,varF])
  mean_var <- mean(dataIris[,varF])
  cat("Feature:",varF," sd",sd_var, "media ",mean_var,"\n")
}
