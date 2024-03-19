library(ggplot2)
library(dplyr)
library(tidyr)
setwd("C://Users//Alumnos//Documents//Reconocimiento_de_patrones//")
#setwd("C:\Users\Alumnos\Documents\Procesamiento_de_imagenes") 

namefile <- ".//hepatitis.data";
#lectura de los datos:archivo sin cabecera   
dataIris <- read.table(namefile,header=FALSE,sep = ",")
idCols <-c(1,2,3,4,5,15,16,17,18)
dataSel <-dataIris[,idCols]
summary(dataSel)

#nombres de las comlumnas de los datos
dataSelH <- c("Class","AGE","SEX","STEROID","ANTIVIRALS","BILIRUBIN","ALK PHOSPHATE","SGOT","ALBUMIN")
names(dataSel) <- dataSelH
features <- dataSelH[1:9]
print(features)
View(dataSel)
# Descripcion de datos
summary(dataSel)


# convertir columnas a categoricas 
dataSel$Class <- factor(dataSel$Class)
dataSel$SEX <- factor(dataSel$SEX)
dataSel$STEROID <- factor(dataSel$STEROID)
dataSel$ANTIVIRALS <- factor(dataSel$ANTIVIRALS)

dataSel$BILIRUBIN <- as.numeric(dataSel$BILIRUBIN)
dataSel$`ALK PHOSPHATE` <- as.numeric(dataSel$`ALK PHOSPHATE`)
dataSel$SGOT <- as.numeric((dataSel$SGOT))
dataSel$ALBUMIN <- as.numeric(dataSel$ALBUMIN)


# otra forma de convertir
#idFedCat <- c(1,3,4,5)
idFedNum <- c(2,6,7,8,9)
#dataSel[,3] <-factor(dataSel[,3])

summary(dataSel)


# plot - densisda con respecto a la clase
for (varF in idFedNum){
  p <- ggplot(dataSel,aes(x = dataSel[,varF],y=after_stat(density),  fill=Class)) + 
    geom_density(alpha=0.5) +
    labs(title = paste("Distribucion de",varF),x = varF,y="Frecuencia",fill="Group") +
  scale_fill_discrete(labels= c("MUERTO","VIVO"))
  x11();
  print(p)
  sd_var <- sd(dataSel[,varF],na.rm = TRUE)
  mean_var <- mean(dataSel[,varF],na.rm = TRUE)
  cat("Feature:",varF," sd",sd_var, "media ",mean_var,"\n")
}

