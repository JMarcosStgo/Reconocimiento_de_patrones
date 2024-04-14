library(ggplot2)
library(dplyr)
library(tidyr)

# CHANGE
setwd("your//path")

source("funciones.R")

# FILE AVAILABLE IN THE DATA FOLDER
namefile <- ".//Steel_industry_data.csv";

#lectura de los datos:archivo sin cabecera   
dataSID <- read.table(namefile,header=TRUE,sep = ",")
#vizualizacion de datos
View(dataSID)
summary(dataSID)

# convertir clase a categorica porque esta columna de clase es categorica 
dataSID$date <- factor(dataSID$date)
dataSID$WeekStatus <- factor(dataSID$WeekStatus)
dataSID$Day_of_week <- factor(dataSID$Day_of_week)
dataSID$Load_Type <- factor(dataSID$Load_Type)

#descipcion de los datos
summary(dataSID)

# Histograma de datos continuos
namesSID <- c("Usage_kWh","Lagging_Current_Reactive.Power_kVarh","Leading_Current_Reactive_Power_kVarh","CO2.tCO2.","Lagging_Current_Power_Factor","Leading_Current_Power_Factor","NSM")
print(namesSID)
for (varF in namesSID){
  p <- ggplot(dataSID,aes(x = dataSID[,varF],y=after_stat(density),  fill=Load_Type)) + 
    geom_density(alpha=0.5) +
    labs(title = paste("Distribucion de",varF),x = varF,y="Frecuencia",fill="Group")+
    scale_fill_discrete(labels= c("Light_Load", "Maximum_Load", "Medium_Load"))
  x11();
  print(p)
  sd_var <- sd(dataSID[,varF])
  mean_var <- mean(dataSID[,varF])
  cat("Feature:",varF," sd",sd_var, "media ",mean_var,"\n")
}

#histograma para datos categoricos
namesSIDCat <- c("WeekStatus","Day_of_week","Load_Type")
#namesSIDCat <- c("date", "WeekStatus","Day_of_week","Load_Type")

print(namesSIDCat)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for (varF in namesSIDCat){
  p <- ggplot(dataSID, aes_string(x = varF, fill = "Load_Type")) + 
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribucion de", varF), x = varF, y = "Frecuencia", fill = "Group") +
    scale_fill_discrete(labels = c("Light_Load", "Maximum_Load", "Medium_Load"))
  x11()
  print(p)
  
  modaa <- Mode(dataSID[[varF]])
  frecuencia <- table(dataSID[[varF]])
  
  cat("Feature:", varF, " moda", modaa, "frecuencia ", frecuencia, "\n")
}
modaDate <- Mode(dataSID$date)
modaWeekStatus <- Mode(dataSID$WeekStatus)
modaDay_of_week <- Mode(dataSID$Day_of_week)
modaLoad_Type <- Mode(dataSID$Load_Type)


print(modaDate)
print(modaWeekStatus)
print(modaDay_of_week)
print(modaLoad_Type)


#imputacion con valor de la media
  #no contiene datos N/A

# normalizacion de los datos continuos
cuantitaFeat <- seq(from=1,to=7)
namesFeatures <- namesSID[cuantitaFeat]
cat("Nombres de las caracteristicas: ",namesFeatures)
mean_features <- sapply(namesFeatures, function(x) mean(dataSID[[x]]))
sd_features <- sapply(namesFeatures, function(x) sd(dataSID[,x]))
cat("Medias de las caracteristicas",mean_features)
cat("Desviacion estandar de las caracteristicas: ",sd_features)
dataSIDNorm <- sapply(namesFeatures, function(x) normalizeDataL(dataSID[,x],mean_features[x],sd_features[x]))
names(dataSIDNorm) <- namesFeatures
dataSIDNorm <- as.data.frame(dataSIDNorm)
View(dataSIDNorm)


#valores extremos por media y varianza
dataWine_WithoutVE <- dataSIDNorm
N <- dim(dataWine_WithoutVE)[1]
isindexExtremo <- rep(FALSE,N)
for(idFeature in namesFeatures){
  isindexExtremo <- isindexExtremo | isVExtremo(dataWine_WithoutVE[,idFeature],0.0,1.0)
}
cat("Numero de valores extremos", sum(isindexExtremo),"\n")
cat("Registros con valores extremos \n")
print(dataSIDNorm[isindexExtremo,])
dataWine_WithoutVE <- dataSIDNorm[!isindexExtremo,]
print(dataWine_WithoutVE)[1]


# Histograma de datos continuos despues del preprocesamiento
namesSID <- c("Usage_kWh","Lagging_Current_Reactive.Power_kVarh","Leading_Current_Reactive_Power_kVarh","CO2.tCO2.","Lagging_Current_Power_Factor","Leading_Current_Power_Factor","NSM")
print(namesSID)
#agrega la columna Load Type al conjunto normalizado
dataSIDNorm <- cbind(Load_Type = dataSID$Load_Type,dataSIDNorm)
View(dataSIDNorm)
for (varF in namesSID){
  p <- ggplot(dataSIDNorm,aes(x = dataSIDNorm[,varF],y=after_stat(density),  fill=Load_Type)) + 
    geom_density(alpha=0.5) +
    labs(title = paste("Distribucion de",varF),x = varF,y="Frecuencia",fill="Group")+
    scale_fill_discrete(labels= c("Light_Load", "Maximum_Load", "Medium_Load"))
  x11();
  print(p)
  sd_var <- sd(dataSIDNorm[,varF])
  mean_var <- mean(dataSIDNorm[,varF])
  cat("Feature:",varF," sd",sd_var, "media ",mean_var,"\n")
}


