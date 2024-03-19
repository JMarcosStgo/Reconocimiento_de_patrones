library(ggplot2)
setwd("C://Users//Alumnos//Documents//Procesamiento_de_imagenes//")
#setwd("C:\Users\Alumnos\Documents\Procesamiento_de_imagenes") 

namefile <- ".//lenses.data";
#lectura de los datos:archivo sin cabecera   
dataLenses <- read.table(namefile,header=FALSE,sep = ",")

#nombres de las comlumnas de los datos
names(dataLenses) <- c("instancia","edad","spec_presc","astigmatic","tear_prod","class")
#dimensiones
dim(dataLenses)
#vizualizar tabla de datos
View(dataLenses)
#descripcion de datos
summary(dataLenses)
#convertir a datos categoricos
dataLenses$class <- factor(dataLenses$class)
dataLenses$edad <-factor(dataLenses$edad)
dataLenses$edad <-factor(dataLenses$edad)
dataLenses$spec_presc <-factor(dataLenses$spec_presc)
dataLenses$astigmatic <-factor(dataLenses$astigmatic)
dataLenses$tear_prod <-factor(dataLenses$tear_prod)
#descripcion de los datos
summary(dataLenses)
#frecuencias clases
freqClass <- table(dataLenses$class)
#frecuencias por clases
print(freqClass[1])
print(freqClass[2])
print(freqClass[3])
#frecuencias por edades
freqEdad <- table(dataLenses$edad)
print(freqEdad[1])
print(freqEdad[2])
print(freqEdad[3])

#por edad
freqEdad_df <- as.data.frame(freqEdad)
names(freqEdad_df) <- c("edad","freq")
print(freqEdad_df)
#histograma de frecuencias
x11()
p <- ggplot(data = freqEdad_df, aes (x=edad,y=freq)) +
    geom_bar(stat="identity",fill="steelblue")
p

# por clases
freqClass_df <- as.data.frame(freqClass)
names(freqClass_df) <- c("class","freq")
print(freqClass_df)
#histograma de frecuencias
x11()
p <- ggplot(data = freqClass_df, aes (x=class,y=freq)) +
  geom_bar(stat="identity",fill="steelblue")
p


