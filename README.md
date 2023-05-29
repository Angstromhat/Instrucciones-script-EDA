#Cargar los datos. (Reemplazar la direccion con tu localizacion de los datos)

datos <- read_excel("C:/Users/Angel/Desktop/Proyecto Microglía/R/datos.xlsx")
View(datos)


#Transfomar a factores

datos$phenotype <- factor(datos$phenotype)
datos$sex <- factor(datos$sex)

#Transformar a numerico

datos$C1_area <- as.numeric(datos$C1_area)
datos$C2_area <- as.numeric(datos$C2_area)
datos$C3_area <- as.numeric(datos$C3_area)
datos$phagolysosome_area <- as.numeric(datos$phagolysosome_area)
datos$phagocitosis_area <- as.numeric(datos$phagocitosis_area)
datos$C1_fraction <- as.numeric(datos$C1_fraction)
datos$C2_fraction <- as.numeric(datos$C2_fraction)
datos$C3_fraction <- as.numeric(datos$C3_fraction)
datos$phagolysosome_fraction <- as.numeric(datos$phagolysosome_fraction)
datos$phagocitosis_fraction <- as.numeric(datos$phagocitosis_fraction)

#ANALISIS EXPLORATORIO

install.packages("tidyverse")
library(tidyverse)

#1.Count total missing values in each column
sapply(datos, function(x) sum(is.na(x)))

#summarise the data
summary(datos)






#2.Data visualisation (Repetir el código para las variables necesarias)

library(ggplot2)

#create histogram of phagocitosis 
ggplot(data=datos, aes(x=phagolysosome_area)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("phagosome_area")

ggplot(data = datos, aes(x = phagocitosis_fraction)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50) +
  ggtitle("fagocitosis") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 60))


#create scatterplot of ... vs ..., using sex as color variable
ggplot(data=datos, aes(x=..., y=..., color=...)) + 
  geom_point()
ggplot(data = datos, aes(x = variable_x, y = variable_y, color = variable_color)) +
  geom_point(size = 3) +
  ggtitle("Título del gráfico de dispersión") +
  scale_x_continuous(limits = c(min_value_x, max_value_x)) +
  scale_y_continuous(limits = c(min_value_y, max_value_y))


ggplot(data = datos, aes(x = phenotype, y = phagolysosome_fraction)) +
  geom_point() +
  labs(x = "Phenotype", y = "Phagolysosome_area") +
  ggtitle("Diagrama de Dispersión: Phenotype vs. Phagosome_area")



#4.Correlation test.


### Usando la función cor.test() para el test de correlación de Pearson
cor_test_pearson <- cor.test(datos$phenotype, datos$phagocitosis_fraction, method = "pearson")

# Imprimir los resultados
print(cor_test_pearson)


# Test de Mann-Whitney with approximate method
mann_whitney <- wilcox.test(phagolysosome_area ~ phenotype, data = datos, exact = FALSE)

# Print the results
print(mann_whitney)










#ANOVA CRUZADO CON INTERACCION 



###ANOVA

# Realizar el test ANOVA con interacción
modelo_anova <- aov(phagocitosis_fraction ~ phenotype * sex, data = datos)

# Obtener un resumen de los resultados
resultado_anova <- summary(modelo_anova)

# Imprimir el resumen del ANOVA
print(resultado_anova)


