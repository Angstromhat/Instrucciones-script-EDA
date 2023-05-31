install.packages("readxl")
library(readxl)
install.packages("tidyverse")
library(tidyverse)
install.packages("conflicted")
library(conflicted)

conflict_prefer("filter", "dplyr")

#Cargar los datos. (Reemplazar la direccion con tu localizacion de los datos)

datos <- read_excel("datos.xlsx")
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


#1.Count total missing values in each column of area
#SELECCIONAMOS DATOS DE ÁREAS

columnas <- c("C1_area", "C2_area", "C3_area", "phagolysosome_area", "phagocitosis_area") 

missing_values <- sapply(datos[columnas], function(x) sum(is.na(x)))

df <- data.frame(Columna = names(missing_values), Missing_Values = missing_values)

boxplot(Missing_Values ~ Columna, data = df, main = "Area Missings")


#1.TENDENCIA CENTRAL

# Filtrar columnas numéricas sin missings
columnas_numericas <- columnas[sapply(datos[columnas], is.numeric) & !sapply(datos[columnas], anyNA)]

# Calcular las estadísticas solo para las columnas numéricas sin missings
resumen <- sapply(datos[columnas_numericas], function(x) c(mean = mean(x), median = median(x), sd = sd(x), min = min(x), max = max(x)))

# Asignar los valores de las estadísticas
media <- resumen["mean", ]
mediana <- resumen["median", ]
desviacion_estandar <- resumen["sd", ]
variacion_percentiles <- c(min = resumen["min", ], max = resumen["max", ])

# Imprimir el resumen
print(media)
print(mediana)
print(desviacion_estandar)


#OTRA OPCIÓN MÁS FÁCIL DE OBTENER LA TENDENCIA CENTRAL
summary(datos)

#2.VISUALIZACIÓN DE VARIABLES EN FUNCIÓN DEL GENOTIPO

# HISTOGRAMA

ggplot(data = datos, aes(x = phagolysosome_area, fill = phenotype)) +
  geom_histogram(color = "black", bins = 20) +
  scale_fill_manual(values = c("J20" = "lightblue", "OE" = "lightgreen")) +
  ggtitle("phagosome_area")

# BLOXPLOT

ggplot(data = datos, aes(x = phenotype, y = phagolysosome_area, fill = phenotype)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  scale_fill_manual(values = c("J20" = "lightblue", "OE" = "lightgreen")) +
  ggtitle("Boxplot de phagosome_area en J20 y OE") +
  scale_x_discrete(limits = c("J20", "OE")) +
  scale_y_continuous(limits = c(0, 50))  

#OTRA OPCIÓN DE OBTENER EL BOXPLOT

datos_J20 <- datos$phagolysosome_area[datos$phenotype == "J20"]
datos_OE <- datos$phagolysosome_area[datos$phenotype == "OE"]

boxplot(datos_J20, datos_OE, names = c("J20", "OE"), ylab = "Valor", main = "Box Plot Phagocytosis area_J20 Y J20-OE")
media_J20 <- mean(datos_J20)
media_OE<- mean(datos_OE)

abline(h = media_J20, col = "red")
abline(h = media_OE, col = "blue")

legend("topright", legend = c("Media J20", "Media OE"), col = c("red", "blue"), lty = 1)

# Test de T-student de la variable en función del genotipo 

J20 <- subset(datos, phenotype == "J20")$phagolysosome_area

OE <- subset(datos, phenotype == "OE")$phagolysosome_area

resultado <- t.test(J20,OE)
print(resultado)

# Crear el gráfico de barras para mostrar en dos columnas la diferencia de los genotipos
media_J20 <- mean(datos_J20)
media_OE<- mean(datos_OE)
barplot(c(media_J20, media_OE), 
        ylim = c(0, max(c(media_J20, media_OE)) + 5),
        ylab = "Media", 
        names.arg = c("J20", "OE"),
        col = c("lightblue", "lightgreen"),
        main = "Comparación mean de phagolysosome_Area")

# Agregar las etiquetas de las medias
text(x = c(1, 2), y = c(media_J20, media_OE), labels = round(c(media_J20, media_OE), 2), pos = 3)

# Agregar información sobre la prueba t
legend("top", legend = "T-STUDENT", bty = "n")


# scatterplot de phagolysosome_Area vs phenotype, usando sexo como variable de color
library(ggplot2)

ggplot(data = datos, aes(x = phenotype, y = phagolysosome_area, color=sex)) +
  geom_point() + scale_color_manual(values = c("lightblue", "lightgreen")) +
  labs(x = "Genotipo", y = "Area") +
  ggtitle("Genotipo vs. Phagosome_area") 
 

#4.Correlation test.

## Usando la función cor.test() para el test de correlación

# Necesario convertir 'datos$phenotype' a numérico 
datos$phenotype <- as.numeric(datos$phenotype)

# Realizar el test de correlación
correlation_test <- cor.test(datos$phenotype, datos$phagocitosis_area)
print(correlation_test)


# Test de Mann-Whitney with approximate method
mann_whitney <- wilcox.test(phagolysosome_area ~ phenotype, data = datos, exact = FALSE)

# Print the results
print(mann_whitney)



#5.ANOVA 

#ANOVA 

# Realizar ANOVA con la variable "phagolysosome_area" y considerando la variable "Phenotype"
modelo_anova <- aov(phagolysosome_area ~ phenotype, data = datos)
resultados_anova <- summary(modelo_anova)
print(resultados_anova)


#ANOVA dos factores con interacción 
modelo_anova <- aov(phagolysosome_area ~ phenotype * sex, data = datos)
resultados_anova <- summary(modelo_anova)
print(resultados_anova)




