#REGRESION LINEAL SIMPLE (Solo se tiene una variable para predecir)

library(tidyverse)
library(boot)
library(car)
library(QuantPsyc)
library(ggplot2)

#Para que me deje manejar el archivo:
attach(sales)
names(sales)

#Para saber el tipo de variable:
class(ventas)
class(Publicidad)

#Modelo de regresion lineal:
modelo1 = lm(ventas ~ Publicidad, data=sales, na.action = na.exclude)
#na.action = na.exclude (por si alguna fila le falta observación)

summary(modelo1)
#Multiple R-squared nos da el porcentaje de confiabilidad de la regresion
# Para que sea confiable, debe ser de alrededor: 0.8 y 1

# GRAFICAR:

grafica1 = ggplot(sales,aes(Publicidad,ventas))

grafica1

grafica1 + geom_point()

grafica1 + geom_point() + geom_smooth(method = "lm",colour = "Red")
