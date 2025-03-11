# SE REQUIERE SABER CUAL DE LAS VARIABLES INDEPENDIENTES ES
#SIGNIFICATIVA PARA LA VARIABLE DEPENDIENTE "SALARY"

#Cargar librerias
library(lubridate)
library(tseries)
library(forecast)
library(tidyverse)
library(car)
library(foreign)
library(timsac)
library(lmtest)
library(dplyr)
library(caTools)
library(caret)
library(ggplot2)
library(glmnet)
library(car)
library(carData)

#Descargar los datos
data("Salaries", package = "carData")

#Inspeccionar los datos
sample_n(Salaries,10)

#Genera el modelo de regresion con variables categóricas en R
#SEX:

modelo1 <-lm(salary~sex, data = Salaries)

#Coeficientes del Modelo
summary(modelo1)$coef
#El valor p asociado a este coeficiente es 5.667107e-03 , 
#que es menor a 0.05. Esto significa que la diferencia en 
#salario entre hombres y mujeres es estadísticamente significativa .

# Para conocerlos códigos que usa R para crear la variable Dummy
contrasts(Salaries$sex)
# 1 si es hombre, 0 para mujer

#Para cambiar las variables Dummy relevel()

Salaries <-  Salaries  %>%
  mutate(sex = relevel(sex, ref = "Male"))

modelo2 <-lm(salary~sex, data = Salaries)
summary(modelo2)$coef

#Para variables categoricas con mas de dos niveles (n-1)
res <- model.matrix(~rank, data = Salaries)
head(res[,-1])

modelo3 <-lm(salary~yrs.service + rank + sex + discipline, data = Salaries)
summary(modelo3)$coef

contrasts(Salaries$rank)
contrasts(Salaries$sex)

# CONCLUSION

#  Variables significativas (pr < 0,05):

#rankAssocProf: Los profesores asociados tienen un impacto significativo en el salario.

#rankProf: Los profesores titulares tienen un impacto significativo en el salario.

#disciplineB: La disciplina B tiene un impacto significativo en el salario.

#  Variables no significativas (pr ≥ 0,05):

#yrs.service: Los años de servicio no tienen un impacto significativo en el salario.

#sexFemale: El género no tiene un impacto significativo en el salario en este modelo.
