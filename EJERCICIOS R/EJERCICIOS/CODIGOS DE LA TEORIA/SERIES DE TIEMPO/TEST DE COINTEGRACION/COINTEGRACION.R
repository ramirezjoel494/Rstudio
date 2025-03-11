library(lubridate)
library(tseries)
library(car)
library(urca)
library(nlme)
library(mFilter)
library(tidyverse)
library(astsa)
library(foreign)
library(forecast)
library(quantmod)
library(xts)
library(TTR)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)

attach(coint)
names(coint)

#Generamos los logaritmos con las variables a trabajar
lnPCE = log(PCE)
lnDPI = log(DPI)

#Creamos las variables de serie de tiempo
DPI.ts = ts(lnDPI, start = c(1947,1), end = c(2007,4), frequency = 4)
PCE.ts = ts(lnPCE, start = c(1947,1), end = c(2007,4), frequency = 4)
datos1 = cbind(DPI.ts,PCE.ts)
plot(cbind(DPI.ts,PCE.ts), main = "Tendencia")

#Generar modelo
modelo1 = lm(PCE.ts ~ DPI.ts)
summary(modelo1)
residuales = modelo1$residuals
summary(residuales)
residualPlot(modelo1)

adf.test(residuales)

y=ur.df(residuales, type="trend", selectlogs = "AIC")
summary(y)
y@teststat
y@cval
#Si el valor de t es menor en valor absoluto al valor critico del
#los residuales son No estacionarios (tiene raiz unitaria)

y2 =ur.df(residuales, type="drift", selectlogs = "AIC")
summary(y2)

y3 =ur.df(residuales, type="none", selectlogs = "AIC")
summary(y3)

# Prueba de Phillips y ouliaris para cointegracion
# no inmporta cual variable va en lado izquierdo de la ecuacion

prueba.PD = ca.po(datos1, type = "PZ")
summary(prueba.PD)

prueba.PD2 = ca.po(datos1, type = "PD")
summary(prueba.PD2)