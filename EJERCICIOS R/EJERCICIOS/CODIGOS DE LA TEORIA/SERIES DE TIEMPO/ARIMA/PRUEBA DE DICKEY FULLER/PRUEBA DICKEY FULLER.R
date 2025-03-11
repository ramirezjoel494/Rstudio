# LA PRUEBA DE DICKEY FULLER SE USA PARA SABER SI LA SERIE ES 
# ESTACIONARIA

library(lubridate)
library(tseries)
library(lubridate)
library(tidyverse)
library(car)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(MASS)
library(forecast)
library(fpp2)
library(urca)
library(quantmod)

#LLamamos al archivo
attach(coint)
names(coint)

#Generar Logaritmos(PARA PODER MANEJAR LOS VALORES ALTOS)
lnPCE=log(PCE)
lnDPI=log(DPI)

#Generar variables de Series de Tiempo
DPI.ts=ts(lnDPI, start=c(1947,1),frequency = 4)
PCE.ts=ts(lnPCE, start=c(1947,1),frequency = 4)

#Hacer una tabla
datos1=cbind(DPI.ts,PCE.ts)
datos1
plot(datos1, main="Tendencia")

#Generar Modelo

modelo1=lm(PCE.ts ~DPI.ts)
summary(modelo1)

#ERRORES PARA LA PRUEBA DE DICKEY
residuales=modelo1$residuals
residualPlot(modelo1)
#Si los valores rondan la media, es estacionaria

summary(residuales)


#Prueba de Dickey-Fuller para raiz unitaria (ur)
y=ur.df(residuales)
summary(y)
y@teststat
y@cval
#Se toma el valor critico de 1pct y se compara con el value 
# of test static (t). Si el del 1pct < t es estacionaria

#con intercepto
y2=ur.df(residuales, type="drift",selectlags="AIC")
summary(y2)


#Sin tendencia y sin intercepto
y3=ur.df(residuales, type="none",selectlags="AIC")
summary(y3)

#Dickey Fuller Aumentada
adf.test(residuales)
