

library(lubridate)
library(tseries)
library(tidyverse)
library(astsa)
library(foreign)
library(forecast)
library(quantmod)
library(xts)
library(TTR)
library(readxl)
library(cellranger)

#Series de Tiempo Univariadas 

#Paso 1. Convertir a objeto de Serie de Tiempo en R

Arimar.ts=ts(Arimar, start=c(2013,1), frequency = 12)

print(Arimar.ts)

class(Arimar.ts)

start(Arimar.ts)
end(Arimar.ts)

plot(Arimar.ts,  main="Serie de tiempo", ylab="Precio", col="red")

serielog=log(Arimar.ts)
serielog
plot(serielog)

#Estacionariedad: Para conocer el número de diferencias que se requieren para lograr que la serie 
#sea estacionaria

ndiffs(Arimar.ts)

#Paso 2.Prueba de DickeyFuller

adf.test(Arimar.ts)

seriedif=diff(Arimar.ts)
plot(seriedif)
acf(seriedif)

ndiffs(seriedif)
adf.test(seriedif)

#Prueba de Dickey Fuller con dos diferencias


seriedif2=diff(Arimar.ts, differences =2)
plot(seriedif2)
adf.test(seriedif2)


#Paso 4: Analisis visual de las graficas


plot(seriedif2, type="o", lty="dashed",main="Serie de Tiempo",col="red")
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(seriedif2)
pacf(seriedif2)
acf(ts(seriedif2, frequency=1))
pacf(ts(seriedif2, frequency=1))

#Modelo Arima

modelo1=arima(Arimar.ts,order=c(1,2,1))
summary(modelo1)
tsdiag(modelo1)
Box.test(residuals(modelo1),type="Ljung-Box")


error=residuals(modelo1)
plot(error)

#Pronosticos Arima
pronostico=forecast::forecast(modelo1,h=10)
pronostico
plot(pronostico)

