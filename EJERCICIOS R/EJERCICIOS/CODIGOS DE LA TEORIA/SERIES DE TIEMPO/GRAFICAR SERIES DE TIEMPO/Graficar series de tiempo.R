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

attach(preciopm)
names(preciopm)


#Series de Tiempo Univariadas Versus Multivariadas

#Paso 1. Convertir a objeto de Serie de Tiempo en R

preciopma.ts=ts(precio, start = c(1990,1), frequency = 12)

preciopma.ts

class(preciopma.ts)

start(preciopma.ts);end(preciopma.ts)
#Estacionariedad: Para conocer el número de diferencias que se requieren para lograr que la serie 
#sea estacionaria
ndiffs(preciopma.ts)

#Paso 2. Explorar los datos mediante gráficas

plot(preciopma.ts, ylab="Precio", col="blue")


seasonplot(preciopma.ts, col=rainbow(12), year.labels = TRUE)

#Paso 3. función de Autocorrelación
acf(preciopma.ts)
Pacf(preciopma.ts)

seriedif=diff(preciopma.ts)
plot(seriedif)
acf(seriedif)
ndiffs(seriedif)



#analisis visual de las graficas

par(mfrow=c(2,2), mar=c(4,4,4,1)+.1)
plot(preciopma.ts, ylab="Precio")
acf(preciopma.ts, main="Serie No Estacionaria")
plot(seriedif)
acf(seriedif, main="Serie Estacionaria")

