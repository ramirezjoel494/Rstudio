library(lubridate)
library(tseries)
library(tidyverse)
library(astsa)
library(foreign)
library(forecast)
library(quantmod)
library(xts)
library(TTR)

#PRECIO DEL PETROLEO A PARTIR DE PRECIOS DE AÑOS ANTERIORES

# Cargar datos (asegúrate de que 'Arimar' esté definido)
Arimar <- read.csv("C:/Users/User/Desktop/EJERCICIOS R/EJERCICIOS/CODIGOS DE LA TEORIA/SERIES DE TIEMPO/Arimar.csv")  # Cambia la ruta al archivo
Arimar <- Arimar$Precio  # Selecciona la columna adecuada

#Paso 1. Convertir a objeto de Serie de Tiempo en R

# año = 2013 , mes = 1, frecuencia = 12 meses
Arimar.ts=ts(Arimar, start=c(2013,1), frequency = 12)

#ver la serie de tiempo
print(Arimar.ts)

class(Arimar.ts)

# Inicio de la serie
start(Arimar.ts)

#Fin de la serie
end(Arimar.ts)

# Mostrar grafico de serie de tiempo
plot(Arimar.ts,  main="Serie de tiempo", ylab="Precio", col="red")

#Transformo los valores a escala log-log
serielog=log(Arimar.ts)
serielog

plot(serielog)

#ESTACIONARIEDAD: 
#Para conocer el número de diferencias que se requieren para lograr que la serie 
#sea estacionaria

# Cuantas diferencias necesito para lograr estacionariedad:
ndiffs(Arimar.ts)





#Prueba de Dickey-Fuller (si p > 0.05 no es estacionaria)
adf.test(Arimar.ts)

#DIFERENCIACION PARA LOGRAR ESTACIONARIEDAD

#Primera diferencia:
seriedif=diff(Arimar.ts)
plot(seriedif)

#Prueba de dickey-fuller para ver si ya se logró estacionariedad
acf(seriedif)

ndiffs(seriedif)
adf.test(seriedif)

#Prueba de Dickey Fuller con dos diferencias (ya que, no bastó con na al ser p-value>0.05)

seriedif2=diff(Arimar.ts, differences =2)
plot(seriedif2)
adf.test(seriedif2)


#Paso 4: Analisis visual de las graficas

# Grafico mas bonito la grafica anterior
plot(seriedif2, type="o", lty="dashed",main="Serie de Tiempo",col="red")

# Para que haga el grafico en una sola ventana acf y pacf:
#mfrow=c(2,1): dos filas una columna
#mar=c(4,4,4,1)+.1: margenes

par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)

#Funcion de autocorrelacion
acf(seriedif2)

#Funcion de autocorrelacion parcial
pacf(seriedif2)

#Para que en el grafico, los rezagos coincidan con las frecuencias
acf(ts(seriedif2, frequency=1))
pacf(ts(seriedif2, frequency=1))

#Modelo Arima
#Sabiendo cuantos autorregresivos, medias moviles y rezagos hay, planteo el modelo ARIMA
#order=c(1,2,1): 1 Autorregresivo 2 diferencias 1 media movil

modelo1=arima(Arimar.ts,order=c(1,2,1))

#Obtengo el coeficiente para el autorregresivo (ar1) y el para la media movil (ma1)
#También dará los errores estandar s.e.

summary(modelo1)

#Vemos los graficos incluyendo la prueba Ljung-box para saber si p>0.05 y haya ruido blanco
tsdiag(modelo1)

#Confimamos de que haya ruido blanco calculando (p-value >0.05)
Box.test(residuals(modelo1),type="Ljung-Box")

#Para corrobar,verificamos que el error si tenga media = 0
error=residuals(modelo1)
plot(error)

#Pronostico Arima para los proximos 10 meses (h = 10)
pronostico=forecast::forecast(modelo1,h=10)
pronostico
plot(pronostico)

#Lo : Limite de confianza inferior
#Hi : Limite de confianza superior
#95: Es el porcentaje de confianza
