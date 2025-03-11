library(lubridate)
library(tseries)
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

attach(preciopm)
names(preciopm)

# Transformo los datos a serie de tiempo:
#Empieza en enero de 1990 hasta abril de 2019
precio.ts = ts(preciopm, start = c(1990,1), end= c(2019,4), frequency = 12)

print(precio.ts)

start(precio.ts)

end(precio.ts)

class(precio.ts)

#modelo aditivo
modeloaditivo = decompose(precio.ts)
plot(modeloaditivo)

#observed: valor observado
#trend: tendencia (si la tendencia aumenta, la varianza tambien)
#seasonal: estacionalidad
#residuos

#modelo multiplicativo
modelomultiplicativo = decompose(precio.ts, type = "mult")
plot(modelomultiplicativo)

#Estimamos la tendencia
tendencia = modelomultiplicativo$trend
print(tendencia)

#Estimamos la estacionalidad
estacionalidad = modelomultiplicativo$seasonal
print(estacionalidad)

# La tendencia y estacionalidad se incorporará la tendencia y estacionalidad

ts.plot(cbind(tendencia, tendencia*estacionalidad),lty = 1:2)
# grafica de tendencia con tendencia*estacionalidad

