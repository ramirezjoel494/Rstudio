##################################################################
# Introduccion al Analisis Cuantitativo: Data Cuantitativa
##################################################################
################## Pildora 4 ####################################
# Va sin acentos

library(MASS) # Cargamos el paquete 
data() #desplegamos los datasets disponibles 

#_________________________________________________________________________
# Ejemplo 1: Talla de cuerpo y cabeza de mam?feros (Exploraci?n de Datos).
#_________________________________________________________________________

?mammals #Pedimos la informaci?n que tiene R de los datos

head(mammals) # Vemos los primeros datos

is.matrix(mammals)# Verificamos si los datos tipo matriz

is.data.frame(mammals)# Verificamos si los datos son tipo dataframe

summary(mammals) # Resumen estad?stico

boxplot(mammals)# Visualizamos los datos en dos gr?ficos de caja
# Podemos ver como los datos se encuentran, en su mayor?a por debajo del valor 166 (tercer cuartil,
# recordemos, que por este nos indica que el 75% del total de los datos en la poblaci?n,
# se encuentran por debajo de este valor), mientras, que la media de los datos se ve fuertemente influenciada
# por los valores que quedan por fuera de la caja, que son varios, lo que nos muestra una de las 
# caracter?sticas que tiene la media, es decir, que esta es fuertemente desviada por los valores at?picos.

plot(mammals) # Veamos si existe alg?n tipo de relaci?n entre las variables

# no podemos distinguir algo, debido a la escala, por lo que tomamos el log() de los datos, lo que 
# es equivalente a ver la variaci?n de los datos y no los datos directamente.

plot(log(mammals$body), log(mammals$brain),
     xlab="log(body)", ylab="log(brain)")

# Como apreciamos, el gr?fico en escala log-log, podemos ver mejor el comportamiento de los datos pareados.
#Sin embargo, se tiene que tener cuidado al aplicar, ya que si, por ejemplo, tenemos datos negativos,
#no podremos aplicar la transformaci?n a logaritmos.

summary(log(mammals))# Resumen de datos 

# Vemos como los principales valores estad?sticos son muchos m?s similares, teniendo la media y la mediana
# mucho m?s similares.

boxplot(log(mammals), names=c("log(body)", "log(brain)")) # Gr?fico de cajas en escala log-log

#Correlaci?n y Recta de Regresi?n 

# Correlaci?n

cor(log(mammals)) # Calculemos la matriz de correlaci?n en escala log-log

# Obtenemos, ya como se observaba en el plot(log(mammmals)), existe evidencia estad?stica
# que nos permite establecer que hay una relaci?n lineal entre log(body) y log(brain)

# Como son dos variables, podemos calcular la correlaci?n de las variables de interes
cor(log(mammals$body), log(mammals$brain)) #Nos arroja el valor que vimos en la matriz

# Regresi?n lineal

plot(log(mammals$body), log(mammals$brain),
     xlab="log(body)", ylab="log(brain)")
x = log(mammals$body); y = log(mammals$brain)
abline(lm(y ~ x)) # Ajustamos la recta con el metodo de minimos cuadrados ordinarios

#__________________________________________________________________________________
# Ejemplo 2: IQ de gemelos (An?lisis Bivariado  por Grupos)
#__________________________________________________________________________________

# Importamos los datos directamente, especificamos la ruta donde encontramos los datos
# guardamos los datos en twins
twins = read.table("D:/Diplomados online/Sweave_Probabilidad/Analisis Estadistico/Nuevo concepto/twinIQ.txt", header=TRUE)

# Tambien podemos utilizar Rcmdr

head(twins)

summary(twins)

# Notemos que para las variables Foster y Biological, que son de tipo num?rico, el summary nos trae 
# los valores estad?sticos usuales presentandose en la misma escala, mientras, que para Social, el resultado es diferente, ya que es de 
# tipo factor.

# Hagamos un boxplot de la diferencia de los IQ entre los gemelos
boxplot(Foster - Biological ~ Social, twins)

# Otra forma de observar los datos es con un scatterplot

# Hacemos un attach de los datos para poder llamarlos por su nombre sin problema 
attach(twins)
status = as.integer(Social)# Creamos una variable entera que asigne 1=Alta 3=Media 2=Baja
status

# Graficamos los gemelos y los coloreamos y le colocamos los caracteres seg?n la variable 
# status que hemos creado
plot(Foster ~ Biological, data=twins, pch=status,  col=status)
# legenda, ubicacion, nombres de etiquetas segun caracteres y colores, margenes
legend("topleft", c("Alta","Bajo","Medio"),
       pch=1:3, col=1:3, inset=.02)

abline(0, 1) # Comparo con respecto a la bisectriz para determinar que tan lejos se 
# encuentran los valores de la igualdad, es decir, Foster=Biological

# Plots Condicionales
# En lugar de desplegar la data en diferentes colores y caracteres, realiza diferentes con la misma escal
#

coplot(Foster ~ Biological|Social, data=twins)

#__________________________________________________________________________________
# Ejemplo 3: Talla del cerebro e inteligencia (Data Multivariada)
#__________________________________________________________________________________

brain = read.table("D:/Diplomados online/Sweave_Probabilidad/Analisis Estadistico/Nuevo concepto/brainsize.txt", header=TRUE)
#Exploramos los datos
summary(brain)
# Importante notar la presencia de datos faltantes

mean(brain$Weight)
# Debemos indicarle a R que calcule la media sin considerar los datos faltantes

mean(brain$Weight, na.rm=TRUE)

# Resumimos por grupo
# En la muestra se tiene 20 mujeres y la misma cantidad de hombres, por lo que quisieramos 
# poder tener estadisticas por separado por genero, esto lo hacemos con la funcion by( ).
# Entendamos la funcion: El -1 significa que considere todas las variables menos la primera 
# que es genero, le indicamos que consideremos como indice el genero y que calcule la media 
# para cada variable

by(brain, brain$Gender, function(x){
  means <- colMeans(x[,2:7], na.rm = TRUE)
})

attach(brain)
gender = as.integer(Gender) #necesita entero para ploteae simbolo y color
plot(Weight, MRI_Count, pch=gender, col=gender)
legend("topleft", c("Femenino", "Masculino"), pch=1:2, col=1:2, inset=.02)

# REsumiendo par de variables
pairs(brain[, 2:7])
# Pareciera que los datos pueden ser agrupados en dos grupos 

round(cor(brain[, 2:7]), 2)

round(cor(brain[, 2:7], use="pairwise.complete.obs"), 2)# quitamos los datos faltantes


#____________________________________________________________________________________
####################################################################################
# Fin de la pildora 
####################################################################################
#___________________________________________________________________________________