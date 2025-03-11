##################################################################
# Introduccion al Analisis Cuantitativo: Data Cuantitativa
##################################################################
################## Pildora 5 ####################################
# Va sin acentos

#__________________________________________________________________________________
# Ejemplo 4: 
#__________________________________________________________________________________

# Este ejemplo es un ejemplo de series de tiempo
nhtemp # Visualizamos la serie
# Son 60 datos que van de 1912 a 1971

plot(nhtemp) # Graficamos los datos, con el objeto de ver los patrones en los datos

# Uno podria estar interezado en identificar cualquier tendencia en los datos, lo que se hace
# ajustando una curva suave, utilizando la funcion lowess(), la cual esta basada en una 
#regresión polinómica ponderada localmente. De igual manera, incluimos una linea de referencia
# horizontal a traves de la media usando abline().

plot(nhtemp, ylab="Temperatura promedio anual")
abline(h = mean(nhtemp))# Ingresamos la recta en la media de los datos 
lines(lowess(nhtemp)) # Curva de tendencia de los datos

# A menudo se desea transformar una serie temporal para que los datos sean estables con
#respecto a la media en el tiempo. 
#Cuando los datos parecen tener una tendencia aproximadamente lineal a
#lo largo del tiempo, las primeras diferencias a menudo eliminan la tendencia.
# La primera diferencia los calculamos con la funcion diff().
diff(nhtemp) # Calculamos las primeras diferencias

# Colocamos como recta de referencia una constante de altura cero e incluimos la curva suave
# como lo hicimos anteriormente.
d = diff(nhtemp)
plot(d, ylab="Primera diferencia de la temperatura promedio anual")
abline(h = 0, lty=3) # Recta punteada 
lines(lowess(d))

# Notemos que el grafico sugiere que la media de las series diferenciadas es estable a lo 
#largo del tiempo; observe que la curva mas baja (linea solida) es casi horizontal y 
#muy cercana a la línea horizontal de puntos hasta 0.

#________________________________________________________________________________________
# Ejemplo 5: ( Loteria del Ejercito de 1970) 
#________________________________________________________________________________________

# Cargamos los datos del ejemplo, lo cuales, se encuentran en el archivo draft-lottery
draftnums = read.table("D:/Diplomados online/Sweave_Probabilidad/Analisis Estadistico/Nuevo concepto/draft-lottery.txt", header=TRUE)

# Este dataframe es una tabla que contiene los numeros de la loteria por dia y mes
names(draftnums)

# Para encontrar un numero especifico de la seleccion, como por ejemplo para el 15 de Enero, hacemos 
draftnums$Jan[15]

# Asumiendo que los cumpleaños fueron etiquetados con numeros elegidos de manera aletoria, deberiamos
# esperar que la mediana de cada mes se encuentre cerca de 366/2=183.
# Entonces, para calcular la mediana de cada mes, aplicamos la funcion sapply(). 
meses = draftnums[2:13] # Extraemos los meses de los datos que se encuentran de la columna 2 a la 13 
sapply(meses, median) #Calculamos la mediana a cada mes

# En los casos en que se tengan datos faltantes, la mediana de los datos no traera el valor NA,
# por lo que debemos incorporar la instruccion na.rm=TRUE

sapply(meses, median, na.rm=TRUE) # Incorporando la instruccion podemos realizarel calculo de la mediana

# Grafiquemos estos resultados
medianas = sapply(meses, median, na.rm=TRUE) #guardemos los dato sen la variable medianas
plot(medianas, type="b", xlab="Nùmero del mes") # Con type="b" obtenemos las marcas de los datos por mes
abline(h=183)
lines(lowess(medianas))
# Observamos que las mediana se encuentran mas o menos disribuidas de forma uniforme alrededor del valor
# 183 que los indicamos con la recta horizontal, a la vez que observamos una tendencia decreciente

# Un grafico de cajas es muy util para comparar la distribucion de los numeros en cada mes 
meses = draftnums[2:13]
boxplot(meses)

# Como observamos,la distribucion de los numeros es mas o menos distribuida uniformemente que era la situacion
# qu eveniamos observando, sin embargo, para los dos ultimos meses, estos parecen estar mas abajo que el resto
#de los meses

#________________________________________________________________________________________
# Ejemplo 6: (Media muestral y el Teorema central del limite) 
#________________________________________________________________________________________

# Inspecciones un poco los valores generados por el algoritmo RANDU
head(randu)

# Veamos que efectivamente la media de los datos es similar a la teorica
sapply(randu, mean) #En particular se debe a la ley de los grandes numeros

# Vemos que es aproximadamente igual a la media teorica de 0.5

# Mientras que la varianza, la tenemos por 
var(randu)

# Esto nos arroja una matriz varianza-covarianza, para la cual, los valores que nos interezan en 
# este ejemplo, son los que se encuentran en la diagonal, ya que lo que estamos viendo es la varianza,
# en cada entrada del vector x,y,z.
# Vemos, que son aproximadamente igual a la varianza teorica de 0.08333.
# Si deseamos solo ver la diagonal, hacemos
diag(var(randu))

# Si deseamos ver la matriz de correlaciones, hacemos 
cor(randu)
# Notemos que las correlaciones son proximas a cero en valor absoluto, lo que nos habla de la 
#independencia en la generación de los números aleatorios.
 
#Aunque los datos de los randu (x,y,z) tienen correlaciones cercanas a cero, de hecho hay 
#una relación lineal que se puede observar si vemos los datos en un gráfico tridimensional. 
#Hagamos el gráfico para ver que se puede observar un patrón en los datos
#(no es muy aleatorio, pseudoaleatorios).

install.packages("lattice")# Instalamos el paquete 
library("lattice")# Cargamos la libreria
cloud(z ~ x + y, data=randu)
# Si Observamos bien, vemos algunos patrones lineales 

# Estudiemos la distribucion de las medias de la columnas.
means = apply(randu, MARGIN=1, FUN=mean) # Calculemos las medias para cada vector (cada fila), es decir, 400.

# Podemos hacer un histograma de frecuencias de la muestra means usando hist()
hist(means)
# Este histograma tiene una forma de montículo, algo simetrico. Este tipo de agrupamientos es el 
# que se debe esperar cuando el tamaño de la muestra tiende a infinito.
# Para poder comparar nuestro histograma con una distribucion nomal, lo necesitamos en probabilidades,
# no es frecuencias, para ello hacemos prob=TRUE

hist(means, prob=TRUE)

# Estimamos uns densidad para los datos randu
plot(density(means)) # Notamos que tiene una forma acampanada tal como la distribucion normal

# Por defecto, la función truehist() muestra un histograma de probabilidad, que ajustara 
#una funcion acampanada. Con la funcion curve(), podemos compararla con una funcion 
# normal para la cual colocamos la media y desviacion estandar teorica
# es decir, media 1/2=0.5 y desviacion estandar, para el promedio de los valores, se divide
# entre 3, dando como resultado (1/12)/3=1/36=0.0277
truehist(means)
curve(dnorm(x, 1/2, sd=sqrt(1/36)), add=TRUE)

# Como podemos observar la distribucion de las medias puede ser muy bien modelada por la distribucion
#normal, a pesar de que el tamaño de las muestras para realizar el promedio es tres.

# Con al funcion qqnorm() comparamos los cuantiles teoricas con los de la muestra
qqnorm(means)
qqline(means) # ajustamos la recta identidad para realizar la comparacion

#________________________________________________________________________________________
# Topicos Especiales
#________________________________________________________________________________________
# Ejemplo 7: (DIstancia entre puntos)
# _______________________________________________________________________________________
library("MASS")# Llamamos a la libreria 
data(mammals)# Cargamos los datos 

x = mammals[1:5, ] # Elegimos de la base de datos los 5 primeros datos 

# podemos calcular la distancia, que R por defecto nos trae al distancia euclidea.
dist(x) # Nos arroja una matriz con la distancias de cada variable a la otra 

as.matrix(dist(x)) # Para muchas aplicaciones necesitamos toda la matriz. 

plot(log(mammals$body), log(mammals$brain),# Graficamos los datos y escalamos 
     xlab="log(body)", ylab="log(brain)")

y = log(mammals[c("Grey wolf", "Cow", "Human"), ])# Extraemos tres observaciones de mammals
polygon(y) # ploteamos la diagonal

text(y, rownames(y), adj=c(1, .5)) # Agregamos los textos

dist(y)# vemos las distancias para este vector

#________________________________________________________________________________________
# Ejemplo 8: (Analisis de Cluster de Distancia)
# _______________________________________________________________________________________
# El analisis de cluster es frecuentemente aplicado para revelar posibles estructuras es los datos
# Veamos una funcion que implementa el analisis de cluster gerarquico, con la 
# funcion hclust()

d = dist(log(mammals)) 
h = hclust(d, method="complete") 
# Este metodo, fundamentalmente, va agrupando los datos en función de sus distancias, es decir,
# va agrupando los datos que se encuentran mas cercanos

big = subset(mammals, subset=(body > median(body))) # trabajaremos con la mitad mas grande

d = dist(log(big))
h = hclust(d, method="complete")

plot(h)# Obtenemos el dendograma
# Las ramas mas bajso muestra a los mas cercanos, mientras, que los superiores muestra los mas lejanos

head(h$merge)# Vemos los mas cercanos segun este algoritmo.
# Vemos que el logaritmo de la observacion 22 y la 28 tienen la menor distancia
# Buscamos a la observacion 22 y la 28
rownames(mammals)[c(22, 28)]

log(mammals)[c(22, 28), ]# Vemos sus logaritmos


#____________________________________________________________________________________
####################################################################################
# Fin de la pildora 
####################################################################################
#___________________________________________________________________________________

