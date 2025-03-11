###########################
# Introduccion al Analisis Cuantitativo: Introduccion a R
###########################
######### Pildora 2 ######################################
# Va sin acentos

#Ejemplo 4: Simulando data de Patada de Caballo
# En este ejemplo utilizaremos el generador numeros aleatorios de Poisson, recordando que, en el ejemplo 
# anterior, se utilizaron valores de la densidad de Poisson para simular los datos.

y = rpois(200, lambda=.61) #En cada corrida se generaron nuevos numeros

patadas = table(y) #Tabla de Frecuencias
patadas

Teoreticos = dpois(0:4, lambda=.61) # Generamos la distribucion de masa para la distribucion discreta de Poisson 

Muestra = patadas / 200 # Asignamos probabilidades, a traves de la definicion clasica

cbind(Teoreticos, Muestra) # Comparamos las probabilidades teoricas con las muestrales 

mean(y) #Calculamos la media, que en este caso es mas facil ya que tenemos los datos en un vector, y.

var(y) # De igual manera la varianza.

# Es interesante notar que los datos simulados del problema de caballo no se ajustan tan bien al modelos de POisson 
# como los datos originales, esto en virtud de los valores obtenidos para la media y la varianza.


####### Creando Funciones en R 
#________________________________________________________________
# Discutiremos un poco, como se crean funciones por parte del usuario

# Ejemplo 5: Definicion de una funcion %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# El estimador de MLE

#%________________________________________________________________

var.n = function(x){
  v = var(x)
  n = NROW(x) # Computa el numero de observaciones en x
  v * (n - 1) / n
}
#%________________________________________________________________

# OJO: Las funciones deben ejecutar antes de aplicarlas

temps = c(51.9, 51.8, 51.9, 53) # Creamos un vector de ejemplo con los datos de temperatura

var(temps) # Estimador insesgado de la varianza

var.n(temps) # Estimador de MLE de la varianza

integrate(f, lower=0, upper=1, a=2, b=2) # Integrate, es el ejemplo de una funcion que utiliza mas de un argumento 

# Ejemplo 6: Grafico de una funcion usando curve() %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

curve(x*(1-x), from=0, to=1, ylab="f(x)")

######## Vectores y Matrices
# _______________________________________________________________
# Usaremos la funci?n matrix( )

probs = c(.45, .05, .01, .48, .70, .50, .07, .25, .49) # Definimos primero el vector de probabilidades

P= matrix(probs, nrow=3, ncol=3) # Creamos la matriz P de dimension 3x3, 3 filas y 3 columnas
P # Notemos que lamatriz se va llenando por columnas, para llenarla por filas hacemos byrow=TRUE 

rownames(P) <- colnames(P) <- c("baja", "media", "alta") # Le damos nombre a las filas y columnas
P

rowSums(P) # Verificamos qu een cada fila la suma de las probabilidades sea 1

P2 = P %*% P # Calculamos el producto de PxP,esto nos d? las probabilidades de transici?n en 2 generaciones.
P2

P2[1, 3] # ubicamos el elemento ubicado en la fila 1 columna 3 de P2.

P2[1, ] # nos traemos los valores de la fila 1 de P2

P4 = P2 %*% P2 # Despues de 8 generaciones se tiene las probabilidades
P8 = P4 %*% P4
P8

# Manualmente se pueden introducir los valores de la forma siguiente, 

Q = matrix(c(  0.45, 0.48, 0.07,
               0.05, 0.70, 0.25,
               0.01, 0.50, 0.49), nrow=3, ncol=3, byrow=TRUE)
Q

############ Dataframes 
#__________________________________________________________________

head(USArrests) # Desplegamos las primeras lineas de los datos

NROW(USArrests) # numero de observaciones
dim(USArrests) # numero de filas y columnas

names(USArrests)# Nombre de las variables en el dataframe

# Estructura de los datos
str(USArrests) # Arroja el tipo de variable y una muestras de la misma

# Esta informacion puede guardarse en una matriz pero todos los datos deben ser del mismo tipo, as? R convierte 
# todos los datos a tipo numerico por defecto
arrests = as.matrix(USArrests) 
str(arrests)

# Datas perdidos

any(is.na(USArrests))# Preguntamos se existe algun valor faltante

# Trbajando con el dataframe 

summary(USArrests) # Nos ofrece un resumen de los estadisticos fundamentales para los datos 

# observamos que la media y la media son casi similares en todas salvo en Assault,lo que indica posible sesgo.

#Extraer datos del dataframe

USArrests["California", "Murder"] # Extrae para California, el numero de muertes

USArrests["California", ] # Extrae toda la iformacion de california

# Extraer variable usando $

USArrests$Assault # Solo los asaltos

hist(USArrests$Assault, col = 'lightblue') # Histograma de frecuencias para asaltos

# Attach el dataframe
# Si hacemos un attach al dataframe podemos referenciar directamente las variable sin el uso del $

attach(USArrests)
murder.pct = 100 * Murder / (Murder + Assault + Rape)
head(murder.pct)

# Cuando ya no se requiere el attach podemos utilizar la funcion detach()

# Scatterplots and correlaciones 

plot(UrbanPop, Murder) #Se plotea poblacion urbana contra asesinatos

pairs(USArrests) #Plot 2 a 2 con todas las variables

cor(UrbanPop, Murder) #Correlacion, grado de asociacion lineal

cor(USArrests)# Matriz de Correlacion 

#____________________________________________________________________________________
####################################################################################
# Fin de la pildora 2
####################################################################################
#___________________________________________________________________________________

