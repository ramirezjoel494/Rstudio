x = c(2,1,3,2,0)
x
joel = "jesus"
joel


probs = c(.45, .05, .01, .48, .70, .50, .07, .25, .49) # Definimos primero el vector de probabilidades

P= matrix(probs, nrow=3, ncol=3) # Creamos la matriz P de dimension 3x3, 3 filas y 3 columnas
P # Notemos que lamatriz se va llenando por columnas, para llenarla por filas hacemos byrow=TRUE 

rownames(P) = c("baja", "media", "alta") # Le damos nombre a las filas y columnas
colnames(P) = c("baja", "media", "alta")
P

rowSums(P) # Verificamos qu een cada fila la suma de las probabilidades sea 1

P2 = P %*% P # Calculamos el producto de PxP,esto nos d? las probabilidades de transici?n en 2 generaciones.
P2

P2[1, 3] # ubicamos el elemento ubicado en la fila 1 columna 3 de P2.

P2[1, ] # nos traemos los valores de la fila 1 de P2

P4 = P2 %*% P2 # Despues de 8 generaciones se tiene las probabilidades
P8 = P4 %*% P4
P8

