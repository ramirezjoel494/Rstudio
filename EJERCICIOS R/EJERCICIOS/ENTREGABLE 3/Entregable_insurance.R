#CARGA Y PREPARACION DE LOS DATOS

#Obtengo el archivo y lo muestro en consola
df = read.csv("C:/Users/User/Desktop/EJERCICIOS R/EJERCICIOS/ENTREGABLE 3/insurance2.csv", sep=",", header = TRUE)
head(df)

#Se muestran cuantas filas y columnas hay
dim(df)

# Janitor sirve para limpiar los nombres de las columnas y contar los valores faltantes en ella
library(janitor) 

# Garantizo que los nombres de las columnas sean solo letras, numeros o guiones bajos
df = clean_names(df)
head(df)

# Cambio los espacios por guines bajos
df$sex = gsub(" ", "_", df$sex)
df$children = gsub(" ", "_", df$children)
df$region = gsub(" ", "_", df$region)
head(df)


#ANALISIS DE VARIABLE OBJETIVO

# Muestro el histograma con base en la variable objetivo
hist(df$charges, freq = TRUE)

# Muestro el histograma con base en la variable objetivo pero en escala logaritmica
log_price = log1p(df$charges)
hist(log_price, freq = TRUE)


# VALORES FALTANTES
sapply(df, function(x) sum(is.na(x)))


# DIVISIÓN DE DATOS

set.seed(1234)
trvaltest <- function(dat,prop = c(0.6,0.2,0.2)){
  nrw = nrow(dat)
  trnr = as.integer(nrw *prop[1])
  vlnr = as.integer(nrw*prop[2])
  set.seed(123)
  trni = sample(1:nrow(dat),trnr)
  trndata = dat[trni,]
  rmng = dat[-trni,]
  vlni = sample(1:nrow(rmng),vlnr)
  valdata = rmng[vlni,]
  tstdata = rmng[-vlni,]
  mylist = list("trn" = trndata,"val"= valdata,"tst" = tstdata)
  return(mylist)
}

outdata = trvaltest(df,prop = c(0.6,0.2,0.2))
df_train = outdata$trn; df_val = outdata$val; df_test = outdata$tst
head(df_train)

#Guardo los valores originales
y_train_orig =  select(df_train, charges)
y_val_orig = select(df_val, charges)
y_test_orig = select(df_test, charges)

#Transformo los valores a escala logaritmica
y_train = log1p(y_train_orig)
y_val = log1p(y_val_orig)
y_test = log1p(y_test_orig)

#Eliminación de la variable objetivo
df_train <- df_train[ ,!colnames(df_train)=="charges"]
df_val <- df_val[ ,!colnames(df_val)=="charges"]
df_test <- df_test[ ,!colnames(df_test)=="charges"]

