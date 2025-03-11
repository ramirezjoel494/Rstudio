#Evaluación 3 en R

#CARGA Y PREPARACION DE LOS DATOS

df = read.csv("data.csv", sep=",", header = TRUE)
head(df)

dim(df)

library(janitor)
df = clean_names(df)
head(df)

df$model = gsub(" ", "_", df$model)
df$engine_fuel_type = gsub(" ", "_", df$engine_fuel_type)
df$driven_wheels = gsub(" ", "_", df$driven_wheels)
head(df)


#ANALISIS DE VARIABLE OBJETIVO

hist(df$msrp, freq = TRUE)

log_price = log1p(df$msrp)
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

library(dplyr)

y_train_orig =  select(df_train, msrp)
y_val_orig = select(df_val, msrp)
y_test_orig = select(df_test, msrp)

y_train = log1p(y_train_orig)
y_val = log1p(y_val_orig)
y_test = log1p(y_test_orig)

#Eliminación de la variable objetivo
df_train <- df_train[ ,!colnames(df_train)=="msrp"]
df_val <- df_val[ ,!colnames(df_val)=="msrp"]
df_test <- df_test[ ,!colnames(df_test)=="msrp"]

