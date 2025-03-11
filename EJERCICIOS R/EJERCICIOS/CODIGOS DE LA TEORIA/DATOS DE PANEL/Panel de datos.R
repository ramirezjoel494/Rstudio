
install.packages("AER")
library(AER)

# 11 grandes compañias, 3 variables macro en el periodo de 1935-1954

data("Grunfeld", package = "AER")
print(Grunfeld)

install.packages("plm")
library("plm")

Grun_all <- pdata.frame(Grunfeld, index = c("firm", "year"))
print(Grun_all)

#Si quisieramos utilizar solo 3 firmas entonces:
gr <- subset(Grunfeld, firm %in% c("General Electric", "General Motors", "IBM"))
print(gr)

pgr <- pdata.frame(gr, index = c("firm", "year"))
print(pgr)

#Pool de Datos OLS

# invest it = β1value it + β2capital it + αi + νit,

gr_pool <- plm(invest ~ value + capital, data = pgr, model = "pooling")
summary(gr_pool)

#modelo con Efectos fijos

gr_fe <- plm(invest ~ value + capital, data = pgr, model = "within")
summary(gr_fe)


#Para comparar efectos fijos Vs Pool

pFtest(gr_fe, gr_pool)


#Para un modelo con efectos aleatorios por diferentes métodos

# De Wallace-Hussain
gr_re <- plm(invest ~ value + capital, data = pgr, model = "random", random.method = "walhus")
summary(gr_re)

#De Amemiya
gr_re1 <- plm(invest ~ value + capital, data = pgr, model = "random", random.method = "amemiya")
summary(gr_re1)

# De Nerlove
gr_re2 <- plm(invest ~ value + capital, data = pgr, model = "random", random.method = "nerlove")
summary(gr_re2)



#Hausman TEST. para revisa cual modelo es consistente

Ho: Efectos aleatorios es consistente
Ha: Efectos fijos es consistente

phtest(gr_re, gr_fe)

phtest(gr_re1, gr_fe)

phtest(gr_re2, gr_fe)

#Significa que la hipotesis alternativa es inconsistente, por tanto es mejor usar
#Efectos aleatorios!!!


