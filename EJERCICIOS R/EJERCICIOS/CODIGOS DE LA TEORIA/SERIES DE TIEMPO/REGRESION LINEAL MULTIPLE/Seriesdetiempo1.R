
#Regresión lineal con series de tiempo en R con diferencias y retardos

library(AER)
library(dynlm)

install.packages("USMacroG")
data(package = "AER")

print(USMacroG)

plot(USMacroG[, c("dpi", "consumption")], lty = c(3, 1),
     plot.type = "single", ylab = "")

legend("topleft", legend = c("income", "consumption"),lty = c(3, 1), bty = "n")


#Consumption i = β1 + β2 dpi i + β3 dpi i−1 + εi

cons_1 <-dynlm(consumption ~ dpi + L(dpi), data = USMacroG)


#Consumption i = β1 + β2 dpi i + β3 consumption i−1 + εi

cons_2 <-dynlm(consumption ~ dpi + L(consumption), data = USMacroG)


#Con mas de un retardo

cons_3 <-dynlm(consumption ~ dpi + L(consumption, 4), data = USMacroG)

#Con la primera diferencia

cons_4 <-dynlm(d(consumption) ~ d(dpi) + L(consumption, 3), data = USMacroG)



summary(cons_1)
summary(cons_2)
summary(cons_3)
summary(cons_4)



#Para obtener el RSS de ambos modelos

deviance(cons_1)
deviance(cons_2)
deviance(cons_3)
deviance(cons_4)

#Modelo global (Encompassing test)


cons_ADL <- dynlm(consumption ~ dpi + L(dpi) + L(consumption), data = USMacroG)
summary(cons_ADL)

#ya podemos comparar los dos primeros modelos con el modelo general

encomptest(cons_1, cons_2)


#Lo que hace encomptest es comparar modelos no anidados, 
#para ello primero genera un modelo que contiene todas las variables (Model E).
#Luego ese Modelo E lo compara con el modelo 1 y 2
#cuando los 2 resultan significativos no podemos llegar a ninguna conclusión
