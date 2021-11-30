##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECON?MICAS
#                         ECONOMETRIA II - 2020-II
#           Monitoria: Cointegración - Metodología Engle & Granger.
##____________________________________________________________________________________
##_____________________________________________________________________________________

## Limpiar el entorno de objetos
remove(list = ls())

# Se cargan los paquetes que vamos a utilizar. 
library(urca)
library(car)
library(rlang)
library(forecast)
library(tseries)
library(readxl)
library(dynlm)
library(broom)
library(aTSA)
 
#-------------
## Ejemplo 1 
#-------------

## vamos a replicar un ejemplo del libro introduction to econometrics with R de Hanck, Arnold, Gerber y Schmelzer. 
#Disponible en el siguietne link: https://www.econometrics-with-r.org/16-3-cointegration.html

## Para este ejemplo veremos la relacion entre la tasa de interés de corto plazo y la tasa de interés de largo plazo, 
## el spread de los bonos del Departamento del Tesoro de U.S.

#Cargamos la base de datos

USMacroSWQ <- read_xlsx(file.choose(),sheet = 1,col_types = c("text", rep("numeric", 9)))

# Formato de la columna fecha (date)
USMacroSWQ$...1 <- as.yearqtr(USMacroSWQ$...1, format = "%Y:0%q")

# Ajustamos nombres de las columnas
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI",
                          "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")

# Tasa de interés 3-months Treasury bills
TB3MS <- ts(USMacroSWQ$TB3MS, start = c(1957,1), end = c(2013,4), frequency = 4)
# Tasa de interés 10-years Treasury bonds
TB10YS <- ts(USMacroSWQ$GS10,start = c(1957,1), end = c(2013,4), frequency = 4)


#Siempre es recomendado analizar gráficamente las series.  
x11()
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)),
     plot.type = "single",
     lty = c(2, 1),
     lwd = 2,
     xlab = "Año",
     ylab = "Porcentaje anual",
     ylim = c(-5, 17),
     main = "Tasas de interés")

#Ahora vamos a graficar el spread a la misma gráfica.
lines(as.zoo(TB10YS-TB3MS),
      col = "steelblue",
      lwd = 2,
      xlab = "Año",
      ylab = "Porcentaje anual")

#Añadimos la leyenda.
legend("topright",
       legend = c("TB3MS", "TB10YS", "Spread"),
       col = c("black", "black", "steelblue"),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1))

#Del anterior gráfico es posible concluir que las series posiblemente están cointegradas, en tanto:
#las series en nivel aparentemente son I(1) (aparentan tener una tendencia estocástica común), y finalmente, 
#el spread entre ellas parece ser estacionario (la combinación lineal entre ambas parece eliminar la tendencia estocástica)

#_____________________________________________________________________________________________________

#METODOLOGÍA ENGLE & GRANGER.

#------
#Paso 1: Aplicaremos la prueba ADF y KPSS sobre cada una de las series.

## Prueba sobre la tasa de interés de corto plazo. 

#Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores críticos de la prueba son sensibles a 
#la inclusión de términos determinísticos, de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf1 = ur.df(TB3MS, lags=6, type = "trend")
summary(adf1)
plot(adf1)
#El tau nos dice que la serie tiene al menos una raíz unitaria, mientras el phi3 nos dice que la tendencia 
#no es significativa. Por lo tanto, vamos a mirar únicamente la prueba con intercepto.

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi2 me indica si la deriva es
#significativa, y por consiguiente, si se debe incluir en el test de raíz unitaria. 
adf2 = ur.df(TB3MS, lags=6, type = "drift")
summary(adf2)
plot(adf2)

#Los resultados indican que la serie tiene al menos una raíz unirtaria. El phi2, por su parte, indica que 
#la deriva de la serie no es significativa, por tanto, debe hacerse una prueba sin t?

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
summary(ur.df(TB3MS,lags=6, type = "none"))

#Claramente se evidencia que la serie tiene al menos una raíz unitaria, en tanto no se rechaza la hipótesis nula. 
#Noten la importancia de determinar si la serie tiene términos detemrmin?sticos, pues el valor calculado en cada
#especificación de la prueba cambió de forma importante.

#Hacemos la prueba KPSS cuya hipótesis nula es que la serie es estacionaria, la cual es totalmente contraria a la ADF. 
kp1 = ur.kpss(TB3MS, use.lag = 6, type = "mu") #el mu nos indica que es una prueba sin términos determinísticos.
summary(kp1)
plot(kp1)

#Claramente rechazamos la hipótesis nula de estacionariedad, así que las serie no es estacionaria. 

## Prueba sobre la tasa de interés de largo plazo. 

#Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores críticos de la prueba son sensibles a 
#la inclusión de términos determinísticos, de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf4 = ur.df(TB10YS,lags=6, type = "trend")
summary(adf4)
plot(adf4)
#El tau nos dice que la serie tiene al menos una raíz unitaria, mientras el phi3 nos dice que la tendencia 
#no es significativa. Por lo tanto, vamos a mirar únicamente la prueba con intercepto.

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi2 me indica si la deriva es
#significativa, y por consiguiente, si se debe incluir en el test de raíz unitaria. 
adf5 = ur.df(TB10YS, lags=6, type = "drift")
summary(adf5)
plot(adf5)
#Los resultados indican que la serie tiene al menos una raíz unitaria. El phi2, por su parte, indica que 
#la deriva de la serie no es significativa, por tanto, debe hacerse una prueba sin deriva

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
summary(ur.df(TB10YS, type = "none"))
#Claramente se evidencia que la serie tiene al menos una raíz unitaria, en tanto no se rechaza la hipótesis nula. 
#Noten la importancia de determinar si la serie tiene términos detemrminísticos, pues el valor calculado en cada
#especificación de la prueba cambió de forma importante.

#Hacemos la prueba KPSS cuya hipótesis nula es que la serie es estacionaria, la cual es totalmente contraria a la ADF. 
kp2=ur.kpss(TB10YS, use.lag = 6, type = "mu") #el mu nos indica que es una prueba sin términos determinísticos.
summary(kp2)
plot(kp2)
#Claramente rechazamos la hipótesis nula de estacionariedad, así que las serie tiene al menos una raíz unitaria. . 


#Hacemos la prueba ADF sobre las series diferenciadas para confirmar que son I(1)
adf7 = ur.df(diff(TB10YS), lag=6, type = "none");summary(adf7);plot(adf7)#Estacionaria
adf8= ur.df(diff(TB3MS), lag=6, type = "none"); summary(adf8);plot(adf8)#Estacionaria. 


#------
#Paso 2: Estimación de los residuales de la regresión en niveles.

#La teoría económica sugiere que el vector de cointegración es -1, no obstante, debemos estimar la regresión en niveles:

FS_EGADF <- dynlm(TB10YS ~ TB3MS) 
summary(FS_EGADF)

#Obtenemos los residuales
z_hat <- residuals(FS_EGADF)

#Graficamos los residuales y analizamos su ACF y PACF
x11()
plot.ts(z_hat)

lags=36
par(mfrow=c(1,2))
acf(z_hat,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales') 
pacf(z_hat,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales')

#Haremos la prueba ADF sin términos determinísticos.
adf9 = ur.df(z_hat,lags=6,type = "none");summary(adf9);plot(adf9) #Los residuales son estacionarios: hay cointegración. 

# No obstante, la prueba de ur.df ofrece valores críticos que no corresponden a los valores que se requieren para la prueba de cointegración 
#a pesar de que las mecánica sea lo misma.  Lo anterior, se debe a que los residuales fueron estimados (no son las verdaderas innovaciones porque no 
# son conocidas) debido a que  el vector de cointegración (el theta) en el caso univariado tuvo que ser estimado en la primer etapa

# A la hora de calcular los valores criticos para la prueba de engle-granger hay que tener en cuenta
# tanto el numero de observaciones como variables que se encuentren en la relacion de cointegracion
# para mas info ver enders 6.4 y el suppelementary manual 

# El comando coint.test del paquete aTSA permite realizar los dos pasos de la prueba de cointegración 
coint.test(TB10YS, TB3MS,nlag=6)

# type: el comando da tres resultados diferentes (para este ejemplo se usara el valor de type1)
## type1 es para prueba sin tendencia determinística 
## type2 es para prueba con tendencia lineal
## type3 es para prueba con tendencia cuadrática
# nlag: número de rezagos para la prueba de ADF (utilizamos los mismos que seleccionó el AIC)
# Generalmente el comando escoge automáticamente el numero de rezagos a usar 
# En este caso decidimos usar 6 rezagos

# numero de datos a la hora de realizar el test de cointegracion 
length(TB10YS)

# Si se usa la tabla c del supplementary manual del Enders se encuentra que cuando se tiene alrededor 
# De 200 datos Y 2 variables en la relación de cointegración, el test rechaza la hipotesis nula 
# de no cointegracion a valores de significancia del  5% pero no del 1%. Como en economia se usa generalmente un 
#nivel de significancia del 5 % se concluye de que hay cointegracion entre TB10YS y TB3MS

#------
#Paso 3: Regresión con variables cointegradas. 

###Modelo de Minimos Cuadrados Ordinarios Dinámicos###

#Asumiremos que hay un máximo de 6 rezagos, por lo tanto, miraremos cuál tiene el menor AIC.
# d(x, k) significa diferencia, donde k implica el orden de la diferencia (por default k = 1)
# L significa rezagos. Así L(d(TB3MS),-1:1)) significa incluya los adelantes y rezagos
# de la primera diferencia de TB3MS. -1 esta asociado con el adelanto mientras que 1 con el rezago

# Tener en cuenta que para un modelo dinámico es necesario incluir tanto rezagos como adelantos
# Para que la inferencia sea válida 

FS_MCOD1 <- dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-1:1)); summary(FS_MCOD1)
FS_MCOD2 <- dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-2:2)); summary(FS_MCOD2)
FS_MCOD3 <- dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-3:3)); summary(FS_MCOD3)
FS_MCOD4 <- dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-4:4)); summary(FS_MCOD4)
FS_MCOD5 <- dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-5:5)); summary(FS_MCOD5)
FS_MCOD6 <- dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-6:6)); summary(FS_MCOD6)


#Elegimos el mejor modelo
cbind(AIC(FS_MCOD1), AIC(FS_MCOD2),AIC(FS_MCOD3),AIC(FS_MCOD4),AIC(FS_MCOD5),AIC(FS_MCOD6)) #Elegimos 6 rezagos y adelantos, en tanto tuvo el menor AIC


#Graficamos el spread que nos dice la teoría junto con el estimado
x11()
plot(merge(as.zoo(TB10YS-TB3MS), as.zoo(residuals(FS_MCOD6))),
     plot.type = "single",
     lty = c(2, 1),
     lwd = 2,
     ylim = c(-5, 5),
     xlab = "Año",
     ylab = " ",
     main = "Diferencias")
legend("bottomright",
       legend = c("Spread","MCOD"),
       col = c(2,1),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1))

#Hacemos la prueba ADF sobre los residuales del modelo MCOD
adf10 = ur.df(residuals(FS_MCOD6), lag=6, type = "none"); summary(adf10);plot(adf10) #Residuales estacionarios,


#Por cuestión de tiempo solo validaremos no autocorrelación.No obstante, deben validar los demás supuestos.

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(TB10YS)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(FS_MCOD6),lag=60, type = c("Box-Pierce")) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(FS_MCOD6),type='Box-Pierce',lag=20) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(FS_MCOD6),type='Box-Pierce',lag=30) #rechazo H0,  no se cumple el supuesto.

# Test  Ljung-Box para autocorrelaci?n en los residuales.
Box.test(residuals(FS_MCOD6),lag=60, type = c("Ljung-Box")) #rechazo H0, no se cumple el supuesto.
Box.test(residuals(FS_MCOD6),type='Ljung-Box',lag=20) #rechazo H0, no se cumple el supuesto.
Box.test(residuals(FS_MCOD6),type='Ljung-Box',lag=30) #rechazo H0, no se cumple el supuesto.

#NOTA: casi siempre los residuales en un modelo MCOD presentará autocorrelación debido a que no se incluyen rezagos 
#de la variable dependiente, de  manera que en esos casos es recomendable utilizar errores robustos tipo HAC.

### MODELO DE CORRECIÓN DE ERRORES (VEC): Utilizaremos el Beta de la regresión del paso 2. 
### Introduciremos 3 rezagos de cada variable por supuesto. En realidad deberían comparar
### varios modelos con diferentes especificaciones por medio del AIC y determinar el mejor. La 
### inclusión del intercepto en el modelo de largo plazo no afecta el resultado.

VEC_EQ1 <- dynlm(d(TB10YS) ~  L(TB10YS-0.83095*TB3MS,1) + L(d(TB3MS), 0:3) + L(d(TB10YS), 1:3));summary(VEC_EQ1) #El parámetro de velocidad de ajuste alpha tiene el signo esperado
VEC_EQ2 <- dynlm(d(TB3MS) ~  L(TB10YS-0.83095*TB3MS,1)+ L(d(TB3MS), 1:3) + L(d(TB10YS), 0:3));summary(VEC_EQ2) #El parámetro de velocidad de ajuste alpha tiene el signo esperado

VEC_EQ3 <- dynlm(d(TB10YS) ~  L(TB10YS-0.95737*TB3MS,1) + L(d(TB3MS), 0:3) + L(d(TB10YS), 1:3));summary(VEC_EQ3) #El parámetro de velocidad de ajuste alpha tiene el signo esperado
VEC_EQ4 <- dynlm(d(TB3MS) ~  L(TB10YS-0.95737*TB3MS,1)+ L(d(TB3MS), 1:3) + L(d(TB10YS), 0:3));summary(VEC_EQ4) #El parámetro de velocidad de ajuste alpha tiene el signo esperado


#PASO 4: VALIDACIÓN

#Analizamos que los residuales sean estacionarios.
summary(ur.df(residuals(VEC_EQ1), lag=6, type = "none")) #Residuales estacionarios.
summary(ur.df(residuals(VEC_EQ2), lag=6, type = "none")) #Residuales estacionarios.

# De la preuba de ADF aplicado a estos residuales hay que tener cuidado en utilizar los valores criticos correctos dado que al incluir 
#el terimino de correccion de error se están incluyendo estimaciones de una primera etapa lo que hace que los valores criticos para una ADF
# aplicada a los residuales del modelo de corrección de errores pueden cambiar. Los valores criticos para usar son los mismos que se usan para 
#la prueba de cointegracion y están en el Enders.No obstante, dicho test no es para probar que los residuales son ruido blanco sino que son 
#estacionarios en media, lo cual es una consecuencia de que haya cointegración. 

# Test para probar que los residuales son ruido blanco

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(VEC_EQ1),lag=60, type = c("Box-Pierce")) #NO rechazo H0, se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),type='Box-Pierce',lag=20) #NO rechazo H0, se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),type='Box-Pierce',lag=30) #NO rechazo H0,  se cumple el supuesto.

Box.test(residuals(VEC_EQ2),lag=60, type = c("Ljung-Box")) #NO rechazo H0, se cumple el supuesto. 
Box.test(residuals(VEC_EQ2),type='Ljung-Box',lag=20) #NO rechazo H0, se cumple el supuesto. 
Box.test(residuals(VEC_EQ2),type='Ljung-Box',lag=30) #NO rechazo H0,  se cumple el supuesto.

#En tanto los residuales no están autocorrelacionados en ninguna ecuación, deberían validar los demás supuestos.
#A diferencia de MCOD, la inclusión de rezgos de la dependiente captura las estructuras de correlación de 
#carácter autorregresivo que allí no se incorporan.


#_____________________________________________________________________________________
#_____________________________________________________________________________________

#Ahora haremos un ejericio del libro de Enders para probar la PPA R=N*(P*/P)->  r= e + log(P*) - log(P)


#JAPANCPI is the Japanese price level and JAPANEX is the bilateral Japanese/U.S. exchange rate. 
#The starting date for all variables is January 1974 while the availability of the variables is such that most end near 
#the end of 2012. The price indices have been normalized to equal 100 in January 1973

PPA <- read_excel(file.choose())
attach(PPA)
View(PPA)

e = ts(log(JAPANEX), start=c(1974,1), frequency=12)
lp.j = ts(log(JAPANCPI), start=c(1974,1), frequency=12)  
lp.u = ts(log(USCPI), start=c(1974,1), frequency=12)

#METODOLOGÍA ENGLE & GRANGER.

#------
#Paso 1: Aplicaremos la prueba ADF y KPSS sobre cada una de las series.

## Prueba sobre el logaritmo de la tasa de cambio nominal

#Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores críticos de la prueba son sensibles a 
#la inclusión de términos determinísticos, de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf11= ur.df(e, lags=9, type = "trend")
summary(adf11)
plot(adf11)
#El tau nos dice que la serie tiene al menos una raíz unitaria, mientras el phi3 nos dice que la tendencia 
#no es significativa. Por lo tanto, vamos a mirar únicamente la prueba con intercepto.

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi2 me indica si la deriva es
#significativa, y por consiguiente, si se debe incluir en el test de raíz unitaria. 
adf12= ur.df(e, lags=9, type = "drift")
summary(adf12)
plot(adf12)
#Los resultados indican que la serie tiene al menos una raíz unitaria. El phi2, por su parte, indica que 
#la deriva de la serie no es significativa, por tanto, debe hacerse una prueba sin términos determinísticos.

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
adf13=ur.df(e, lags=9, type = "none")
summary(adf13)
plot(adf13)
#Claramente se evidencia que la serie tiene al menos una raíz unitaria, en tanto no se rechaza la hipótesis nula. 
#Noten la importancia de determinar si la serie tiene términos detemrminísticos, pues el valor calculado en cada
#especificación de la prueba cambió de forma importante.

#Hacemos la prueba KPSS cuya hipótesis nula es que la serie es estacionaria, la cual es totalmente contraria a la ADF. 
kp3= ur.kpss(e, use.lag = 9, type = "mu") #el mu nos indica que es una prueba sin términos determin?sticos.
summary(kp3)
plot(kp3)
#Claramente rechazamos la hipótesis nula de estacionariedad, así que las serie no es estacionaria. 

# Prueba sobre el logaritmo del nivel internacional de precios

#Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores críticos de la prueba son sensibles a 
#la inclusión de términos determinísticos, de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf14= ur.df(lp.j, lags=8, type = "trend")
summary(adf14)
plot(adf14)
#La serie según esta prueba es estacionaria en torno a una tendencia determinística. 

#Hacemos la prueba KPSS cuya hipótesis nula es que la serie es estacionaria, la cual es totalmente contraria a la ADF. 
kp4=ur.kpss(lp.j, use.lag = 8, type = "tau") #el tau nos indica que es una prueba con términos determinísticos.
summary(kp4)
plot(kp4)
#Claramente rechazamos la hipótesis nula de estacionariedad, así que las serie no es estacionaria. Nos inclinamos por
#esta prueba al evaluar la gráfica de la serie y sus residuales.


# Prueba sobre el logaritmo del nivel nacional de precios

#Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores críticos de la prueba son sensibles a 
#la inclusión de términos determinísticos, de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf15 = ur.df(lp.u,lags=7, type = "trend")
summary(adf15)
plot(adf15)

#La serie según esta prueba no es estacionaria en torno a una tendencia determinística. 

#Hacemos la prueba KPSS cuya hipóesis nula es que la serie es estacionaria, la cual es totalmente contraria a la ADF. 
kp5=ur.kpss(lp.u, use.lag = 7, type = "tau") #el tau nos indica que es una prueba con términos determinísticos.
summary(kp5)
plot(kp5)
#Claramente rechazamos la hipótesis nula de estacionariedad, así que las serie no es estacionaria. 


#Graficamos
x11()
autoplot(e, col="red", main = "logaritmo de la tasa de cambio nominal",xlab = "Fecha", ylab="", lwd=2)
autoplot(lp.j, col="red", main = "logaritmo del indice de precios de Japón",xlab = "Fecha", ylab="", lwd=2)
autoplot(lp.u, col="red", main = "logaritmo del indice de precios de USA",xlab = "Fecha", ylab="", lwd=2)


#------
#Paso 2: Estimación de los residuales de la regresión en niveles.

#La teoría económica sugiere que el tipo de cambio real bajo PPA debe ser uno, así que al sacar logaritmos nos queda e = log(p)-log(p*) 

R.1 <- dynlm(e ~ lp.j + lp.u) 
summary(R.1)

#Obtenemos los residuales
res <- residuals(R.1)

#Graficamos los residuales y analizamos su ACF y PACF
plot.ts(res)

lags=48
par(mfrow=c(1,2))
acf(res,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales') 
pacf(res,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales')

#Haremos la prueba ADF sin términos determinísticos.
adf16=ur.df(res, lags=12,type = "none") #Los residuales son estacionarios: hay cointegración. 
summary(adf16)
plot(adf16)


# Recordar que los valores criticos para la prueba de engle-granger son distintos a los valores
# criticos de una ADF convencional dado que hubo una estimacion anterior de primera etapa.
# Dichos valores se encuentran en la tabla del Enders, incluyendo 3 variables en la relación de cointegración

# Usando la funcion coef.test del paquete aTSA se obtiene: (hay que constuir una matriz para describir
# que hay dos variables regresoras o inputs)
coint.test(e, cbind(lp.j, lp.u),nlag = 12)

#------
#Paso 3: Modelo de Corrección de errores.
#Para este ejemplo solo estimaremos la primera ecuación. 
### Introduciremos 5 rezagos de cada variable por supuesto. En realidad deberían comparar
### varios modelos con diferentes especificaciones por medio del AIC y determinar el mejor


#Sin incluir la constante en el término de corrección de error. 
VEC_EQ1 <- dynlm(d(e) ~  L(e-0.10420*lp.j -0.76823*lp.u ) + L(d(lp.j), 0:5) + L(d(lp.u), 0:5) + L(d(e), 1:5));summary(VEC_EQ1) #El parámetro de velocidad de ajuste alpha tiene el signo esperado
#Incluyendo la constante en el término de corrección de error.
VEC_EQ1.cons <- dynlm(d(e) ~  L(e-9.97459 -0.10420*lp.j  -0.76823*lp.u ) + L(d(lp.j), 0:5) + L(d(lp.u), 0:5) + L(d(e), 1:5));summary(VEC_EQ1.cons) #El parámetro de velocidad de ajuste alpha tiene el signo esperado

#PASO 4: VALIDACI?N

#Analizamos que los residuales sean estacionarios.
summary(ur.df(residuals(VEC_EQ1), lags=3, type = "none")) #Residuales estacionarios.
summary(ur.kpss(residuals(VEC_EQ1), type="mu", use.lag = 3)) #Residuales estacionarios.

# De la preuba de ADF aplicado a estos residuales hay que tener cuidado en utilizar los valores críticos correctos dado que al incluir 
#el término de corrección de error se están incluyendo estimaciones de una primera etapa, lo que hace que los valores criticos para una ADF
# aplicada a los residuales del modelo de corrección de errores puedan cambiar.Los valores críticos para usar son los mismos que se usan para 
#la prueba de cointegracion y están en el Enders.No obstante, dicho test no es para probar que los residuales son ruido blanco sino que son 
#estacionarios en media, lo cual es una consecuencia de que haya cointegración. 

# Test Box-Pierce para autocorrelaci?n en los residuales
Box.test(residuals(VEC_EQ1),lag=117, type = c("Box-Pierce")) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),lag=60, type= c("Box-Pierce"))  #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),type='Box-Pierce',lag=20) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),type='Box-Pierce',lag=30) # rechazo H0,no  se cumple el supuesto.

Box.test(residuals(VEC_EQ1),lag=117, type = c("Ljung-Box")) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),lag=60, type = c("Ljung-Box")) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),type='Box-Pierce',lag=20) # rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),type='Box-Pierce',lag=30) #rechazo H0,  no se cumple el supuesto. 

#En tanto no se satisface el supuesto de no autocorrelación, tenemos que volver a la etapa de estimación e incluir más rezagos. Si fuese un modelo de
#MCOD, es natural que no se satisfaga el supuesto, por lo cual se utilizaría errores tipo HAC. Para VEC es necesario introducir rezagos adicionales, sobre
#todo si las series son mensuales debido a la persistencia de la correlación en este tipo de series. 