##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2021-I
#      SESIÓN MONITORIA : Simulación de procesos AR, MA y ARMA
##____________________________________________________________________________________
##_____________________________________________________________________________________

#Limpiamos el environment.
remove(list = ls())

#Activamos algunos paquetes que vamos a utilizar en toda la metodología

library(forecast)    # Para hacer pronósticos con modelos arima
library(lmtest)      # Significancia individual de los coeficientes ARIMA
library(urca)        # Prueba de raíz unitaria
library(tseries)     # Para estimar modelos de series de tiempo y hacer pruebas de supuestos.
library(readxl)      # Para leer archivos de excel
library(stargazer)   # Para presentar resultados más estéticos.
library(psych)       # Para hacer estadísticas descriptivas
library(seasonal)    # Para desestacionalizar series
library(aTSA)        # Para hacer la prueba de efectos ARCH
library(astsa)       # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA  
library(xts)         # Para utilizar objetos xts 
library(tidyverse)   # Conjunto de paquetes (incluye dplyr y ggplot2)
library(readxl)      # Para leer archivos excel 
library(car)         # Para usar la función qqPlot

#---- Explicación del Script -----

# Inicialmente vamos a simular diferentes Procesos Generadores de datos para analizar sus características
# Por ende, se van a simular procesos autoregresivos, de media móvil y ARMA
# En total se van a simular 11 series y se van a simular tanto series estacionarias como no estacionarias

#---

# Número de observaciones para todas las series
obs =  2500 # cambiar el valor para ajustar el número de datos de todas las series

#---- Simulación de un proceso de ruido blanco ----

set.seed(1)
yt_rb = ts(arima.sim(model= list(order = c(0,0,0)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt_rb, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,0,0) (Ruido Blanco)",lty=1, lwd=0.2, col="red")

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,0,0) (Ruido Blanco)
lags=20
par(mfrow=c(1,2))
acf(yt_rb,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,0,0) (Ruido Blanco)') 
pacf(yt_rb,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,0,0) (Ruido Blanco)')

#---- Simulación de un proceso AR(1) estacionario ----

set.seed(8202) #Para que los valores de la simulación no cambien

yt1s = ts(arima.sim(model= list(order = c(1,0,0), ar=c(0.6)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt1s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (1,0,0)",lty=1, lwd=0.4, col="red")

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,0,0)
lags=20
par(mfrow=c(1,2))
acf(yt1s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,0,0)') 
pacf(yt1s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,0,0)')
#Otro comando para encontrar la acf y la pacf simultáneamente 
x11()
acf2(yt1s, max.lag = lags)

#---- Simulación de un proceso AR(1) no estacionario ----

set.seed(29101) #Para que los valores de la simulación no cambien
yt1n = ts(arima.sim(model= list(order = c(1,1,0), ar=c(0.6)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt1n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (1,1,0)",lty=1, lwd=0.4, col="blue")

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,1,0) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente y por ende tampoco es estacionario.
lags=20
par(mfrow=c(1,2))
acf(yt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,0)') 
pacf(yt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,0)')

#---

#---- Simulación de un proceso AR(2) estacionario ----

set.seed(2929) #Para que los valores de la simulación no cambien
yt2s = ts(arima.sim(model= list(order = c(2,0,0), ar=c(0.6,0.3)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt2s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (2,0,0)",lty=1, lwd=0.4, col="red")

#Vamos a hacer la ACF y PACF del PGD ARIMA(2,0,0)
lags=20
par(mfrow=c(1,2))
acf(yt2s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(2,0,0)') 
pacf(yt2s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(2,0,0)')


#---- Simulación de un proceso AR(2) no estacionario ----

set.seed(0202) #Para que los valores de la simulación no cambien
yt2n = ts(arima.sim(model= list(order = c(2,1,0), ar=c(0.6,0.3)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt2n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (2,1,0)",lty=1, lwd=0.4, col="blue")

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,1,0) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente.
lags=20
par(mfrow=c(1,2))
acf(yt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(2,1,0)') 
pacf(yt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(2,1,0)')


#---

#---- Simulación de un MA(1) estacionario ----

set.seed(2020) #Para que los valores de la simulación no cambien
yt3s = ts(arima.sim(model= list(order = c(0,0,1), ma=c(0.7)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt3s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,0,1)",lty=1, lwd=0.2, col="red")

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,0,1)
lags=20
par(mfrow=c(1,2))
acf(yt3s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,0,1)') 
pacf(yt3s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,0,1)')


#---- Simulación de un MA(1) no estacionario ----

set.seed(0891) #Para que los valores de la simulación no cambien
yt3n = ts(arima.sim(model= list(order = c(0,1,1), ma=c(0.7)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt3n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,1,1)",lty=1, lwd=0.4, col="blue")

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,1,1) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente.
lags=20
par(mfrow=c(1,2))
acf(yt3n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,1,1)') 
pacf(yt3n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,1,1)')



#--- 

#---- Simulación de un MA(2) estacionario ----

set.seed(10181) #Para que los valores de la simulación no cambien
yt4s = ts(arima.sim(model= list(order = c(0,0,2), ma=c(0.7,0.25)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt4s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,0,2)",lty=1, lwd=0.18, col="red")

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,0,2)
lags=20
par(mfrow=c(1,2))
acf(yt4s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,0,2)') 
pacf(yt4s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,0,2)')


#---- Simulación de un MA(2) no estacionario ----

set.seed(9171) #Para que los valores de la simulación no cambien
yt4n = ts(arima.sim(model= list(order = c(0,1,2), ma=c(0.7,0.25)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt4n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,1,2)",lty=1, lwd=0.4, col="blue")

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,1,2) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente.
lags=20
par(mfrow=c(1,2))
acf(yt4n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,1,2)') 
pacf(yt4n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,1,2)')


#---

#---- Simulación de un ARMA(1,1) estacionario ----

set.seed(41561) #Para que los valores de la simulación no cambien
yt5s = ts(arima.sim(model= list(order = c(1,0,1), ar=c(0.6),ma=c(0.7)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt5s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (1,0,1)",lty=1, lwd=0.2, col="red")

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,0,1)
lags=20
par(mfrow=c(1,2))
acf(yt5s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,0,1)') 
pacf(yt5s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,0,1)')


#---- Simulación de un ARMA(1,1) no estacionario ----

set.seed(81711) #Para que los valores de la simulación no cambien
yt5n = ts(arima.sim(model= list(order = c(1,1,1),ar=c(0.6), ma=c(0.7)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt5n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (1,1,1)",lty=1, lwd=0.2, col="blue")

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,1,1) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente.
lags=20
par(mfrow=c(1,2))
acf(yt5n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,1)') 
pacf(yt5n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,1)')
