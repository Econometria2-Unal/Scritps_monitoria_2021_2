##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECON?MICAS
#                         ECONOMETRIA II - 2021-I
#      SESIÓN MONITORIA : Metodologia Box-Jenkins para la identificación, 
#               estimación y pronóstico de series de tiempo univariadas
#               Series modeladas: 
#                   - Índice de producción industrial  
#                   - spread entre la tasa de interés de bonos norteamericanos a 5 años y Tbills
##____________________________________________________________________________________
##_____________________________________________________________________________________

#Activamos algunos paquetes que vamos a utilizar en toda la metodolog?a

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

# Limpiar el Environment
rm(list = ls()) 

#
#---- Spread between 5 year Treasury bonds rate and 3 month TBills (spread) ----
#

# Vamos a trabajar con una serie del spread entre la tasa de interés del bono del tesoro 
# de los Estados Unidos a 5 años y la tasa de interés de los Tbills a 3 meses

# Ojo: La tasa de interés de los bonos del tesoro nortemearicano a 3 meses es comunmente 
#      usada como la tasa libre de riesgo de los mercados financieros por lo que 
#      ese spread modelado se podría ver como el exceso de retorno de los bonos de 5 años del tesoro

# Fuente: http://time-series.net/data_sets (Tomados de la página oficial de Walter Enders)

#---- PASO 1- IDENTIFICACIÓN spread ----

# Se cargan las series de tiempo
quarterly <- read_excel(file.choose())

quarterly = quarterly %>% 
  mutate(spread = r5 - Tbill) 

spread = xts(quarterly$spread, order.by = quarterly$DATE)

# Gráfica base de datos 
x11()
plot(spread, main = "spread entre los bonos a 5 años y los Tbills a 3 meses del Tesoro de los EE.UU.", ylab  = "spread")
abline(h = mean(spread), col = "green")

# ACF y PACF
lags=24
x11()
ggAcf(spread,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del spread")
ggPacf(spread,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del spread")

# Prueba de Augment Dickey Fuller

# Prueba con trend

adf.trend= ur.df(spread, type="trend", selectlags = "AIC")
summary(adf.trend)

# Prueba con drift

adf.drift= ur.df(spread, type="drift", selectlags = "AIC")
summary(adf.drift) 

# Prueba sin términos determínisticos

adf.none= ur.df(spread, type="none", selectlags = "AIC")
summary(adf.none) 

# Se trabajara directamente con la serie original dado los resultados de las pruebas de raíz unitaria 
# En particular, que la serie es estacionaria

# Criterios de información para la identificación del modelo: 

#---- Funciones para idenficar modelo ARIMA por criterios de información ----

# 1. Función para seleccionar el modelo ARIMA con el menor criterio de información
arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool_trend, metodo){
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool_trend, 
                    method = metodo)
      df[index,] = c(p, d, q, AIC(fitp), BIC(fitp))
      index = index + 1
    }
  }  
  return(df)
}

# 2. Creo la función arma_min_AIC para seleccionar el mejor modelo según AIC.
arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}

# 3. Creo la función arma_min_BIC para seleccionar el mejor modelo según BIC
arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}

# Seleccionar el modelo (en este caso d = 0 porque no hay que diferenciar la serie dado que es estacionaria)
AR.m = 8; MA.m = 8
df_criterios_info = arma_seleccion_df(spread, AR.m, MA.m, d = 0, TRUE, "ML")
min_aic_0 = arma_min_AIC(df_criterios_info) # selecciona el min AIC 
min_bic_0 = arma_min_BIC(df_criterios_info) # selecciona el min BIC

# Nota: El modelo con el menor BIC es un ARIMA(1,0,1), no obstante, ese modelo no satisface el supuesto de no correlacion 
#       serial en los residuales por lo que es necesario utilizar el modelo con el segundo menor BIC el cual es un ARIMA(1,0,2)

# Selecciono el modelo ARIMA(1,0,2) dado por el criterio de información BIC dado que es más parsimonioso
# que el modelo ARIMA(5,0,5) dado por el criterio de info. AIC

# Por auto.arima
auto.arima(spread, method = "ML")

#---- PASO 2- ESTIMACIÓN spread ----

spread = as.ts(spread)  # es necesario transformar el objeto de xts a ts de nuevo para poder usar la función forecast del paquete forecast
arima_1.0.2_d0 = arima(spread,
                       order = c(1,0,2), include.mean = T, 
                       method = "ML"); summary(arima_1.0.2_d0) # modelamiento ARIMA (1,0,1)

#---- PASO 3- VALIDACIÓN spread ----

## ACF y PACF para el modelo con d = 0
x11()
ggAcf(residuals(arima_1.0.2_d0))
ggPacf(residuals(arima_1.0.2_d0)) # la ACF y PACF parece indicar que hay correlación serial en los residuales del modelo

# Pruebas formales: 

# 1. Pruebas de correlación serial en los residuales

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(spread)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(arima_1.0.2_d0),lag=lags.test, type = c("Box-Pierce")) #No Rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima_1.0.2_d0),type='Box-Pierce',lag=20) #No Rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima_1.0.2_d0),type='Box-Pierce',lag=30) #No Rechazo H0, se cumple el supuesto.
# Test Ljung-Box para autocorrelación en los residuales.
Box.test(residuals(arima_1.0.2_d0),lag=lags.test, type = c("Ljung-Box")) #No Rechazo H0, se cumple el supuesto.
Box.test(residuals(arima_1.0.2_d0),type='Ljung-Box',lag=20) #No Rechazo H0, se cumple el supuesto.
Box.test(residuals(arima_1.0.2_d0),type='Ljung-Box',lag=30) #No Rechazo H0, se cumple el supuesto.

# 2. Pruebas de Heterocedasticidad en los residuales

arch <-arch.test(arima_1.0.2_d0, output=TRUE) #Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.

# 3. Pruebas de normalidad en los residuales

#QQ-PLOT 
par(mfrow=c(1,1))
Residuales=arima_1.0.2_d0$residuals
qqPlot(Residuales)

#Prueba Jarque-Bera
jarque.bera.test(residuals(arima_1.0.2_d0)) #Se rechaza H0, no hay normalidad. 

#---- PASO 4- PRONÓSTICO spread ----

# Pronóstico 12 pasos adelante: 

## Utilizando el comando forecast del paquete forecast 
forecast.spread <- forecast(arima_1.0.2_d0, lead = 12, alpha = 0.05,output = T); forecast.spread
plot(forecast.spread)
autoplot(forecast.spread)

## Utilizando el comando sarima.for del paquete astsa
sarima.for(spread, n.ahead = 12, p = 6, d = 1, q = 3)

#Predicción sobre la muestra

## fitted values del modelo ARIMA(1,0,2) estimado
fit_0 <- spread - residuals(arima_1.0.2_d0)

x11()
plot.ts(spread,type="l",main="Spread observado vs spread estimado",lwd=2)
points(fit_0,col="green",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","green"),lty=1,lwd=2)
