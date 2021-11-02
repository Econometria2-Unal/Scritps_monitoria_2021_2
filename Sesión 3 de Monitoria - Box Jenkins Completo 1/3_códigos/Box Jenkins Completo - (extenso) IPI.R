##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2021-I
#      SESIÓN MONITORIA : Metodologia Box-Jenkins para la identificación, 
#               estimación y pronóstico de series de tiempo univariadas
#               Series modeladas: 
#                   - Índice de producción industrial  
#                   - spread entre la tasa de bonos norteamericanos a 5 años y Tbills
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

# Nota: En R uno puede trabajar series de tiempo con 3 objetos:
# ts  (Intervalos de tiempo fijos, objeto estándar de series de tiempo en R)
# zoo
# xts (muy útil para el modelamiento de series financieros con intervalos de tiempo que nos son fijos)

#
#---- Industrial production index (indprod) ----
#

# Vamos a trabajar con el industrial production index de los EE.UU. el cuál es un indicador económico
# que mide la producción real de los Estados Unidos en términos de manufactura, minería, electicidad y gas. 

# Ojo: Al ser un índice tiene que tener un año base en el cuál el valor del índice sea 100
       # (para el ejemplo el año base fue 2012, es decir Index 2012=100)
       # Valores mayores a 100 indican que para ese año corriente la producción manufacturera fue mayor que en el año base
       # Valores menores a 100 indican que para ese año corriente la producción manufacturera fue menor que en el año base

# Vamos a trabajar con una serie del índice de produccion industrial (indprod) de Estados Unidos 
# Fuente: https://fred.stlouisfed.org/series/INDPRO#0 (Tomados de la FRED de la Reserva Federa de St. Louis)


#---- PASO 1- IDENTIFICACIÓN indprod ----

# Se cargan las series de tiempo
base_fred <- read_excel(file.choose())

indprod = ts(base_fred$INDPRO, start = 1960, frequency = 4) # Se coloca 4 para indiciar que la frecuencia es trimestral

# Visualización de los datos
View(indprod)  

# Crear un objeto tipo xts
indprod_xts = xts(base_fred$INDPRO, 
                  order.by = base_fred$observation_date) # Importar como un objeto xts

# Ver la clase que son cada uno de los ejemplos de series de tiempo
class(indprod)
class(indprod_xts)

# Gráficas

## Para el objeto ts
x11()   
# Usando la función estándar de gráficación en R
plot.ts(indprod, xlab="",ylab="", main="IPI trimestral de Estados Unidos (1960-2012) (Index 2012 = 100)",lty=1, lwd=2, col="blue")
# Usando ggplot2
autoplot(indprod, col="green", main = "IPI trimestre de Estados Unidos (1960-2012) (Index 2012 = 100)",xlab = "Fecha", ylab="", lwd=1)

# Grafica para el objeto xts
x11()
# Gráfica mucho más linda e informativa
plot(indprod_xts, main = "IPI trimestre de Estados Unidos (1960-2012) (Index 2012 = 100)", ylab  = "IPI")

#Vamos a graficar la ACF y PACF de la serie en nivel.
lags=24
par(mfrow=c(1,2))
acf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PCE de USA') 
pacf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PCE de USA')
# Versíón ggplot:
x11()
ggAcf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA")
ggPacf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")
# Versión paquete astsa (sirve tanto para objetos ts como para objetos xts)
acf2(indprod)

# Prueba de Augment Dickey Fuller

# La ADF es una prueba estándar y muy utilizada para saber si una serie de tiempo tiene al menos una raíz unitaria o no
# La presencia de una raíz unaitaria hace que la serie de tiempo estudiada no sea estacionari. A la hora de utilizar
# la metodología Box Jenkins para series ARIMA es importante trabajar con una serie estacionaria o con una una serie
# no estacionaria debidamente diferenciada para que la serie diferenciada (pueden ser varias diferencias) sea
# estacionaria y de ahí la importancia de la pruba ADF para la identificación de series estacionarias

# Dado que aún no han visto pruebas de raíz unitaria se colocará el resultado de la prueba por completitud 
# Y en scripts posteriores se les indicará como es la mecánica de la prueba y como interpretar sus resultados

# Prueba con trend

adf.trend= ur.df(indprod, type="trend", selectlags = "AIC")
summary(adf.trend)

# Prueba con drift

adf.drift= ur.df(indprod, type="drift", selectlags = "AIC")
summary(adf.drift) 

# Conclusión Prueba de Dickey Fuller: 

# 1. Los resultados de la prueba indican que la serie debería tener término de 
# deriva (drift)

# 2. Y que la serie no es estacionaria por lo que hay que diferenciarla para 
# eliminar raíces unitarias

# Nota: presencia de raíz unitaria es lo mismo que la existencia de una tendencia 
#       estócástica en la serie. Solución: Diferenciar la serie

# La serie presente una tedencia estocástica y el término de deriva es el que hace
# que los valores de la serie crezcan en el tiempo

# Es estándar en la práctica hacer tres transformaciones para la serie: 

## 1. Aplicar diff(serie_original): 
      # Para diferencia la serie y eliminar la tendencia estocástica

d.indprod= diff(indprod) # serie diferenciada

## 2. Aplicar log(serie_original): 
      # para estabilizar la varianza de la serie

l.indprod=log(indprod) # serie que se le aplica solo el logaritmo 

## Aplicar diff(log(serie_original)): 
    # Para diferenciar y estabilizar la varianza de la serie
    # Se interpretra como una tasa de crecimiento   

dl.indprod= diff(log(indprod))*100   # diferencia de logaritmos de la serie (tasa de crecimiento)

#Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su valor en logaritmos.
x11()
par(mfrow=c(2,2))
plot.ts(indprod, xlab="",ylab="", main="indprod en nivel para Estados Unidos 1959-2019",lty=1, lwd=2, col="blue")
plot.ts(l.indprod, xlab="",ylab="", main="indprod en logaritmo para Estados Unidos 1959-2019",lty=1, lwd=2, col="black")
plot.ts(d.indprod, xlab="",ylab="", main="Variación del indprod para Estados Unidos 1959-2019",lty=1, lwd=2, col="red")
plot.ts(dl.indprod, xlab="",ylab="", main="Tasa de crecimiento del indprod para Estados Unidos 1959-2019",lty=1, lwd=2, col="green")

# Vamos a elegir la tasa de crecimiento de indprod debido a que la varianza es mucho más estable, pese a 
# tener la misma media constante que la serie en primera diferencia.

#Ahora hacemos la ACF y la PACF, para la tasa de crecimiento del indprod donde evidenciamos un proceso débilmente dependiente. 
lags=30
par(mfrow=c(1,2))
acf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la tasa de crecimiento del PIB') 
pacf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la tasa de crecimiento del PIB')

ggAcf(dl.indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA")
ggPacf(dl.indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")

ADF.dl.indprod <- ur.df(dl.indprod, type="drift", selectlags = "AIC")
summary(ADF.dl.indprod) #Rechazamos H0, así que la tasa de crecimiento del PIB es estacionaria en sentido débil.

#Vamos a analizar las estadísticas descriptivas de la serie en primera diferencia. 
describe(dl.indprod) #Parece que la media es distina a cero, por eso se incluye un intercepto en los arima
describe(l.indprod)

#
# Uso de criterios de información para la identificación del proceso ARIMA que se esté modelando 
#

# Método manual para identificar el ARIMA usando criterios de información 

#Ahora vamos a ver lo que muestran los criterios AIC y BIC
AR.m <- 6 #Supondremos que el rezago autorregresivo máximo es 6 (pmax)
MA.m <- 6 #Supondremos que el rezago de promedio móvil máximo es 6. (qmax)

#---- Funciones para idenficar modelo ARIMA por criterios de información ----

# 1. Función para seleccionar el modelo ARIMA con el menor criterio de información

# función que me permite crear un data frame para distintos modelos ARIMA(p,d,q)
# en donde cada fila del df me da el orden del modelo y los AIC y BIC correspondientes a dicho modelo
# Ojo: utlizar method = "ML" dentro de arima(), de lo contrario les arrojará error
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

# Llamo la función arma_selection_df para construir un data frame con todos los posibles modelos
# ARIMA(p, d, q) donde p y q varían entre 0 y 1. Nótese que acá estoy utilizando la serie dl.indprod
# dado que el órden de integración d = 1  me diferencia la serie

mod_d1 = arma_seleccion_df(l.indprod, AR.m, MA.m, d = 1, TRUE, "ML")
view(mod_d1)

# Selecciono el mejor modelos según criterios de información cuando d=1
min_aic_1 = arma_min_AIC(mod_d1); min_aic_1 #ARIMA (3,1,5)
min_bic_1 = arma_min_BIC(mod_d1); min_bic_1 #ARIMA (1,1,0)

# Acá estoy trabajando con la serie diferenciada debido a que d = 0 

mod_d0 = arma_seleccion_df(dl.indprod, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod_d0)

# Selecciono el mejor modelos según criterios de información cuando d=0
min_aic_0 = arma_min_AIC(mod_d0); min_aic_0 #ARIMA (2,0,4)
min_bic_0 = arma_min_BIC(mod_d0); min_bic_0 #ARIMA (2,0,4)

#
# Método automático para identificar el ARIMA usando criterios de información 
# Usando la función auto.arima() del paquete forecast
#

# Nota: La misma función auto.arima es capaz de reconocer el orden de diferenciación
# Para que la serie quede estacionaria. 
# En realidad la función auto.arima calcula un modelo SARIMA porque además de 
# modelas la parte ARMA de la serie también modela la componente estacional de 
# la serie
# Para mayor info. sobre los modelos SARIMA: Ver capítulo 2 sección 11: Seasonality
auto.arima(l.indprod, method = "ML")

# Ojo: Muy importante
      # En la práctica lo mejor es usar tanto el método manual como el auto.arima
      # Basarse exclusivamente en el modelo auto.arima generalmente conduce a 
      # conlusiones erroneas por lo que no basta solo usar el modelo auto.arima
      # el método manual siempre debe realizarse y debe ser la principal guía
      # el auto.arima debe verse como un método de identificación complementario 

# Ojo: Es muy importante a la hora de seleccionar un modelo ARIMA para explicar 
       # el proceso generador de datos 
       # usar tanto la gráfica de la serie, las ACF y la PACF y los 
       # criterios de información 

#---- PASO 2- ESTIMACIÓN indprod ----

##
# Una vez se ha identificado la serie, se procede a estimar el modelo ARIMA. 
# Debe tomarse en cuenta que incluir muy pocos rezagos puede llevar a que los 
# residuales del modelo no se comporten como un Ruido Blanco y que incluir 
# muchos parámetros puede llevar a que nuestro modelo no sea parsimonioso/se 
# pierdan muchos grados de libertad y se pierda eficiencia.
##

# Existen 3 métodos de estimación para la función arima: (por default los 3 comandos utilizan CSS)
## ML: Máxima verosimilitud (más preciso y usualmente la mejor opción para bases pequeñas donde se pueda encontrar una solución analítica)
## CSS: (más veloz generalmente. se usa como aproximación o para bases de datos grandisimas)
## CSS-ML: Una combinación de ambas

### ML puede no converger
### CSS puede no hacer estimaciones lo suficientemente precisas y arrojar error 

# Para más información ver: 
# https://stats.stackexchange.com/questions/209730/fitting-methods-in-arima#:~:text=CSS%20sets%20the%20initial%20observations,an%20estimate%20of%20these%20values.


# Nota: Para hacer la estimación de un modelo arima se pueden utilizar 3 funciones distintas: 
## arima: función del paquete stats que es la más usual emplear y sirve para hacer predicciones con la función forecast del paquete forecast
## Arima: función del paquete forecast que es básicamente un wrapper de la función arima
## sarima: función del paquete astsa que permite estimar modelos sarima

# ojo: tanto la función Arima como la función sarima están construidas sobre la función arima()
     # Cualquier de las 3 funciones para estimar un ARIMA permiten estimar modelos SARIMA
     # Es decir, son capaces de modelar la componente estacional de una serie

# Nota: De acá en adelante utilizaremos los modelos que se obtuvieron utilizando el criterio de información BIC
      # Pese a qué en series de tiempo es más común usar BIC para seleccionar el modelo se conseja también observar los resultados
      # cuando se escoge el modelo porpuesto por el AIC

# Nota: Cómo ya se indicó previamente, para mayor precisión en las estimaciones
      # y para evitar algunos inconvenientes que a veces ocurren al utilizar 
      # el método CSS la estimación del arima se hará mediante maxima verosimilitud

arima_1.1.0_d1 = arima(l.indprod, order = c(1,1,0), include.mean = T, method = "ML"); summary(arima_1.1.0_d1) # modelamiento ARIMA (1,1,0)
  
arima_2.0.4_d0 = arima(dl.indprod, order = c(2,0,4), include.mean = T, method = "ML"); summary(arima_2.0.4_d0) # modelamiento ARIMA (2,0,4)

#---- PASO 3- VALIDACIÓN indprod ----

# El supuesto más importante que se debe validar es que los residuales estimados se comporten como un ruido blanco. 
# Es decir, que la media de los residuales sea cero, la varianza constante y la covarianza sea cero

# Vamos a realizar el análisis de residuales para cada modelo 

#
# arima_1.1.0_d1
#

## ACF y PACF para el modelo con d = 1
x11()
ggAcf(residuals(arima_1.1.0_d1))
ggPacf(residuals(arima_1.1.0_d1)) # la ACF y PACF parece indicar que hay correlación serial en los residuales del modelo

# Pruebas formales: 

# 1. Pruebas de correlación serial en los residuales

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(l.indprod)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(arima_1.1.0_d1),lag=lags.test, type = c("Box-Pierce")) #Rechazo H0, no se cumple el supuesto. 
Box.test(residuals(arima_1.1.0_d1),type='Box-Pierce',lag=20) #Rechazo H0, no se cumple el supuesto. 
Box.test(residuals(arima_1.1.0_d1),type='Box-Pierce',lag=30) #Rechazo H0, no se cumple el supuesto.
# Test Ljung-Box para autocorrelación en los residuales.
Box.test(residuals(arima_1.1.0_d1),lag=lags.test, type = c("Ljung-Box")) #Rechazo H0, no se cumple el supuesto.
Box.test(residuals(arima_1.1.0_d1),type='Ljung-Box',lag=20) #Rechazo H0, no se cumple el supuesto.
Box.test(residuals(arima_1.1.0_d1),type='Ljung-Box',lag=30) #Rechazo H0, no se cumple el supuesto.

# Nota: Como se rechaza la H0 para los dos test anteriores significa que el modelo arima escogido para modelar la serie l.indprod
      # No alcanza a capturar toda la estructura de correlación en la serie y eso se refleja en la presencia de correlación en los residuales
      # la solución seria agregar más residuos autoregresivos o algunas componente de media móvil. No obstante, otra alternativa es utilizar
      # el modelo arima seleccionado por el AIC o el segundo mejor modelo escogido por el BIC. En todo caso, es necesario volver a estimar 
      # un modelo arima con coeficientes diferentes dado que es importante que no haya presencia notoria de correlación serial en los residuales del modelo arima estimado

# 2. Pruebas de Heterocedasticidad en los residuales

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch <-arch.test(arima_1.1.0_d1, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie

# 3. Pruebas de normalidad en los residuales

#QQ-PLOT 
par(mfrow=c(1,1))
Residuales=arima_1.1.0_d1$residuals
qqPlot(Residuales)

#Prueba Jarque-Bera
jarque.bera.test(residuals(arima_1.1.0_d1)) #Se rechaza H0, no hay normalidad. 

#
# arima_2.0.4_d0
#

## ACF y PACF para el modelo con d = 0
x11()
ggAcf(residuals(arima_2.0.4_d0))
ggPacf(residuals(arima_2.0.4_d0)) # la ACF y PACF parece indicar que no hay correlación serial en los residuales del modelo 

# Pruebas formales: 

# 1. Pruebas de correlación serial en los residuales

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(l.indprod)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(arima_2.0.4_d0),lag=lags.test, type = c("Box-Pierce")) #No Rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima_2.0.4_d0),type='Box-Pierce',lag=20) #No Rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima_2.0.4_d0),type='Box-Pierce',lag=30) #No Rechazo H0, se cumple el supuesto.
# Test Ljung-Box para autocorrelación en los residuales.
Box.test(residuals(arima_2.0.4_d0),lag=lags.test, type = c("Ljung-Box")) #No Rechazo H0, se cumple el supuesto.
Box.test(residuals(arima_2.0.4_d0),type='Ljung-Box',lag=20) #No Rechazo H0, se cumple el supuesto.
Box.test(residuals(arima_2.0.4_d0),type='Ljung-Box',lag=30) #No Rechazo H0, se cumple el supuesto.

# 2. Pruebas de Heterocedasticidad en los residuales

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch <-arch.test(arima_2.0.4_d0, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie

# 3. Pruebas de normalidad en los residuales

#QQ-PLOT 
par(mfrow=c(1,1))
Residuales=arima_2.0.4_d0$residuals
qqPlot(Residuales)

#Prueba Jarque-Bera
jarque.bera.test(residuals(arima_2.0.4_d0)) #Se rechaza H0, no hay normalidad. 

#---- PASO 4- PRONÓSTICO indprod ----

# Nuevamente acá solo se hará los pronósticos utilizando los resultados del BIC

# Pronóstico pasos adelante 

# Nota: Los intervalos de confianza que aparecen en dichos pronósticos no son buenos dado que no se cumple 
      # el supuesto de normalidad en los residuales

## Utilizando el comando forecast del paquete forecast 
forecast.l.indprod <- forecast(arima_1.1.0_d1, lead = 12, alpha = 0.05,output = T)   # Pronóstico 12 pasos adelante.
forecast.l.indprod

## Utilizando el comando sarima.for del paquete astsa
sarima.for(l.indprod, n.ahead = 12, p = 1, d = 1, q = 0)

#Ahora vamos a ver el ajuste dentro de muestra

## Para la series l.PIB
fit_1 <- l.indprod - residuals(arima_1.1.0_d1)
## Para la series dl.PIB
fit_0 <- dl.indprod - residuals(arima_2.0.4_d0)

#Predicción sobre la muestra

x11()
plot.ts(l.indprod,type="l",main="log(indprod) ajustada VS log(indprod) observada",lwd=2)
points(fit_1,col="blue",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","blue"),lty=1,lwd=2)

x11()
plot.ts(dl.indprod,type="l",main="Tasa de crecimiento ajustada VS Tasa de crecimeinto observada",lwd=2)
points(fit_0,col="green",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","green"),lty=1,lwd=2)

#
# Notas finales
#

# Como pudieron notar los modelos ARMA son modelos que están diseñados para modelar series estacinoarias en covarianza
## Eso implica que uno debería utilizar un modelo ARMA cuando las series se comportan con media constante, varianza constante 
## y autocorrelación que depende solo de la distancia temporal entre las observaciones
## Para corroborar que una serie es estacionario se podría verificar que los residuales del modelo en efecto se comporten como
## Ruido blanco 

#
# Notas sobre volatilidad
#

# Como lo muestra la gráfica: 
plot(diff(log(indprod_xts)), main = "Tasa de crecimiento IPI trimestre de Estados Unidos (1960-2012) (Index 2012 = 100)", ylab  = "tasa de crecimiento IPI")

# La tasa de crecimeiento del IPI pareciera presentar clusters de volatilidad, es decir, no hay una varianza constante a lo largo de la serie
# Sino que pareciera que en determinados periodos de tiempo hay un tipo de volatilidad y en otro periodo de tiempo dicha volatilidad cambia

# El anterior comportamiento es muy común en muchas series de tiempo 
# En particular, dicho comportamiento de presencia de varios clusters diferentes de volatilidad en una misma serie tiende a ser común 
# en series de tiempo financieras. 

# Dado que los modelos ARMA están diseñados para series con varianza constante se requiere de otro tipo de modelos capaces de capturar esos
# esos clusters de volatilidad. Para ello, se emplean modelos capaces de capturar modelar la varianza variable en el tiempo del proceso generador de datos
# Dichos modelos son: 
### arch: Extensión del modelo AR para modelar series de tiempo con clusters de volatilidad
### garch: Extensión del modelo ARMA para modelar serie de tiempo con clusters de volatilidad 
