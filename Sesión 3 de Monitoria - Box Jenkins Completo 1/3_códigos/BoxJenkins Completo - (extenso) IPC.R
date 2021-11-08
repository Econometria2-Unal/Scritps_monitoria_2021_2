##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2021-II
#      SESIÓN MONITORIA : Metodologia Box-Jenkins para la identificación, 
#               estimación y pronóstico de series de tiempo univariadas
#               Series modeladas: 
#                   - Índice de precios al consumidor de los Estados Unidos
#                   - spread entre la tasa de bonos norteamericanos a 5 años y Tbills
##____________________________________________________________________________________
##_____________________________________________________________________________________

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

# Limpiar el Environment
rm(list = ls())

# Nota: En R uno puede trabajar series de tiempo con 3 tipos de objetos diferentes:
## ts: Intervalos de tiempo fijos, objeto estándar de series de tiempo en R (paquete stats)
## zoo: Objeto para trabajar modelamiento de series de tiempo financieras
## xts: Objeto de series de tiempo más poderoso en R
##      Permite: Trabajar con series que no tienen intervalos fijos (muy útil para series de tiempo financieras)
##      Se construye sobre los objetos zoo por lo que también hereda toda la capacidad y potencia
##      que tienen los objetos de series de tiempo zoo

#
#---- Consumer's Price Index (CPI) (En español IPC o Índice de precios al consumidor)----
#

# Vamos a trabajar con el índice de precios al consumidor de los EE.UU. el cual es un indicador económico
# que aproxima el nivel de precios los Estados Unidos. 

# Ojo: Al ser un índice tiene que tener un año base en el cual el valor del índice sea 100
# (para el ejemplo los años base fueron 1982-1984, es decir, entre estos el promedio es Index =100)
# Valores mayores a 100 indican que para ese año corriente el nivel de precios fue mayor que en el año base
# Valores menores a 100 indican que para ese año corriente el nivel de precio fue menor que en el año base

# Vamos a trabajar con una serie del índice de precios al consumidor (IPC) de Estados Unidos 
# Fuente: https://fred.stlouisfed.org/series/CPIAUCSL#0 (Tomados de la FRED de la Reserva Federa de St. Louis)


#---- PASO 1- IDENTIFICACIÓN IPC ----

# Se cargan las series de tiempo
base_fred <- read_excel(file.choose())
view(base_fred)

ipc = ts(base_fred$CPIAUCSL, start = 1990, frequency = 12) # Se coloca 12 para indiciar que la frecuencia es mensual

# Visualización de los datos
View(ipc)  

# Crear un objeto tipo xts
ipc_xts = xts(base_fred$CPIAUCSL, 
                  order.by = base_fred$observation_date) # Importar como un objeto xts

View(ipc_xts)

# Ver la clase que son cada uno de los ejemplos de series de tiempo
class(ipc)
class(ipc_xts)

# Gráficas

## Para el objeto ts
x11()   
# Usando la función estándar de gráficación en R
plot.ts(ipc, xlab="",ylab="", main="IPC mensual de Estados Unidos (1990-2020)",lty=1, lwd=2, col="blue")
# Usando ggplot2
autoplot(ipc, col="green", main = "IPC mensual de Estados Unidos (1990-2020)",xlab = "Fecha", ylab="", lwd=1)

# Grafica para el objeto xts
x11()
# Gráfica mucho más linda e informativa
plot(ipc_xts, main = "IPC mensual de Estados Unidos (1990-2020)", ylab  = "IPC")

#Vamos a graficar la ACF y PACF de la serie en nivel.
lags=24
par(mfrow=c(1,2))
acf(ipc,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del IPC de USA') 
pacf(ipc,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del IPC de USA')
# Versíón ggplot:
x11()
ggAcf(ipc,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del IPC de USA")
ggPacf(ipc,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del IPC de USA")
# Versión paquete astsa (sirve tanto para objetos ts como para objetos xts)
acf2(ipc)

# Prueba de Augment Dickey Fuller

# La ADF es una prueba estándar y muy utilizada para saber si una serie de tiempo tiene al menos una raíz unitaria o no.
# La presencia de una raíz unitaria hace que la serie de tiempo estudiada no sea estacionaria. A la hora de utilizar
# la metodología Box Jenkins para series ARIMA es importante trabajar con una serie estacionaria, o, con una una serie
# no estacionaria debilmente que esté diferenciada (pueden ser varias diferencias). Esto permite que la serie se comporte
# como un proceso estacionario, lo cual muestra la importancia de la prueba ADF para la modelación de series estacionarias

# Dado que aún no hemos visto pruebas de raíz unitaria, se colocará el resultado de la prueba completa (tendencia + deriva)
# En scripts posteriores se les indicará cómo es la mecánica de la prueba y cómo interpretar sus resultados

# Prueba con trend

adf.trend= ur.df(ipc, type="trend", selectlags = "AIC")
summary(adf.trend)

# Prueba con drift

adf.drift= ur.df(ipc, type="drift", selectlags = "AIC")
summary(adf.drift)

# Prueba none (en caso de que phi1 no sea significativo)

adf.none= ur.df(ipc, type="none", selectlags = "AIC")
summary(adf.none)

# Conclusión Prueba de Dickey Fuller: 

# 1. Los resultados de la prueba indican que la serie debería tener un término de 
# deriva (drift)

# 2. Y que la serie no es estacionaria, por lo que hay que diferenciarla para 
# eliminar al menos la primera raíz unitaria. Si queremos ver si hay más de una raíz, es
# necesario repetir el procedimiento sobre la serie diferencia

# Nota: presencia de raíz unitaria es lo mismo que la existencia de una tendencia 
#       estócástica en la serie. Solución: Diferenciar la serie

# La serie presente tiene una tedencia estocástica y el término de deriva es el que hace
# que los valores de la serie crezcan a lo largo del tiempo

# Es estándar, en la práctica, hacer tres transformaciones para la serie: 

## 1. Aplicar diff(serie_original): 
# Para diferencia la serie y eliminar la tendencia estocástica

d.ipc = diff(ipc) # serie diferenciada

## 2. Aplicar log(serie_original): 
# para estabilizar la varianza de la serie

l.ipc =log(ipc) # serie que se le aplica solo el logaritmo 

## Aplicar diff(log(serie_original)): 
# Para diferenciar y estabilizar la varianza de la serie
# Se interpretra como una tasa de crecimiento   

# Inflación mensual 
dl.ipc = diff(log(ipc))*100   # diferencia de logaritmos de la serie (tasa de crecimiento)

# Inflación anual
dl_anual.ipc = diff(log(ipc), lag = 12, differences = 1)*100   # diferencia de logaritmos de la serie (tasa de crecimiento)

#Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su valor en logaritmos.
x11()
par(mfrow=c(2,2))
plot.ts(ipc, xlab="",ylab="", main="IPC en nivel para Estados Unidos 1990-2021",lty=1, lwd=2, col="blue")
plot.ts(l.ipc, xlab="",ylab="", main="IPC en logaritmo para Estados Unidos 1990-2021",lty=1, lwd=2, col="black")
plot.ts(d.ipc, xlab="",ylab="", main="Variación del IPC para Estados Unidos 1990-2021",lty=1, lwd=2, col="red")
plot.ts(dl.ipc, xlab="",ylab="", main="Inflación para Estados Unidos 1990-2021",lty=1, lwd=2, col="green")

# Vamos a elegir la variación porcentual del IPC (i.e.,inflación mensual) debido a que la varianza es mucho más estable.

#Ahora hacemos la ACF y la PACF, para la tasa de crecimiento del ipc donde evidenciamos un proceso débilmente dependiente. 
lags=30
par(mfrow=c(1,2))
acf(dl.ipc,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la inflación mensual') 
pacf(dl.ipc,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la inflación mensual')

ggAcf(dl.ipc,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF de la inflación de USA mensual")
ggPacf(dl.ipc,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF de la inflación de USA mensual")

ADF.dl.ipc <- ur.df(dl.ipc, type="trend", lags = 2); plot(ADF.dl.ipc)
summary(ADF.dl.ipc) #Rechazamos H0, así que la la inflación es estacionaria en sentido débil.

#Vamos a analizar las estadísticas descriptivas de la serie en primera diferencia. 
describe(dl.ipc) #Parece que la media es distina a cero, por eso se incluye un intercepto en los arima
describe(l.ipc)

#
# Uso de criterios de información para la identificación del proceso ARIMA que se está modelando 
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
# ARIMA(p, d, q) donde p y q varían entre 0 y 1. Nótese que acá estoy utilizando la serie l.ipc
# dado que el órden de integración d = 1  me diferencia la serie

mod_d1 = arma_seleccion_df(l.ipc, AR.m, MA.m, d = 1, TRUE, "ML")
view(mod_d1)

# Selecciono el mejor modelos según criterios de información cuando d=1
min_aic_1 = arma_min_AIC(mod_d1); min_aic_1 #ARIMA (5,1,5)
min_bic_1 = arma_min_BIC(mod_d1); min_bic_1 #ARIMA (1,1,2)

# Acá estoy trabajando con la serie diferenciada debido a que d = 0 

mod_d0 = arma_seleccion_df(dl.ipc, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod_d0)

# Selecciono el mejor modelos según criterios de información cuando d=0
min_aic_0 = arma_min_AIC(mod_d0); min_aic_0 #ARIMA (5,0,4)
min_bic_0 = arma_min_BIC(mod_d0); min_bic_0 #ARIMA (0,0,1)

# Nota: 
# Para ambos modelos se selecciona el modelo sugerido por el criterio de información de AIC
# por las características de la serie. Al estar modelando la inflación mensual
# de los EE.UU. se espera que las correlaciones sean persistentes y ésto tiene sentido, 
# pues existen muchos choques que pueden mantenerse durante dos o tres meses. 
# En otras palabras, deberían tener modelos con más rezagos autorregresivos y de 
# promedio móvil para capturar esas correlaciones
# La selección de modelos por BIC suele dar modelos más parsimoniosos porque 
# penaliza más fuerte agregar más regresores que el criterio AIC. Si bien lo anterior, 
# puede ser conveniente en algunos casos, para la serie en estudio, y teniendo en cuenta
# que la inflación mensual puede seguir teniendo efectos 3 meses adelantes los modelos
# sugeridos por el criterio de información de BIC son excecivamente parsiomoniosos para
# la serie en estudio y por tanto se prefiere en este caso usar el criterio de AIC

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

auto.arima(l.ipc, method = "ML")  #(0,2,4) (0,0,1)


# Ojo: Muy importante
# En la práctica lo mejor es usar tanto el método manual como el auto.arima
# Basarse exclusivamente en el modelo auto.arima generalmente conduce a 
# conlusiones erróneas por lo que no basta solo usar el modelo auto.arima.
# El método manual siempre debe realizarse y debe ser la principal guía, mientras que
# el auto.arima debe verse como un método de identificación complementario 

# Ojo: Es muy importante a la hora de seleccionar un modelo ARIMA y aproximar 
# el proceso generador de datos, usar tanto la gráfica de la serie, las ACF y 
#la PACF como los criterios de información 

#---- PASO 2- ESTIMACIÓN IPC ----

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

# Nota: De acá en adelante utilizaremos los modelos que se obtuvieron utilizando el criterio de información AIC
# Pese a qué en series de tiempo es más común usar BIC para seleccionar el modelo se conseja también observar los resultados
# cuando se escoge el modelo porpuesto por el AIC

# Nota: Cómo ya se indicó previamente, para mayor precisión en las estimaciones
# y para evitar algunos inconvenientes que a veces ocurren al utilizar 
# el método CSS la estimación del arima se hará mediante maxima verosimilitud

arima_5.1.5_d1 = arima(l.ipc, order = c(5,1,5), include.mean = T, method = "ML"); summary(arima_5.1.5_d1) # modelamiento ARIMA (5,1,5)

arima_5.0.4_d0 = arima(dl.ipc, order = c(5,0,4), include.mean = T, method = "ML"); summary(arima_5.0.4_d0) # modelamiento ARIMA (5,0,4)

#---- PASO 3- VALIDACIÓN IPC ----

# El supuesto más importante que se debe validar es que los residuales estimados se comporten como un ruido blanco. 
# Es decir, que la media de los residuales sea cero, la varianza constante y la covarianza sea cero

# Vamos a realizar el análisis de residuales para cada modelo 

#
# arima_5.1.5_d1
#

## ACF y PACF para el modelo con d = 1
x11()
ggAcf(residuals(arima_5.1.5_d1))
ggPacf(residuals(arima_5.1.5_d1)) # la ACF y PACF parece indicar que hay correlación serial en los residuales del modelo

# Pruebas formales: 

# 1. Pruebas de correlación serial en los residuales

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(l.ipc)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(arima_5.1.5_d1),lag=lags.test, type = c("Box-Pierce")) #No rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima_5.1.5_d1),type='Box-Pierce',lag=20) #No Rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima_5.1.5_d1),type='Box-Pierce',lag=30) #No Rechazo H0, se cumple el supuesto.
# Test Ljung-Box para autocorrelación en los residuales.
Box.test(residuals(arima_5.1.5_d1),lag=lags.test, type = c("Ljung-Box")) #No Rechazo H0, se cumple el supuesto.
Box.test(residuals(arima_5.1.5_d1),type='Ljung-Box',lag=20) #No Rechazo H0, se cumple el supuesto.
Box.test(residuals(arima_5.1.5_d1),type='Ljung-Box',lag=30) #No Rechazo H0, se cumple el supuesto.

# Nota: Como no se rechaza la H0 para los dos test anteriores significa que el modelo arima escogido para modelar la serie l.ipc
# capturar la estructura de correlación en la serie, ya que, como se ve, no hay presencia de correlación en los residuales.
# Cuando se viola este supuesto, una solución sería agregar más rezagos autoregresivos o algunas componente de media móvil. 
# No obstante, otra alternativa es utilizar el modelo arima seleccionado por el AIC o el segundo mejor modelo escogido por el BIC. 
# En todo caso, es necesario volver a estimar un modelo arima con coeficientes diferentes dado que es importante que 
# no haya presencia notoria de correlación serial en los residuales del modelo arima estimado.

# 2. Pruebas de Heterocedasticidad en los residuales

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedásticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch <-arch.test(arima_5.1.5_d1, output=TRUE) # Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie

# 3. Pruebas de normalidad en los residuales

#QQ-PLOT 
par(mfrow=c(1,1))
Residuales=arima_5.1.5_d1$residuals
qqPlot(Residuales)

#Prueba Jarque-Bera
jarque.bera.test(residuals(arima_5.1.5_d1)) #Se rechaza H0, no hay normalidad. 

#
# arima_5.0.4_d0
#

## ACF y PACF para el modelo con d = 0
x11()
ggAcf(residuals(arima_5.0.4_d0))
ggPacf(residuals(arima_5.0.4_d0)) # la ACF y PACF parece indicar que no hay correlación serial en los residuales del modelo 

# Pruebas formales: 

# 1. Pruebas de correlación serial en los residuales

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(dl.ipc)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(arima_5.0.4_d0),lag=lags.test, type = c("Box-Pierce")) #No Rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima_5.0.4_d0),type='Box-Pierce',lag=30) #No Rechazo H0, se cumple el supuesto.
Box.test(residuals(arima_5.0.4_d0),type='Box-Pierce',lag=20) #No Rechazo H0, se cumple el supuesto. 

# Test Ljung-Box para autocorrelación en los residuales.
Box.test(residuals(arima_5.0.4_d0),lag=lags.test, type = c("Ljung-Box")) #No Rechazo H0, se cumple el supuesto.
Box.test(residuals(arima_5.0.4_d0),type='Ljung-Box',lag=30) #No Rechazo H0, se cumple el supuesto.
Box.test(residuals(arima_5.0.4_d0),type='Ljung-Box',lag=20) #No Rechazo H0, se cumple el supuesto.


# 2. Pruebas de Heterocedasticidad en los residuales

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch <-arch.test(arima_5.0.4_d0, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie

# 3. Pruebas de normalidad en los residuales

#QQ-PLOT 
par(mfrow=c(1,1))
Residuales=arima_5.0.4_d0$residuals
qqPlot(Residuales)

#Prueba Jarque-Bera
jarque.bera.test(residuals(arima_5.0.4_d0)) #Se rechaza H0, no hay normalidad. 

#---- PASO 4- PRONÓSTICO IPC ----

# Nuevamente acá solo se hará los pronósticos utilizando los resultados del AIC

# Pronóstico pasos adelante 

# Nota: Los intervalos de confianza que aparecen en dichos pronósticos no son buenos dado que no se cumple 
# el supuesto de normalidad en los residuales

## Utilizando el comando forecast del paquete forecast 
forecast.l.ipc <- forecast(arima_5.1.5_d1, lead = 12, alpha = 0.05, output = T)   # Pronóstico 12 pasos adelante.
forecast.l.ipc

## Utilizando el comando sarima.for del paquete astsa
sarima.for(l.ipc, n.ahead = 12, p = 5, d = 1, q = 5)

#Ahora vamos a ver el ajuste dentro de muestra

## Para la series l.PIB
fit_1 <- l.ipc - residuals(arima_5.1.5_d1)
## Para la series dl.PIB
fit_0 <- dl.ipc - residuals(arima_5.0.4_d0)

#Predicción sobre la muestra

x11()
plot.ts(l.ipc,type="l",main="log(ipc) ajustada VS log(ipc) observada",lwd=2)
points(fit_1,col="blue",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","blue"),lty=1,lwd=2)

x11()
plot.ts(dl.ipc,type="l",main="Inflación ajustada VS Inflación observada",lwd=2)
points(fit_0,col="green",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","green"),lty=1,lwd=2)

#
# Notas finales
#

# Como pudieron notar los modelos ARMA son modelos que están diseñados para modelar series estacionarias en covarianza
## Eso implica que uno debería utilizar un modelo ARMA cuando las series se comportan de forma estable en torno a una media 
## o atractor, no tener una dinámica muy volátil e irregular, y no tener correlaciones muy persistentes entre los datos (los
## choques deben ser transitorios). Para corroborar que una serie se comporte como un proceso estacionariio, es necesario verificar
## que los residuales se comporten como un ruido blanco

#
# Notas sobre volatilidad
#

# Como lo muestra la gráfica: 
plot(diff(log(ipc_xts)), main = "Inflación mensual de Estados Unidos (1990-2020)", ylab  = "Inflación EE.UU.")

# La variación mensual del IPC pareciera presentar clusters de volatilidad, es decir, no hay una varianza constante a lo largo de la serie
# Sino que pareciera que en determinados periodos de tiempo hay un tipo de volatilidad y en otro periodo de tiempo dicha volatilidad cambia

# El anterior comportamiento es muy común en muchas series de tiempo 
# En particular, dicho comportamiento de presencia de varios clusters de volatilidad en una misma serie tiende a ser común 
# en series de tiempo financieras. 

# Dado que los modelos ARMA están diseñados para series con varianza constante se requiere de otro tipo de modelos capaces de capturar esos
# esos clusters de volatilidad. Para ello, se emplean modelos capaces de capturar modelar la varianza variable en el tiempo del proceso generador de datos
# Dichos modelos son: 
### arch: Extensión del modelo AR para modelar series de tiempo con clusters de volatilidad
### garch: Extensión del modelo ARMA para modelar serie de tiempo con clusters de volatilidad 

#
# Nota adicional: Comentarios finales (tema avanzado/opcional)
#
## Recordar que los pronósticos puntuales para el modelamiento de media del
## de un modelo ARMA son insesgados. No obstante, por la presencia de 
## heterocedasticidad y no normalidad los intervalos de confianza no son fiables
## y por ende queda incertidumbre asociado al grado de confianza del pronóstico
## del modelo
## Mecánismos para corregir por la presencecia de heterocedasticidad 
## y no normalidad en los residuales de un modelo ARMA
### 1. Usar bootstrapping permite tratar los problemas de no normalidad
###    en una serie. No obstante, si la serie presente tanto no normalidad
###    como heterocedasticidad condicional (errores ARCH) se puede hacer
###    un bootstrapping especial que sea capaz de tratar ambos problemas
###    como un bootstrapping por bloques (MBB: moving-block-bootstrapping)(tema más avanzado)
### 2. Modelar la heterocedasticidad condicional del proceso
###    (e.g. mediante un modelo GARCH(1,1)) y utilizar una distribución 
###    diferente para modelar los errores no normales. E.g. si los residuales
###    parecieran reflejar colar gruesas y presentaran asimetría se podría
###    modelar los residuales mediante una distribución t con asimetría
###    u otra distribución que se ajuste mejor a los residuales del proceso
###    Todo lo anterior se puede realizar mediante el paquete *rugarch*
###    Nota: Esta segunda opción es muy fácil de realiar si se sabe manejar el paquete *rugarch* y corrige 
###          Tanto problema de heterocedasticidad como de normalidad
###    Nota: La distribución t modela mejor que la normal los procesos
###    que presentan colas gruesas (es decir que tienen kurtosis más altas que 3 (la kurtosis de una normal))


