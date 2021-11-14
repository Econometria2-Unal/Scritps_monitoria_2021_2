##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2021-I
#      SESIÓN MONITORIA: Ejemplos prácticos de uso de pruebas de ADF sobre series reales
##____________________________________________________________________________________
##_____________________________________________________________________________________

# Limpiar el Environment
# rm(list = ls()) 

# Importación de paquetes
library(forecast)
library(lmtest)      # Significancia individual de los coeficientes ARIMA
library(tseries)
library(stargazer)
library(dynlm)    # Paquete que me permite realizar MCOD
library(urca)     # Paquete que me permite realizar pruebas de raíz unitaria 
library(tidyverse)  #Conjunto de paquetes que incluyen ggplot2 para realizar la parte gráfica
library(readxl)     # Para leer archivos excel

# Nota: Para poder usar la función autoplot requiero los paquetes: tidyverse, forecast y lmtest

##
# Procedimiento para determianr el orden de intregración (estacionaridad de una serie): 
##
##### 1. Mirar la gráfica de la serie (la gráfica da una idea general del comportamiento de la serie)
##### 2. Mirar la ACF y la PACF de la serie (El comportamiento de la ACF podría indicar si el proceso es altamente persistente)
##### 3. Conducir la prueba secuencial de Dickey Fuller (de ser necesario)
#         Ojo: Conducir la prueba con las especificaciones correctas 
#              Tener en cuenta que la prueba es sensible a la inclusión de términos determinísticos
##### 4. Ver si luego de diferenciar la serie, la serie diferenciada es estacionaria
#         (Usando de nuevo un test de DF. Si no es estacionario, mirar si una nueva diferencia puede estacionarizar la serie)

# Nota: Recuerden que un test de DF 
# Nota2: En la prática, generalmente, no se usa solo el test de DF sino sus resultados se corroboran con pruebas de raiz unitaria complementarias
#        Un ejemplo, es el uso de la prueba de kpss: cuya hipótesis nula es que la serie es estacionaria (fijense que esto es contrario a la prueba de DF donde la hipótesis nula es no estacionaridad)
# Nota3: Es importante que la serie a estudiar no presente cambio estructural. 
#        De presentar la serie cambio estructural deja de ser válida la prueba de DF 
# Nota4: Los valores críticos que ofrece la función ur.df están basados en una muestra de 100 datos. 
#        Si se quiere ser más preciso y usar los valores críticos para el número de datos que tenga la serie a trabajar,
#        se pueden usar los valores críticos que se encuentran en la Tabla A del apendice de tablas estadística del manual suplementario del libro de Enders.
#        Generalmente, a medida que aumentan el número de datos en la serie, los valores críticos de la prueba de DF tienden a disminuir 

#---- Ejemplo práctico de uso de pruebas de ADF 1: Ejemplo de Bernhard Pfaff: logaritmo del consumo UK  ---- 

# Se trabajará con el logartimo del consumo para el Reino Unido
# La serie es trimestral (De ahí que frequency = 4)
# Nota: La serie ya ha sido desestacionalizada, de manera que no hay que hacer ajuste por estacionalidad 

data(Raotbl3) # Base de datos que se encuentra en el paquete urca
glimpse(Raotbl3)  # Visualizar la base de datos

# 1. Gráfico la serie
lc = ts(Raotbl3$lc, start = c(1966, 4), end = c(1991, 2), frequency = 4) # creación del objeto ts para trabajar con series de tiempo en R
x11()
autoplot(lc, col="blue", main = "Consumo real del Reino Unido\n(A precios de 1985)",xlab = "Fecha", ylab="", lwd=0.8) + theme_light()

# 2. Gráfico de la acf y la pacf 
lags = 24
x11()
ggAcf(lc,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF consumo real UK") + theme_light()
ggPacf(lc,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF consumo real UK")+ theme_light()

# 3. Test de Augmented Dickey Fuller seobre la serie a estudiar

# El test de la prueba de ADF se hace de manera secuencial empezando por la prueba que contiene "trend" y continuando hasta la prueba que dice "none"
# Nota: 
#   A la hora de ver cuántos rezagos diferenciados utilizar para la prueba de ADF se puede hacer de dos maneras diferentes
#     - utilizando selectlags = "AIC" (procedimiento automático donde el mismo comando selecciona el número de lags usando el criterio de información AIC)
#     - utilizando lag = # (Donde # es un número que provee los rezagos manualmente. Se recomienda utilizar el procedimiento manual dado que el proceso
#       automático algunas veces no selecciona el número correcto de lags que garantizan que los errores sean esféricos)
#  Para utilizar el procedimiento manual, es necesario primero empezar con lags = 1 y usar plot(adf.algo) para ver el comportamiento de la ACF de los residuales
#  Si la ACF muestra correlación serial en los residuales, entonces es necesario ir aumentando el número de lags (es decir lags = 2 e ir subiendo en caso de ser necesario)
#  hasta garantizar de que ya no haya correlación serial en los residuales (y garantizando así residuales esféricos que es lo que desea a la hora de hacer el test de ADF)

# Prueba de ADF con tendencia y deriva (trend & drift)
adf.trend = ur.df(lc, type = "trend", lags = 3); x11(); plot(adf.trend)
summary(adf.trend)

adf.drift = ur.df(lc, type = "drift", lags = 3); x11(); plot(adf.drift)
summary(adf.drift)

adf.none = ur.df(lc, type = "none", lags = 3); x11() ; plot(adf.none)
summary(adf.none)

# El procedimiento secuencial indica que la serie no debería incluirsele ni drift ni trend,y que la serie no es estacionaria 

# Nota: Se usa la prueba KPSS para corroborar la conclusión anterior 
summary(ur.kpss(lc)) # Rechazo la hipótesis nula de estacionaridad, lo que indica que la serie no es estacionaria

# 4. Diferencio la serie y analizo si la seriees estacionaria 

# Gráfico de la serie diferenciada
diff.lc = diff(lc)
x11()
autoplot(diff.lc, col="red", main = "tasa de crecimiento del consumo real del Reino Unido\n(A precios de 1985)",xlab = "Fecha", ylab="", lwd=0.8) + theme_light()

# ACF y PACF de la serie diferenciada 
lags = 24
x11()
ggAcf(diff.lc,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF tasa de crecimiento consumo real UK") + theme_light()
ggPacf(diff.lc,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF tasa de crecimiento consumo real UK")+ theme_light()

# ADF aplicada a la serie diferenciada 
adf.diff.none = ur.df(diff.lc, type = "none", lags = 1); x11() ; plot(adf.diff.none)
# Claramente, rechazo la hipótesis nula de no estacionariedad, de modo que la serie diferenciada sí es estacionaria
summary(adf.diff.none) 

# Ahora corroboro el resultado anterior con la prueba de KPSS

# Claramente, no rechazo la hipótesis nula de estacionaridad, y por ende trabajo con la serie como si fuera estacionaria
summary(ur.kpss(diff.lc)) 

# Conclusión: La serie diferenciada es estacionaria por lo que podría aplicar la metodología de Box Jenkins
#             Sobre la serie diferenciada

# Nota: Una serie no estacionaria cuya primera diferencia es estacionaria se dice que 
#       es una serie integrada de orden 1 (i.e. I(1))

#---- Ejemplo práctico de uso de pruebas de ADF 2: Ejemplo de Enders: GDP real USA  ---- 

# Se trabajara con el PIB real de los Estados Unidos

gdp <- read_excel(file.choose())
glimpse(gdp)

# 1. Gráfico la serie
real_gdp = ts(gdp$RGDP,start = c(1957,1), end = c(2007,4), frequency = 4)
x11()
autoplot(real_gdp, col="green", main = "PIB Real USA", xlab = "Fecha", ylab="", lwd=0.8) + theme_light()

# 2. Gráfico de la acf y la pacf 
lags = 24
x11()
ggAcf(real_gdp,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA") + theme_light()
ggPacf(real_gdp,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")+ theme_light()
  
# 3. Test de Augmented Dickey Fuller sobre la serie a estudiar
# Prueba sencuencial 

adf.trend = ur.df(real_gdp, type = "trend", lags = 1); x11(); plot(adf.trend)
summary(adf.trend)

adf.drift = ur.df(real_gdp, type = "drift", lags = 1); x11(); plot(adf.drift)
summary(adf.drift)

# El procedimiento secuencial indica que la serie no debería incluir trend lineal pero sí debería drift,
# y que la serie no es estacionaria 

# 4. Diferencio la serie y aplico el logaritmo y analizo si la serie es estacionaria 

# Gráfico de la serie diferenciada
dl.real_gdp = diff(log(real_gdp))
x11()
autoplot(dl.real_gdp, col="red", main = "tasa de crecimiento del PIB real USA",xlab = "Fecha", ylab="", lwd=0.8) + theme_light()

# ACF y PACF de la serie diferenciada 
lags = 24
x11()
ggAcf(dl.real_gdp,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF tasa de crecimiento PIB real USA") + theme_light()
ggPacf(dl.real_gdp,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF tasa de crecimiento PIB real USA")+ theme_light() 

# ADF aplicada a la serie diferenciada
adf.diff.drift = ur.df(dl.real_gdp, type = "drift", lags = 1); x11() ; plot(adf.diff.drift)

# Claramente, rechazo la hipótesis nula de no estacionariedad, por lo cual la serie diferenciada sí es estacionaria.
# De igual forma, por el estadístico phi1 es factible que la serie diferenciada tenga intercepto
summary(adf.diff.drift) 
