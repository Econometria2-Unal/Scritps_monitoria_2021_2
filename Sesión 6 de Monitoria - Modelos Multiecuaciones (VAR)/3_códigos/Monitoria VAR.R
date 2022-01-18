##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2022-I
#            SESIÓN MONITORIA : Modelos de Vectores Autorregresivos
##____________________________________________________________________________________
##____________________________________________________________________________________

#Cargamos los paquetes que vamos a utilizar

library(vars)
library(urca)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(dplyr)
library(tidyr)
library(readxl)
 
#---- Ejemplo N.1: Serie simulada ----

#Vamos a simular un modelo VAR(1) en dos variables. En consecuencia, la matriz A_1 será de dimensiones 2x2 y tendrá
#como componentes: a11=0.3; a12=0.2; a21=0.5; a22=0.6. En este ejemplo asumimos que conocemos el verdadero PGD, aunque
#en la vida real esto no es posible. 

#Fijamos la semilla para que siempre dé el mismo resultado
set.seed(82901) 

#Determinamos un tamaño de muestra de 300 observaciones
T = 300 

#y_t es una matriz de 2 variables (una variable por columna)
y_t <- cbind(rep(0, T),rep(0, T))

#En tanto hay dos variables, habrá 2 residuales. Supondrémos que se distribuyen normal.
u_t = cbind(rnorm(T), rnorm(T))

#Definimos la matriz de coeficientes autorregresivos.
A_1 = cbind(c(0.3, 0.5), c(0.2, 0.6))

#Función que permite simular un VAR(1) (simula el proceso generador de datos)
sim = function(y_t, A_1, u_t, T){
  for (i in 2:T) {
    y_t[i,] = A_1 %*% y_t[i-1,] + u_t[i,] # está llenando el vector de variable por filas (1 fila a la vez)
  }  
  return(y_t)
}

# La simulación lo que busca es modelar las variables a partir de la fórmula del VAR en forma
# reducida
y_t = sim(y_t, A_1, u_t, T) # La función sim lo que busca es llenar la matriz de ceros y_t con valores

#Convertimos la serie en un objeto ts
y_t=ts(y_t, start=c(1900,1), frequency=4)

#Vamos a graficar las series
y1=autoplot(y_t[,1], size=2,ts.colour="red", xlab="",ylab="", main="Variable y_1")
y2=autoplot(y_t[,2], size=2,ts.color="blue", xlab="",ylab="", main="Variable y_2")

x11()
grid.arrange(y1,y2,ncol=2)

#Recuerden que los modelos VAR requieren de series estacionarias. Por lo tanto, haremos dos pruebas de raíz unitaria
adf1= ur.df(y_t[,1], lags=3,selectlags = "AIC",type="none");plot(adf1);summary(adf1) #Rechazo H0, la serie es I(0)
adf2= ur.df(y_t[,2], lags=3,selectlags = "AIC",type="none");plot(adf2);summary(adf2) #Rechazo H0, la serie es I(0)

##
# Metodología Box Jenkins para modelos multiecuacionales serie simulada

#---- Primera etapa: Identificación del modelo VAR serie simulada ----

#Una vez hecho esto, vamos a seleccionar el número de rezagos del modelo VAR y la inclusión de términos determinísticos.
#La función VARselect() me indica a partir de 4 criterios de información diferentes, cuál es el modelo VAR más apropiado.

#Selección de rezagos para un VAR con tendencia e intercepto.
VARselect(y_t, lag.max=6,type = "both", season = NULL)#Todos seleccionan un rezago, como en el PGD.

#Selección de rezagos para un VAR con sólo intercepto.
VARselect(y_t, lag.max=6,type = "const", season = NULL)#Todos seleccionan un rezago, como en el PGD.

#Selección de rezagos para un VAR sin términos determinísticos.
VARselect(y_t, lag.max=6,type = "none", season = NULL)#Todos seleccionan un rezago, como en el PGD.

#---- Segunda etapa: Estimación serie simulada ----

#Para elegir cuál modelo estimar, vamos a fijarnos en las gráficas de las series (ninguna aparenta tener tendencia), y 
#de la significancia estadística de los términos determinísticos en cada una de las ecuaciones del modelo VAR.

#VAR con tendencia e intercepto
V.tr = VAR(y_t, p=1, type="both", season=NULL)
summary(V.tr) #La tendencia no es significativa en ningún modelo.Analizemos con drift.

#VAR con sólo intercepto.
V.dr= VAR(y_t, p=1, type="const", season=NULL) 
summary(V.dr) #El intercepto es significativo en una ecuación.

#VAR sin términos determinísticos.
V.no = VAR(y_t, p=1, type="none", season=NULL)  
summary(V.no)

#Elegimos el modelo sin términos determinísticos porque: i)las variables simuladas tienen media cero, y por ende, sería
#apropaido que el VAR también; y, ii) los interceptos en el modelo VAR con constante no son significativas. 


#Ahora analizaremos si el proceso es estable: en los modelos VAR se debe cumplir que todas las raíces del polinomio
#característico estén por fuera del círculo unitario. En esta caso, roots() representa el inverso de dicha raíces, 
#de manera que para que el proceso sea estable debe tener módulos inferiores a 1. 
roots(V.no) #El proceso es estable.

#Ahora analizamos cada uno de los coeficientes estimados. 

#Coeficientes:
Acoef(V.no) #Los valores teóricos eran: a11=0.3; a12=0.2; a21=0.5; a22=0.6. Las estimaciones son cercanas.

#Matriz de varianzas y covarianzas de los residuales
Sigma.est = summary(V.no)$covres #Esta es la matriz (Sigma). Si se hace una descomposición de Choleski recordar que: Sigma = P %*% t(P), donde P es una matriz triangular inferior
Sigma.est #Individualmente cada residual debe ser un ruido blanco. Sin embargo, hay correlación contemporánea entre ellos. (Son residuales que corresponden a la estimación de un VAR en forma reducida)

#---- Tercera etapa: Validación de supuestos serie simulada  ----

## Autocorrelación: PT.asymptotic es para muestra grande y "PT.adjusted" es corrección para muestra pequeña.

P.75=serial.test(V.no, lags.pt = 75, type = "PT.asymptotic");P.75 #No rechazo, se cumple el supuesto
P.30=serial.test(V.no, lags.pt = 30, type = "PT.asymptotic");P.30 #No rechazo, se cumple el supuesto
P.20=serial.test(V.no, lags.pt = 20, type = "PT.asymptotic");P.20  #No rechazo, se cumple el supuesto
P.10=serial.test(V.no, lags.pt = 10, type = "PT.asymptotic");P.10 #No rechazo, se cumple el supuesto

#Graficamos los residuales para 20 pasos_adelantes: se grafican los residuales, su distribución, la ACF y PACF de los residuales y
#la ACF y PACF de los residuales al cuadrado (proxy para heterocedasticidad)
x11()
plot(P.20, names = "Series.1") #Los residuales de la primera serie se comportan bien
plot(P.20, names = "Series.2") #Los residuales de la segunda serie se comportan bien

#Homocedasticidad: Test tipo ARCH multivariado
arch.test(V.no, lags.multi = 24, multivariate.only = TRUE) #No rechazo, se cumple el supuesto.
arch.test(V.no, lags.multi = 12, multivariate.only = TRUE) #No rechazo, se cumple el supuesto

##Test Jarque-Bera multivariado
normality.test(V.no) #No rechazo, se cumple el supuesto. 

#---- Cuarta etapa: Uso del modelo serie simulada ----

#Algunos usos del modelo son: Pronósticos, Análisis de impulso respuesta, Descomposición de varianza, causalidad de Granger
#e identificación de un SVAR. Aquí enfatizaremos únicamente en los primeros tres. 


#Pronóstico
x11()
predict(V.no, n.ahead = 12,ci=0.95) 
autoplot(predict(V.no, n.ahead = 12,ci=0.95)) #Como el proceso tiene media cero, el pronóstico converge a la media.

#Añadimos un fanchart:#Regiones de probabilidad para el pronóstico: las zonas más oscuras son más probables
fanchart(predict(V.no, n.ahead = 12))


#Análisis Impulso-Respuesta: ¿Qué efecto tiene un choque exógeno sobre una variable sobre las variables del sistema?
#todos los choques exógenos se incorporan/capturan en los residuales. 
#Se parte del supuesto de Ceteris Paribus: los choques de una variable no deben correlacionarse con el de otra.

#Definimos el número pasos adelante
pasos_adelantes = 0:18

# Función que me permite calcular y graficar las funciones impulso respuesta (A cada función impulso respuesta le asigno una gráfica)
impulso_respuesta = function(var, impulso, respuesta, pasos_adelantes, ortog, int_conf, titulo){
  # Cáclulo de la función impulso respuesta
  total_pasos_futuros = length(pasos_adelantes) - 1
  IRF = irf(var, impulse=impulso, response=respuesta, n.ahead = total_pasos_futuros, ortho=ortog, ci = int_conf)
  IRF_data_frame = data.frame(IRF$irf,IRF$Lower,IRF$Upper, pasos_adelantes)
  # Gráfica de la función impulso respuesta
  graph = IRF_data_frame %>% 
    ggplot(aes(x=IRF_data_frame[,4], y=IRF_data_frame[,1], ymin=IRF_data_frame[,2], ymax=IRF_data_frame[,3] )) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(titulo)+
    ylab("")+
    xlab("pasos adelante") +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))    
  return(graph)
}

#IRF de las variables del sistema ante distintos choques exógenos.
y1.y1 = impulso_respuesta(V.no, "Series.1", "Series.1", pasos_adelantes, ortog = F, int_conf = 0.95, titulo = "Impulso de y1 - respuesta de y1")
y1.y2 = impulso_respuesta(V.no, "Series.1", "Series.2", pasos_adelantes, ortog = F, int_conf = 0.95, titulo = "Impulso de y1 - respuesta de y2")
y2.y1 = impulso_respuesta(V.no, "Series.2", "Series.1", pasos_adelantes, ortog = F, int_conf = 0.95, titulo = "Impulso de y2 - respuesta de y1")
y2.y2 = impulso_respuesta(V.no, "Series.2", "Series.2", pasos_adelantes, ortog = F, int_conf = 0.95, titulo = "Impulso de y2 - respuesta de y2")

x11()
grid.arrange(y1.y1,y1.y2,y2.y1,y2.y2,ncol=2)

#¿Qué son las respuestas ortogonalizadas? Como les mencionamos anteriormente, u_1 y u_2 (aunque individualmente son ruido blanco) están
#correlacionados de forma contemporánea (en el periodo t), de manera que los choques entre las variables posiblemente también estén correlacionados. 
#Por lo tanto, las funciones impulso-respuesta ortogonales hacen una transformación de la matriz de varianzas y covarianzas de los residuales, de
#forma que ya no estén correlacionados (se hace a partir de la descomposición de choleski), y por ende, los choques entre las variables tampoco. 
#La interpretación es exactamente la misma, sin embargo, el ordenamiento de las variables importa: la primera variable afecta contemporáneamente a todas las
#variables del sistema, mientras la segunda variable afecta contemporáneamente a todas las variables menos a la primera, la tercera afecta a todas menos
#a la primera y la segunda variable,etc...

# Para el calculo de las funciones impuslo respuesta ortogonalizadas lo único que hay que hacer es cambiar el parámetro ortog=F a ortog=T
y1.y1. = impulso_respuesta(V.no, "Series.1", "Series.1", pasos_adelantes, ortog = T, int_conf = 0.95, titulo = "Impulso ortogonal de y1 - respuesta de y1")
y1.y2. = impulso_respuesta(V.no, "Series.1", "Series.2", pasos_adelantes, ortog = T, int_conf = 0.95, titulo = "Impulso ortogonal de y1 - respuesta de y2")
y2.y1. = impulso_respuesta(V.no, "Series.2", "Series.1", pasos_adelantes, ortog = T, int_conf = 0.95, titulo = "Impulso ortogonal de y2 - respuesta de y1")
y2.y2. = impulso_respuesta(V.no, "Series.2", "Series.2", pasos_adelantes, ortog = T, int_conf = 0.95, titulo = "Impulso ortogonal de y2 - respuesta de y2")

x11()
grid.arrange(y1.y1.,y1.y2.,y2.y1.,y2.y2.,ncol=2)


##Descomposición de varianza del error de pronóstico: ¿Qué proporción de la varianza del error de pronóstico de y1 es explicada por y1 y y2?
##¿Qué proporción de la varianza del error de pronóstico de y2 es explicada por y1 y y2?

x11()
fevd(V.no, n.ahead = 18)
plot(fevd(V.no, n.ahead = 18),col=c("red", "green"))
 
##Representación VMA del proceso VAR(1) modelado (10 pasos adelante). De esta representación se 
## obtienen las funciones impulso-respuesta al hacer las derivadas parciales respectivas
Phi(V.no,nstep=10) #Esta función nos calcula la matriz de coeficientes n pasos adelante

##La función psi nos calcula los coeficientes de las matrices de la representación VMA bajo una
##estrategia de identificación ortogonal. Es decir, son los coeficientes de las IRF ortogonales n pasos adelante
Psi(V.no,nstep=10)  



#---- EJEMPLO 2 ----

#Tenemos series de frecuencia trimestral desde 1960 Q1 - 2012 Q4 para el Índice de Producción Industrial, 
#El índice de precios al consumidor y la tasa de desempleo de Estados Unidos

#Partamos de este ejercicios de Enders: 
#in order to estimate the dynamic effects of aggregate demand and supply shocks on industrial 
#production and the inflation rate. Create the logarithmic change in the index of industrial 
#production (indprod) as Δlipt = ln(indprod) − ln(indprodt−1) and the inflation rate 
#(as measured by  CPI) as inft = log(cpit) − log(cpit−1)
                                                          
#Sin embargo, le incorporaremos la tasa de desempleo para tener un sistema en tres variables. 


#Definimos la base de datos y las variables en nivel
Base <- read_excel(file.choose())
attach(Base)
            
IPI = ts(IPI, start=c(1960,1),frequency=4)
CPI= ts(CPI, start=c(1960,1),frequency=4)
UNEM = ts(Unem, start=c(1960,1),frequency=4)

#Ahora definimos la tasa de inflación y la tasa de crecimiento del IPI

dl.IPI = diff(log(IPI))
Infl = diff(log(CPI))

#Graficamos las series:
x11()
autoplot(dl.IPI, size=2, ts.colour="red", xlab="Año", ylab="Porcentaje", main="Tasa de crecimeinto IPI")
autoplot(Infl, size=2, ts.colour="green", xlab="Año", ylab="Porcentaje", main="Tasa de inflación")
autoplot(UNEM, size=2, ts.colour="blue", xlab="Año", ylab="Porcentaje", main="Tasa de desempleo")

#Ahora hacemos las pruebas de raíz unitaria

#Tasa de crecimiento IPI
adf3= ur.df(dl.IPI, lags=6, selectlags="AIC", type="drift"); summary(adf3);plot(adf3) #La serie es estacionaria y con intercepto
 
#Tasa de inflación
adf4= ur.df(Infl, lags=6, selectlags="AIC", type="drift"); summary(adf4);plot(adf4)#La serie es estacionaria y con intercepto.

#Tasa de desempleo
adf5= ur.df(UNEM, lags=6, selectlags="AIC", type="drift");summary(adf5);plot(adf5)  #La serie es estacionaria y con intercepto.


#Como todas las variables son estacionarias de acuerdo al test ADF (recomendamos analizar la KPSS), procedemos 
#a determinar el número de rezagos y los términos determinísticos en el modelo VAR

#Definimos una matriz con las variables, con el siguiente ordenamiento
Unem = UNEM[1:211]
Y = cbind(dl.IPI,Unem,Infl)

#---- Primera etapa: Identificación del modelo VAR ejemplo con series reales ----

#Selección de rezagos para un VAR con sólo intercepto.
VARselect(Y, lag.max=6,type = "const", season = NULL)#Elegimos 3 rezagos

#Selección de rezagos para un VAR sin términos determinísticos.
VARselect(Y, lag.max=6,type = "none", season = NULL)#Elegimos 3 rezagos


#---- Segunda etapa: Estimación modelo VAR ejemplo con series reales ----

#Para elegir cuál modelo estimar, vamos a fijarnos en las gráficas de las series (ninguna aparenta tener tendencia), y 
#de la significancia estadística de los términos determinísticos en cada una de las ecuaciones del modelo VAR.

 
#VAR con sólo intercepto.
V.dr.1= VAR(Y, p=3, type="const", season=NULL) 
summary(V.dr.1) #El intercepto es significativo en una ecuación.

#VAR sin términos determinísticos.
V.no.1 = VAR(Y, p=3, type="none", season=NULL)  
summary(V.no.1)

#Elegimos el VAR con constante con objeto de que el proceso no tenga media cero, debido a que no todas las series parecen
#tener dicha característica. Asimismo, porque la constante es significativa en al menos una de las 3 ecuaciones. 

#Ahora analizaremos si el proceso es estable: en los modelos VAR se debe cumplir que todas las raíces del polinomio
#característico estén por fuera del círculo unitario. En esta caso, rooots() representa el inverso de dicha raíces, 
#de manera que para que el proceso sea estable debe tener módulos inferiores a 1. 
roots(V.dr.1) #El proceso es estable.

#Ahora analizamos cada uno de los coeficientes estimados. 

#Coeficientes:
Acoef(V.dr.1) #Presenta los resultados de la matriz A_1, A_2 y A_3

#Matriz de varianzas y covarianzas de los residuales
Sigma.e = summary(V.dr.1)$covres #Esta es la matriz (Sigma). Si se hace una descomposición de Choleski recordar que: Sigma = P %*% t(P), donde P es una matriz triangular inferior
Sigma.e #Individualmente cada residual debe ser un ruido blanco. Sin embargo, hay correlación contemporánea entre ellos. (Son residuales que corresponden a la estiación de un VAR en forma reducida)

#---- Tercera etapa: Validación de supuestos modelo VAR ejemplo con series reales ----

# Ahora vamos a realizar la validación de supuestos. 

## Autocorrelación: PT.asymptotic es para muestra grande y "PT.adjusted" es corrección para muestra pequeña.
## Si se llegase a rechazar el supuesto habría que incluir más rezagos

P.75.1=serial.test(V.dr.1, lags.pt = 50, type = "PT.asymptotic");P.75.1 #No rechazo, se cumple el supuesto
P.30.1=serial.test(V.dr.1, lags.pt = 30, type = "PT.asymptotic");P.30.1 #No rechazo, se cumple el supuesto
P.20.1=serial.test(V.dr.1, lags.pt = 20, type = "PT.asymptotic");P.20.1  #No rechazo, se cumple el supuesto
 
#Graficamos los residuales para 20 pasos_adelantes: se grafican los residuales, su distribución, la ACF y PACF de los residuales y
#la ACF y PACF de los residuales al cuadrado (proxy para heterocedasticidad)
x11()
plot(P.20.1, names = "dl.IPI") #Relativamente Bien comportados, salvo por normalidad
plot(P.20.1, names = "Unem") #Relativamente Bien comportados, salvo por normalidad.
plot(P.20.1, names= "Infl") #Relativamente Bien comportados, salvo por normalidad.

#Homocedasticidad: Test tipo ARCH multivariado
arch.test(V.dr.1, lags.multi = 24, multivariate.only = TRUE) #rechazo H0, no se cumple el supuesto.
arch.test(V.dr.1, lags.multi = 12, multivariate.only = TRUE) #rechazo Ho, no se cumple el supuesto

##Test Jarque-Bera multivariado
normality.test(V.dr.1) #rechazo, no se cumple el supuesto. 

#---- Cuarta etapa: Uso del modelo VAR ejemplo con series reales ----

####
# Para poder hacer inferencia en las IRF, pronósticos, etc, tenemos que usar Bootstrapping: son
# simulaciones de monte carlo que permiten calcular unos intervalos de confianza asintóticos a 
# partir de los residuales de las regresiones. 
####

# El pronóstico es insesgado, pero no se pueden computar los intervalos de confianza de la manera
# convencional porque los residuales no son normales, por ende hay que hacer bootstrapping
 
x11()
predict(V.dr.1, n.ahead = 12) 
autoplot(predict(V.dr.1, n.ahead = 12)) 
 
#Necesitamos estimar los intervalos de confianza por Bootstrapping
#install.packages("VAR.etp"); 
library(VAR.etp)
For.Boot= VAR.BPR(Y, 3, 12, nboot = 1000, type = "const", alpha = 0.95)
For.Boot

help(VAR.etp)

#Análisis Impulso-Respuesta: ¿Qué efecto tiene un choque exógeno sobre una variable sobre las variables del sistema?
#todos los choques exógenos se incorporan/capturan en los residuales. 
#Se parte del supuesto de Ceteris Paribus: los choques de una variable no deben correlacionarse con el de otra.

#Como en economía los choques están generalmente correlacionados, usaremos la descomposición de Choleski para obtener 
#las IRF ortogonales.

#¿Qué son las respuestas ortogonalizadas? Como les mencionamos anteriormente, u_1 y u_2 (aunque individualmente son ruido blanco) están
#correlacionados de forma contemporánea (en el periodo t), de manera que los choques entre las variables posiblemente también estén correlacionados. 
#Por lo tanto, las funciones impulso-respuesta ortogonales hacen una transformación de la matriz de varianzas y covarianzas de los residuales, de
#forma que ya no estén correlacionados (se hace a partir de la descomposición de choleski), y por ende, los choques entre las variables tampoco. 
#La interpretación es exactamente la misma, sin embargo, el ordenamiento de las variables importa: la primera variable afecta contemporáneamente a todas las
#variables del sistema, mientras la segunda variable afecta contemporáneamente a todas las variables menos a la primera, la tercera afecta a todas menos
#a la primera y la segunda variable,etc...

#En este caso: el crecimiento del índice de producción industrial afecta contemporáneamente a la tasa desempleo y a la inflación. 
#La tasa de desempleo afecta de forma contemporánea a la inflación pero no al crecimiento del IPI
#La tasa de inflación no afecta de forma contemporánea ni al crecimeinto del IPI ni al desempleo.

#Definimos el número pasos adelante
pasos_adelantes = 0:24

# IRF de las variables ante distintos choques ortogonales
## impuslso_respuesta: 1. calcula las IRF y 2. graficarlas
Y1.Y1. = impulso_respuesta(V.dr.1, "dl.IPI",
                           "dl.IPI", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. del crecimiento del IPI - res. del crecimiento del IPI")
Y1.Y2. = impulso_respuesta(V.dr.1, "dl.IPI",
                           "Unem", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. del crecimiento del IPI - resp. del desempleo")
Y1.Y3. = impulso_respuesta(V.dr.1, "dl.IPI",
                           "Infl", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. del crecimiento del IPI - resp. de la inflación")
Y2.Y1. = impulso_respuesta(V.dr.1, "Unem",
                           "dl.IPI", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. tasa de desempleo - resp. del crecimiento del IPI")
Y2.Y2. = impulso_respuesta(V.dr.1, "Unem",
                           "Unem", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de desempleo - resp. tasa de desempleo")
Y2.Y3. = impulso_respuesta(V.dr.1, "Unem",
                           "Infl", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de desempleo - resp. de la inflación")
Y3.Y1. = impulso_respuesta(V.dr.1, "Infl",
                           "dl.IPI", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de inflación - resp. del crecimiento del IPI")
Y3.Y2. = impulso_respuesta(V.dr.1, "Infl",
                           "Unem", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de inflación - resp. del desempleo")
Y3.Y3. = impulso_respuesta(V.dr.1, "Infl",
                           "Infl", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de inflación - resp. de la inflación")

#Las IRF ORTOGONALES son:
x11()
grid.arrange(Y1.Y1., Y1.Y2., Y1.Y3., Y2.Y1., Y2.Y2., Y2.Y3., Y3.Y1., Y3.Y2., Y3.Y3., ncol=3)


##Descomposición de varianza del error de pronóstico: 
#¿Qué proporción de la varianza del error de pronóstico del crecimiento del IPI se explica por el crecimiento del IPI, la tasa de desempleo y la inflación?
#¿Qué proporción de la varianza del error de pronóstico de la tasa de desempleo se explica por el crecimiento del IPI,la tasa de desempleo y la inflación?
#¿Qué proporción de la varianza del error de pronóstico de la tasa de inflación se explica por el crecimiento del IPI,la tasa de desempleo y la inflación?

x11()
fevd(V.dr.1, n.ahead = 24)
plot(fevd(V.dr.1, n.ahead = 24),col=c("red", "green", "yellow"))


##Representación VMA del proceso VAR(1) modelado (10 pasos adelante). De esta representación se 
## obtienen las funciones impulso-respuesta al hacer las derivadas parciales respectivas
Phi(V.dr.1,nstep=10) #Esta función nos calcula la matriz de coeficientes n pasos adelante

##La función psi nos calcula los coeficientes de las matrices de la representación VMA bajo una
##estrategia de identificación ortogonal. Es decir, son los coeficientes de las IRF ortogonales n pasos adelante
Psi(V.dr.1,nstep=10)  

 