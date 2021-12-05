##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2021-I
#            SESIÓN MONITORIA : Cointegración y Metodología de Johansen 
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
library(tsDyn)
library(VAR.etp)

# Aspectos generales de la metodología de Johansen ----

# Consiste en un procedimiento en 4 etapas: 
## Etapa 1: Verificación preliminar de las variables a trabjar (Orden de integración y gráficas)
##          e Identificación del número de rezagos del VECM mediante criterios de información sobre el VAR en niveles 
## Etapa 2: Determinación del rango de la matriz Pi (es decir del número de relaciones de cointegración) 
##          y estimación del VECM
## Etapa 3: Análisis de la matriz beta (matriz que contiene el vector de cointegración) 
##          y matriz alpha (matriz que contiene los parámetros de velocidad de ajuste)
## Etapa 4: Validación de supuestos y usos del modelo 

# Nota: Sobre bibliografía adicional
## Se recomienda mucho leer los capítulos 6.7, 6.8 y 6.9 del Enders que explican en detalle la metodología empleada acá
## Se recomienda leer capítulo 8 para entender las fucniones empleadas acá
## Para mayor profundidad sobre el tema y aprender más sobre las valores característicos y más detalles 
## técnicos de la metodología de Johansen, se recomienda leer el apéndice 6.1 del manual suplementario del Enders

# Ejemplo N.1 Precio de referencia Brent y WTI ----

#Vamos a utilizar una serie del precio spot del petróleo de referencia Brent y una serie del precio spot
#del petróleo de referencia WTI. Las series tienen frecuencia mensual y comprenden el 
#periodo de Enero del 2000 a Diciembre de 2020.

#Vamos a cargar las series
Data <- read_excel(file.choose())
attach(Data)

#Creamos los objetos ts
P.Brent <- ts(Brent, start=c(2000,1), frequency=12)
P.WTI <- ts(WTI,start=c(2000,1), frequency=12)

# 1. Etapa 1: Determinación de orden de integración de las series e identificación del número de rezagos ----

# Gráficas de las series(Se puede ver un movimiento conjunto por lo que parece haber una relación de cointegración entre las dos variables)
x11()
autoplot(cbind(P.Brent, P.WTI), facets = F, main="Precios spot del petróleo", xlab="", ylab="", size=2)

# 1.1 Identificación del orden de integración de las series ----

# Procedemos a hacer las pruebas de raíz unitaria (Para identificar el orden de integración de las dos series)

# Nota: Es importante tener en cuenta que toda la metodología que vamos a emplear es válida si las series son I(1)
#       Para identificar el orden de integración de la series se van a utilizar pruebas de raíz unitaria ADF 

# Series en niveles

##Referencia Brent 
summary(ur.df(P.Brent, lags=6, selectlags="AIC", type="drift"));  #No tiene drift
summary(ur.df(P.Brent, lags=6, selectlags="AIC", type="none")) #La serie es no estacionaria

##Referencia WTI
summary(ur.df(P.WTI, lags=6, selectlags="AIC", type="drift")) #No tiene drift
summary(ur.df(P.WTI, lags=6, selectlags="AIC", type="none")) #La serie es no estacionaria

# Series diferenciadas

##Referencia Brent 
summary(ur.df(diff(P.Brent), lags=6, selectlags="AIC", type="none")) #La serie difereniada es estacionaria

##Referencia WTI
summary(ur.df(diff(P.WTI), lags=6, selectlags="AIC", type="none")) #La serie diferenciada es estacionaria

# Por lo anterior, las dos series son I(1) y se puede emplear el procedimiento de cointegración de Johansen

# 1.1 Posteriormente, estimaremos un VAR en niveles para determinar el número rezagos del VECM ----

# Ojo: Se analizaran los criterios de información sobre el VAR en niveles 

# Se construir la matriz con las series de tiempo que se consideran cointegradas
# Recuerden que el orden importa que se coloca las variables importa cuando se analizan 
# las funciones impulso respuesta ortogonalizads
Y = cbind(P.Brent, P.WTI) 
VARselect(Y, lag.max = 6, type="both", season=NULL) #2 y 3 rezagos, elegimos el más parsimonioso
VARselect(Y, lag.max = 6, type="const", season=NULL) #2 rezagos y 3 rezagos, elegimos el más parsimonioso
VARselect(Y, lag.max = 6, type="none", season=NULL) #2 rezagos y 4 rezagos, elegimos el más parsimonioso

#Estimamos cada uno de los modelos y determinamos cuál es el más apropiado
summary(VAR(Y, p=2, type="both", season=NULL)) #La tendencia no es significativa en ninguna ecuación
summary(VAR(Y, p=2, type="const", season=NULL)) #La constante es significativa en ambas ecuaciones

#Elegiremos el modelo con intercepto debido a que es significativo en ambas ecuaciones y ninguna de las series tiene media cero.
VAR2 <- VAR(Y, p=2, type="const", season=NULL)

#Vamos a analizar el comportamiento de los residuales, si no se comportan bien incluiremos más rezagos
P.75=serial.test(VAR2, lags.pt = 70, type = "PT.asymptotic");P.75 #No rechazo, se cumple el supuesto
P.30=serial.test(VAR2, lags.pt = 30, type = "PT.asymptotic");P.30 #No rechazo, se cumple el supuesto
P.20=serial.test(VAR2, lags.pt = 20, type = "PT.asymptotic");P.20  #No rechazo, se cumple el supuesto
P.10=serial.test(VAR2, lags.pt = 10, type = "PT.asymptotic");P.10 #No rechazo, se cumple el supuesto

#Graficamos los residuales para 20 lags: se grafican los residuales, su distribución, la ACF y PACF de los residuales y
#la ACF y PACF de los residuales al cuadrado (proxy para heterocedasticidad)
x11()
plot(P.20, names = "P.Brent") #Bien comportados, salvo por los residuales al cuadrado
plot(P.20, names = "P.WTI") #Bien comportados, salvo por los residuales al cuadrado

# 2. Etapa 2: Determinación del rango de la matriz Pi y estimación del modelo ----

# Información acerca de la metodología de Johansen para cointegración ----

#Ahora, determinando que el número de rezagos del VAR en niveles es p=2, vamos a aplicar la prueba de cointegración de Johansen. Este prueba analiza
#el rango de la matriz que acompaña al término de corrección de error (x_t-1) en la ecuación del modelo VECM. Si r=0, se determina que no existe 
#cointegración entre las series por lo que debe estimarse un VAR (p-1) en diferencias. Si r < g (siendo g el número de variables), se dice que la matriz tiene rango  
#reducido y r representará las relaciones de cointegración, en este caso se estima un VECM(p-1). Finalmente, si r=g decimos que la matriz tiene rango completo, 
#de manera que debe estimarse un VAR(p) en niveles.

#Con objeto de que el vector de cointegración sea únicamente identificado, debe normalizarse respecto alguna variable. Esto permite descomponer
#la matriz PI = alpha*Beta'. Donde la matriz Beta contiene el vector de cointegración normalizado y alpha contiene los coeficientes de ajuste a la relación 
#de cointegración, ante una desviación de corto plazo en la misma. 

#Por último, se pueden hacer diferentes pruebas sobre restricciones en los parámetros, lo cual es importante para contrastar los resultados 
#empíricos con los postulados teóricos. Dichas pruebas se pueden realizar tanto en la matriz alpha que contiene los coeficientes de ajuste como en la 
#matriz beta que contiene el vector de cointegración

# Especificaciones de la prueba de Johansen (ca.jo) ----

# El software maneja 3 especificaciones distintas para la prueba de Johansen (ecdet): 
## none: No intercerpto en el vector de cointegración lo que implica drift (deriva). 
##       Es decir, A0 es distinto a cero pero el vector de cointegración no tiene constante. 
##       Lo anterior, es equivalente a estimar un modelo con tendencia lineal
##       En este caso las series no solo tienen tendencia estocástica común sino también 
##       tendencia determínisca común
##       Equivalente al primer sistema de eqs. que aparece en la pag. 376 del Enders
##       Matrialmente a la ecuación 6.51 pag. 374 Enders

## const: Para incluir un término contante en el vector de cointegración
##        Es decir, A0 es cero y las constante se incluye en el vector de cointegración 
##        (en el anterior caso era lo contrario, la constante se incluye afuera del vector de cointegración)
##        En este caso las series no solo tienen tendencia estocástica común
##        (Es decir, no tiene tendencia deterministica común)
##        Equivalente al ssegundo sistema de eqs. que aparece en la pag. 376 del Enders
##        Matrialmente a la ecuación 6.52 pag. 376 Enders

## trend: Para incluir tanto intercepto como tendencia en el vector de cointegración 
##        Este caso no se encuentra explicado en el Enders

# Nota: En el Enders, se maneja un tercer caso en el que no hay ningún término determínistico ni dentro
#       del vector de cointegración ni afuera. Es decir, un modelo donde A0 = 0 y el modelo VEC tampoco
#       incluye constante en el vector de cointegración. 
#       Esta nota es importante, dado que el software no considera este caso 

# Nota: Los valores críticos para las diferentes especificaciones de la prueba de Johansen se encuentra en 
#       el manuel suplementario del Enders Tabla E pag. 126

# Nota: Existe un test de razón de verosimilitud para determinar entre una especificación con constante 
#       dentro del vector de cointegración (es decir, sin tendencia lineal -- i.e. A0=0) y con constante
#       fuera del vector de cointegración (es decir, con tendencial lineal -- i.e. A0 diferente de 0)
#       Dicho test se realizar mediante el comando lttest y lo veremos más adelante

# Notas adicionales ----

# Todo el análisis del test de Johansen se hace sobre la matriz Pi que acompaña al término de corrección de error
# El número de relaciones de cointegración depende del número de vectores propios que son diferentes de cero
# Tanto la prueba de la traza como la prueba del valor propio máximo depende de los valores propios de la 
# matriz Pi que acompaña al término de corrección de error
# El término de correción de error es el que contiene la información de largo plazo de las variables

# Argumentos del test de Johansen ----

# Para ver todos los argumentos del test de Johansen 
args("ca.jo") 

## type: Especifica el tipo de prueba. Exiten dos pruebas: eigen y trace
### eigen: prueba del valor propio máximo 
###        Se basa en la ecuación 6.56 de la pag. 378 del Enders
###        H0: r es el número de relaciones de cointegración
###        Ha: r+1 es el número de relaciones de cointegración
###        Prueba de cola derecha al distribuirse el estadístico como una chi2

### trace: prueba de la traza
###        Se basa en la ecuación 6.55 de la pag. 378 del Enders
###        H0: menor o igual a r (<=r)  número de relaciones de cointegración
###        Ha: mayor o igual a r (>=r) número de relaciones de cointegración
###        Prueba de cola derecha al distribuirse el estadístico como una chi2

# Nota: En la practica se suele emplear más a menudo la prueba del valor propio máximo al ser más precisa

# Ojo: Los dos resultados de las dos pruebas pueden ser contradictorios en el sentido que pueden dar
#      un diferente r, es decir, las dos pruebas pueden seleccionar un número distinto de relaciones de
#      cointegración. En este caso, hay que utilizar la intuición teórico para determinar cuáles podrían
#      ser, desde la teoría, las posibles relaciones de largo plazo entre las variables y de esta forma, 
#      determinar cuál de las dos pruebas posiblemente está determinando el número correcto de relaciones
#      de cointegración

## ecdet: Especifica el tipo de término determinístico que se le va a incluir a la prueba
### none: No intercerpto en el vector de cointegración lo que implica drift (deriva).
### const: Para incluir un término contante en el vector de cointegración
### trend: Para incluir tanto intercepto como tendencia en el vector de cointegración 

## K: Se coloca el número de rezagos que se determinaron para el VAR en niveles cuando 
##    se utilizó los criterios de información para determinarlos (en este caso K=2)

## spec: Especifica que tipo de VECM se empleará
### longrun: Estima el modelo que se encuentra en la ecuación 4.8a en la pag. 79 del libro de Pfaff
### transitory: Estima el modelo que se encuentra en la ecuación 4.9a en la pag. 80 del libro de Pfaff
###             Ésta, es la misma especificación que usa la profe en las diapositivas 
###             (Se sugiera usar esta especificación)

## season: Se empleo en caso de que haya estacionalidad en las series. Se utiliza para modelar la 
##         estacionalidad y lo que hace es agregar dummys que tienen en cuenta dicho componente estacional

## dumvar: Se emplea en caso de que se quieran agregar regresores exógenos al modelo
##         Es decir, variables que no son endógenas al modelo y por tanto que no se incluyen
##         dentro del vector x_t de variables endógenas del VECM

# Nota muy importante: Los valores críticos de la prueba arrojados por el software en algunas especificaciones
#                      difieren un poco de los valores que arroja la tabla E pag. 378
#                      del manual suplementario del Enders  

# 2.1 Test de Johansen - Con drift fuera del vector de cointegración (Es decir, modelo con tendencia lineal) ----

#Criterio del valor propio máximo (generalmente es la prueba preferida). Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r=1, luego H0: r=1 vs H1: r=2, y así sucesivamente. Aquí k=2
eigen1 = ca.jo(Y, ecdet = "none", type = "eigen", K = 2, spec = "transitory", season = NULL)
summary(eigen1) #Al 5% de confianza las series están cointegradas.

#Criterio de la traza. Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r>=1, luego H0: r<=1 vs H1: r>1, y así sucesivamente. Aquí k=2

trace1= ca.jo(Y, ecdet = "none", type = "trace", K = 2, spec = "transitory", season = NULL)
summary(trace1) #Al 5% de confianza las series están cointegradas.

# 2.2 Test de Johansen - con constante dentro del vector de cointegración (Es decir, modelo sin tendencia lineal) ----

#Criterio del valor propio (es la prueba más robusta). Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r=1, luego H0: r=1 vs H1: r=2, y así sucesivamente. Aquí k=2

eigen2 = ca.jo(Y, ecdet = "const", type = "eigen", K = 2, spec = "longrun",season = NULL)
summary(eigen2) #Al 5% de confianza las series están cointegradas.

#Criterio de la traza. Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r>=1, luego H0: r<=1 vs H1: r>1, y así sucesivamente. Aquí k=2

trace2 = ca.jo(Y, ecdet = "const", type = "trace", K = 2, spec = "longrun",season = NULL)
summary(trace2) #Al 5% de confianza las series están cointegradas.

# 2.3 Estimación del modelo VEC con drift fuera del vector de cointegración (Es decir, modelo con tendencia lineal) ----

#La función cajorls permite estimar el modelo VEC
VEC1 = cajorls(eigen1, r=1) 
VEC1

#Con esta función obtenemos el vector de cointegración normalizado
coefB(VEC1)

#Con esta función obtenemos los coeficientes de velocidad de ajuste
coefA(VEC1)

# 2.4 Estimación del modelo VEC con constante dentro del vector de cointegración (Es decir, modelo sin tendencia lineal) ----

#La función cajorls permite estimar el modelo VEC
VEC2 = cajorls(eigen2, r=1) 
VEC2

#Con esta función obtenemos el vector de cointegración normalizado
coefB(VEC2)

#Con esta función obtenemos los coeficientes de velocidad de ajuste
coefA(VEC2)

# 2.5 Test para determinar la tendencia lineal en el modelo VEC ----

#Como ya se mencionó, se va a desarrollar un test de ratio de verosimilitud para evaluar la inclusión o no 
#inclusión de una tendencia lineal en el modelo VEC. Es decir, mirar si se incluye A0 es diferente de 0 
# o A0=0
#H0: Incluir intercepto en el vector de cointegración (Es decir, no incluir tendencia lineal -- A0=0)
#Ha: Incluir tendencia lineal en el modelo (I.e A0 es diferente de cero)

# Nota: El estadístico sigue una distribución chi^2 por lo que es una pruba de cola derecha

# Nota: Para mayor información sobre el estadístico de prueba ver ecuación 6.57 pag. 381 Enders

lttest(eigen2, r=1) #No rechazo la hipótesis nula, por lo que no se debe incluir tendencia lineal en el modelo. 
                    #Es decir, el vector de cointegración incluye constante

# Por los resultados de la prueba anterior, me quedo con el modelo eigen2 el cual es el modelo con término
# constante en el vector de cointegración y que no tiene tendencia lineal determínistica


# 3. Análisis y pruebas de hipótesis sobre las matrices alpha y beta ---- 

# 3.1 Análisis de las matrizes de velocidad de ajuste (alpha) y del vector de coientración (beta) ----

#Con esta función obtenemos el vector de cointegración normalizado
coefB(VEC2)

#Con esta función obtenemos los coeficientes de velocidad de ajuste
coefA(VEC2)

# 3.2 Pruebas de hipótesis sobre las matrices alpha y beta ----
# Nota: En este script no se realizaran dichas pruebas de hipótesis pero para los interesados
#       en la sección 8.1.3 (pag. 134) y 8.1.4 (pag. 136) del libro de pfaff se encuentran diferentes pruebas 

# 4. Validación de supuestos y usos del modelo ----

# 4.1 Reparametrización del VECM como un VAR en niveles ----

# Para la validación de supuestos y hacer uso de los resultados del modelo es necesario convertir el VECM que 
# se acaba de estimar nuevamente en un modelo VAR en niveles. Lo anterior, es inmediato dado que, como ya se sabe, 
# el modelo VECM es una reparametrización de un modelo VAR en niveles

# Nota: Efectivamente lo que se está haciendo al pasar de un modelo VECM a un modelo VAR es hacer el proceso opuesto
#       al que hace la profe en las diapositivas 24, 25 y 25 D7-SeriesMultivariadas

# Nota: Dado los resultados de la prueba lttest, se usara el modelo VEC con constante en el vector de cointegración

VAR.oil = vec2var(eigen2, r = 1) # VAR.oil va a ser la reparametrización del modelo VEC anterior como VAR en niveles
VAR.oil
class(VAR.oil) # Notar que la clase del objeto ahora es vec2var. Esto es importante, dado que se necesita el 
               # modelo VEC reparametrizado como una VAR en niveles para poder validar los supuetos y poder
               # hacer uso del modelo

# 4.2 Validación de supuestos: reparametrización del VECM como un VAR en niveles ----

# 4.2.1 Autocorrelación: PT.asymptotic es para muestra grande y "PT.adjusted" es corrección para muestra pequeña. ----

P.75=serial.test(VAR.oil, lags.pt = 75, type = "PT.asymptotic");P.75 #No rechazo, se cumple el supuesto
P.30=serial.test(VAR.oil, lags.pt = 30, type = "PT.asymptotic");P.30 #No rechazo, se cumple el supuesto
P.20=serial.test(VAR.oil, lags.pt = 20, type = "PT.asymptotic");P.20  #No rechazo, se cumple el supuesto
P.10=serial.test(VAR.oil, lags.pt = 10, type = "PT.asymptotic");P.10 #rechazo, no se cumple el supuesto

#Graficamos los residuales para 20 lags: se grafican los residuales, su distribución, la ACF y PACF de los residuales y
#la ACF y PACF de los residuales al cuadrado (proxy para heterocedasticidad)
x11()
plot(P.20, names = "P.Brent") #Bien comportados, salvo por los residuales al cuadrado
plot(P.20, names = "P.WTI") #Bien comportados, salvo por los residuales al cuadrado

# 4.2.2 Homocedasticidad: Test tipo ARCH multivariado ----
arch.test(VAR.oil, lags.multi = 24, multivariate.only = TRUE) #Rechazo, no se cumple el supuesto.
arch.test(VAR.oil, lags.multi = 12, multivariate.only = TRUE) #Rechazo, no se cumple el supuesto

# 4.2.3 Normalidad: Test Jarque-Bera multivariado ----
normality.test(VAR.oil) #Rechazo, no se cumple el supuesto. 

# Nota: Como se violan los supuestos de heterocedasticidad y normalidad, hay que calcular los intevalos de confianza
#       mediante bootstrapping para poder hacer inferencia estadística correcta (tanto en los pronósticos como en las imf)

# 4.3 Pronóstico del VECM reparamterizado como un VAR en niveles ----

#Recuerden que debido al incumplimiento de normalidad, los intervalos de confianza deben computarse por bootstrapping.
x11()
predict(VAR.oil, n.ahead=12)
plot(predict(VAR.oil, n.ahead=12))

# 4.3 Funciones impulso respuesta para VECM reparamterizado como un VAR en niveles ----

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

#Definimos el número pasos adelante
lags=c(0:18)

#IRF de las variables del sistema ante distintos choques exógenos.

y1.y1 <- impulso_respuesta(VAR.oil,"P.WTI","P.WTI",pasos_adelantes=c(0:18),ortog = T,int_conf = 0.95,titulo = "Impulso de P.WTI - respuesta de P.WTI")
y1.y2 <- impulso_respuesta(VAR.oil,"P.WTI","P.Brent",pasos_adelantes = c(0:18),ortog = T,int_conf = 0.95,titulo = "Impulso de P.WTI - respuesta de P.Brent")
y2.y1 <- impulso_respuesta(VAR.oil,"P.Brent","P.WTI",pasos_adelantes = c(0:18),ortog = T,int_conf = 0.95,titulo = "Impulso de P.Brent - respuesta de P.WTI")
y2.y2 <- impulso_respuesta(VAR.oil,"P.Brent","P.Brent",pasos_adelantes = c(0:18),ortog = T,int_conf = 0.95,titulo = "Impulso de P.Brent - respuesta de P.Brent")

x11()
grid.arrange(y1.y1,y1.y2,y2.y1,y2.y2,ncol=2)

# Ejemplo No. 2  ----

## vamos a replicar nuevamente el ejemplo del libro introduction to econometrics with R de Hanck, Arnold, Gerber y Schmelzer. 
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

#Graficamos las series
x11()
autoplot(cbind(TB3MS, TB10YS), facets = F, main="Tasa de interés de corto y largo plazo", xlab="", ylab="", size=2)

#Aplicamos las pruebas de raíz unitaria
#Procedemos a hacer las pruebas de raíz unitaria

#bonos de corto plazo
summary(ur.df(TB3MS, lags=6, selectlags="AIC", type="drift")) #No tiene drift
summary(ur.df(TB3MS, lags=6, selectlags="AIC", type="none")) #La serie es no estacionaria

#bonos de largo plazo
summary(ur.df(TB10YS, lags=6, selectlags="AIC", type="drift")) #No tiene drift
summary(ur.df(TB10YS, lags=6, selectlags="AIC", type="none")) #La serie es no estacionaria

#Posteriormente, estimaremos un VAR en niveles para determinar el tamaño del rezago

#Analizamos lo que dicen los criterios de información
Z = cbind(TB3MS, TB10YS)
VARselect(Z, lag.max = 6, type="both", season=NULL) #2 y 6 rezagos
VARselect(Z, lag.max = 6, type="const", season=NULL) #2 rezagos y 6 rezagos
VARselect(Z, lag.max = 6, type="none", season=NULL) #2 rezagos y 6 rezagos

#Estimamos cada uno de los modelos y determinamos cuál es el más apropiado
summary(VAR(Z, p=6, type="both", season=NULL)) #La tendencia es significativa sólo en una ecuación
summary(VAR(Z, p=6, type="const", season=NULL)) #La constante es significativa sólo en una ecuación

#Comparamos el ajuste en ambos modelos
cbind(AIC(VAR(Z, p=6, type="both", season=NULL)),AIC(VAR(Z, p=6, type="const", season=NULL)))

#Elegiremos el modelo con sólo intercepto, debido a que la diferencia entre ambos es muy poca y es preferible tener un modelo más parsimonioso.
VAR.6 <- VAR(Z, p=6, type="const", season=NULL)

#Vamos a analizar el comportamiento de los residuales, si no se comportan bien incluiremos más rezagos
P.75=serial.test(VAR.6, lags.pt = 57, type = "PT.asymptotic");P.75 #No rechazo, se cumple el supuesto
P.30=serial.test(VAR.6, lags.pt = 30, type = "PT.asymptotic");P.30 # rechazo, no se cumple el supuesto
P.20=serial.test(VAR.6, lags.pt = 20, type = "PT.asymptotic");P.20  #rechazo, no se cumple el supuesto
 
#Vamos a estimar un modelo con 8 rezagos, debido a que con 6 rezagos los residuales no se comportan bien

#Elegiremos el modelo con sólo intercepto, debido a que la diferencia entre ambos es muy poca y es preferible tener un modelo más parsimonioso.
VAR.8 <- VAR(Z, p=8, type="const", season=NULL)

#Vamos a analizar el comportamiento de los residuales, si no se comportan bien incluiremos más rezagos
P.75=serial.test(VAR.8, lags.pt = 57, type = "PT.asymptotic");P.75 #No rechazo, se cumple el supuesto
P.30=serial.test(VAR.8, lags.pt = 30, type = "PT.asymptotic");P.30 #No rechazo, se cumple el supuesto
P.20=serial.test(VAR.8, lags.pt = 20, type = "PT.asymptotic");P.20  #rechazo, no se cumple el supuesto


#Graficamos los residuales para 20 lags: se grafican los residuales, su distribución, la ACF y PACF de los residuales y
#la ACF y PACF de los residuales al cuadrado (proxy para heterocedasticidad)
x11()
plot(P.20, names = "TB3MS") #Bien comportados, salvo por los residuales al cuadrado
plot(P.20, names = "TB10YS") #Bien comportados, salvo por los residuales al cuadrado

#___________________________________________________________________________________________________________________________________________________________

#Ahora, determinando que p=8, vamos a aplicar la prueba de cointegración de Johansen. Este prueba analiza el rango de la matriz que acompaña
#al término de corrección de error (x_t-1) en la ecuación del modelo VECM. Si r=0, se determina que no existe cointegración entre las series 
#por lo que debe estimarse un VAR (p-1) en diferencias. Si r < g (siendo g el número de variables), se dice que la matriz tiene rango  
#reducido y r representará las relaciones de cointegración. Finalmente, si r=g decimo que la matriz tiene rango completo, de manera que debe 
#estimarse un VAR(p) en niveles.

#Con objeto de que el vector de cointegración sea únicamente identificado, debe normalizarse respecto alguna variable. Esto permite descomponer
#la matriz PHI = Alpha*Beta'. Donde la matriz Beta contiene el vector de cointegración normaliza y alpha contiene los coeficientes de ajuste a la relación 
#de cointegración, ante una desviación de corto plazo en la misma. 

#Finalmente, la inclusión de una constante en el término de corrección de error generalmente se emplea cuando se cree que las variables tienen 
#tendencia lineal. Si se incluye constante y tendencia, generalmente es porque se cree que las variables siguen una tendencia cuadrática. No 
#obstante, existe un test para determinar cuál es la mejor especificación

#Por último, se pueden hacer diferentes pruebas sobre restricciones en los parámetros, lo cual es importante para contrastar los resultados 
#empíricos con los postulados teóricos. 

#____________________________________________________________________________________________________________________________________________________________

#######################################################
# Test de Johansen - (sin términos deterministicos)
########################################################

#Criterio del valor propio (es la prueba más robusta). Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r=1, luego H0: r=1 vs H1: r=2, y así sucesivamente. Aquí k=8
eigen.1 = ca.jo(Z, ecdet = "none", type = "eigen", K = 8, spec = "longrun",season = NULL)
summary(eigen.1) #Al 5% de confianza las series están cointegradas.

#Criterio de la traza. Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r<=1, luego H0: r=1 vs H1: r>1, y así sucesivamente. Aquí k=8

trace.1= ca.jo(Z, ecdet = "none", type = "trace", K = 8, spec = "longrun",season = NULL)
summary(trace.1) #Al 5% de confianza las series están cointegradas.

#######################################################
# Test de Johansen - (con términos deterministicos)
########################################################

#Criterio del valor propio (es la prueba más robusta). Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r=1, luego H0: r=1 vs H1: r=2, y así sucesivamente. Aquí k=8

eigen.2 = ca.jo(Z, ecdet = "const", type = "eigen", K = 8, spec = "longrun",season = NULL)
summary(eigen.2) #Al 5% de confianza las series están cointegradas.

#Criterio de la traza. Es un procedimiento secuencial en donde se contrasta
# H0: r=0 vs H1: r<=1, luego H0: r=1 vs H1: r>1, y así sucesivamente. Aquí k=8

trace.2 = ca.jo(Z, ecdet = "const", type = "trace", K = 8, spec = "longrun",season = NULL)
summary(trace.2) #Al 5% de confianza las series están cointegradas.


##########################################################
#  Estimación del modelo VEC: sin términos determinísticos 
##########################################################

#Aquí estimamos el modelo VEC
VEC.1 = cajorls(eigen.1, r=1) 
VEC.1

#Con esta función obtenemos el vector de cointegración normalizado
coefB(VEC.1)

#Con esta función obtenemos los coeficientes de velocidad de ajuste
coefA(VEC.1)


##########################################################
#  Estimación del modelo VEC: con términos determinísticos 
##########################################################

#Aquí estimamos el modelo VEC
VEC.2 = cajorls(eigen.2, r=1) 
VEC.2

#Con esta función obtenemos el vector de cointegración normalizado
coefB(VEC.2)

#Con esta función obtenemos los coeficientes de velocidad de ajuste
coefA(VEC.2)

########################################################
# Test para determinar la inclusión de tendencia lineal
########################################################

#Se va a desarrollar un test de ratio de verosimilitud para evaluar la inclusión de términos determinísticos.
#La hipótesis nula corrresponde a NO incluir tendencia lineal

lttest(eigen.2, r=1) #No rechazo la hipótesis nula, por lo que no se debe incluir constante en el vector de cointegración. 

#########################################################
# Convertir un modelo VEC en un modelo VAR en niveles
########################################################

VAR.IR = vec2var(eigen.1, r = 1)


################################################
# Validación de supuestos: representación VAR
################################################

## Autocorrelación: PT.asymptotic es para muestra grande y "PT.adjusted" es corrección para muestra pequeña.

P.75=serial.test(VAR.IR, lags.pt = 75, type = "PT.asymptotic");P.75 #No rechazo, se cumple el supuesto
P.30=serial.test(VAR.IR, lags.pt = 30, type = "PT.asymptotic");P.30 #No rechazo, se cumple el supuesto
P.20=serial.test(VAR.IR, lags.pt = 20, type = "PT.asymptotic");P.20  #rechazo, no se cumple el supuesto
 
#Graficamos los residuales para 20 lags: se grafican los residuales, su distribución, la ACF y PACF de los residuales y
#la ACF y PACF de los residuales al cuadrado (proxy para heterocedasticidad)
x11()
plot(P.20, names = "TB3MS") #Bien comportados, salvo por los residuales al cuadrado
plot(P.20, names = "TB10YS") #Bien comportados, salvo por los residuales al cuadrado

#Homocedasticidad: Test tipo ARCH multivariado
arch.test(VAR.IR, lags.multi = 24, multivariate.only = TRUE) #Rechazo, no se cumple el supuesto.
arch.test(VAR.IR, lags.multi = 12, multivariate.only = TRUE) #Rechazo, no se cumple el supuesto

##Test Jarque-Bera multivariado
normality.test(VAR.IR) #Rechazo, no se cumple el supuesto. 


###########################################
# Pronóstico de la representación VAR
###########################################

#Recuerden que debido al incumplimiento de normalidad, los intervalos de confianza deben computarse por bootstrapping.
x11()
predict(VAR.IR, n.ahead=12)
plot(predict(VAR.IR, n.ahead=12))


###################################################
# Análisis impulso-respuesta representación VAR
##################################################

#Definimos el número pasos adelante
lags=c(0:18)

#IRF de las variables del sistema ante distintos choques exógenos.
IRF1. = irf(VAR.IR, impulse="TB3MS",response="TB3MS",n.ahead = 18,ci = 0.95, boot=T, ortho=T) #No analizaremos respuestas ortogonales (Ahora vemos qué es eso);  
IRF1.1.= data.frame(IRF1.$irf,IRF1.$Lower,IRF1.$Upper, lags)
IRF2. = irf(VAR.IR, impulse="TB3MS",response="TB10YS",n.ahead = 18,ci=0.95, boot=T, ortho=T) #No analizaremos respuestas ortogonales (Ahora vemos qué es eso);  
IRF1.2.= data.frame(IRF2.$irf,IRF2.$Lower,IRF2.$Upper, lags)
IRF3. = irf(VAR.IR, impulse="TB10YS",response="TB3MS",n.ahead = 18,ci=0.95, boot=T, ortho=T) #No analizaremos respuestas ortogonales (Ahora vemos qué es eso);  
IRF2.1.= data.frame(IRF3.$irf,IRF3.$Lower,IRF3.$Upper, lags)
IRF4. = irf(VAR.IR, impulse="TB10YS",response="TB10YS",n.ahead = 18,ci=0.95, boot=T,ortho=T) #No analizaremos respuestas ortogonales (Ahora vemos qué es eso);
IRF2.2.= data.frame(IRF4.$irf,IRF4.$Lower,IRF4.$Upper, lags)

Y1.Y1 <- impulso_respuesta(VAR.IR,"TB3MS","TB3MS",pasos_adelantes = c(0:18),ortog = F,int_conf = 0.95,titulo = "Impulso de TB3MS - respuesta de TB3MS ")
Y1.Y2 <- impulso_respuesta(VAR.IR,"TB3MS","TB10YS",pasos_adelantes = c(0:18),ortog = F,int_conf = 0.95,titulo = "Impulso de TB3MS - respuesta de TB10YS")
Y2.Y1 <- impulso_respuesta(VAR.IR,"TB10YS","TB3MS",pasos_adelantes = c(0:18),ortog = F,int_conf = 0.95,titulo = "Impulso de TB10YS - respuesta de TB3MS")
Y2.Y2 <- impulso_respuesta(VAR.IR,"TB10YS","TB10YS",pasos_adelantes = c(0:18),ortog = F,int_conf = 0.95,titulo = "Impulso de TB10YS - respuesta de TB10YS")

x11()
grid.arrange(Y1.Y1,Y1.Y2,Y2.Y1,Y2.Y2,ncol=2)
