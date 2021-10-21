library(tidyverse)
library(readxl)
library(xts)

set.seed(9821)

#### Bootstrapping Enders ####

# La simulación por bootstrapping es parecida a una simulación de Monte Carlo 
# excepto por una diferencia esencial: 
## En una simulación de Monte Carlo, uno general las variables aleatorias de una
## distribución data (teórica), como lo puede ser una distribución normal. 
## Por otro lado, en una simulación por bootstrapping las variables aleatorias se
## generan de la distribución muestral observada. 

## La simulación por bootstrapping usa "the plug-in-principle", en el que 
## la distribución observada de las variables aleatorias es la mejor estimación
## de su verdadera distribución. 

# La simulación por bootstrap fue propuesta por Efron(1979). 
# La idea principal de Efron es que la muestra de datos observada, es una muestra
# aleatoria de tamaño T proveniente de la verdadera distribución generando 
# los datos

# En escenia, la distibución empirica de los datos es la mejor estimación 
# de la verdadera distribución de los datos. 

# La función de distribución empírica se define como la distribución discreta
# que coloca una probabilidad de 1/T en cada uno de los datos observados. Es
# dicha distribución empírica (y no una distribución teórica predefinida) que
# se usa para generar las varaibles aleatorias

# Una muestra obtenida por bootstrapping es una muetra aleatoria de tamaño T
# sacada con reemplazo de los datos observados que pone una probabilidad de
# 1/T en cada uno de los datos observados. 

# Nota: Muestra con reemplazo y muestra sin reemplazo

## Una muestra que se construye con reemplazo es aquella en donde luego de sacar
## un elemento este se puede volver a considerar para futuros elecciones de 
## la muestra que se está generando. Es decir, que una observación se haya 
## seleccionado no impide que dicha observación no pueda volver a salir en el futuro
## Inclusive, una observación que ya haya sido seleccionada tiene la misma 
## probabilidad de salir que una observación que no haya sido seleccionada
## en selecciones futuras. Por tanto, la selección de observaciones es 
## totalmente independiente dado que no depende de que observación haya
## salido en el pasado

## Un muestra que se construye sin reemplazo es (completar más tarde...)

# Ejemplo 1: Bootstrapping convencional (iid) programado de manera manual ----

# Se tienen la una muestra observada con los siguientes 10 valores 
vect = c(0.8, 3.5, 0.5, 1.7, 7, 0.6, 1.3, 2, 1.8, -0.5)

# Características del vector
mean(vect)
sd(vect)

# Función que me genera muestreos basados en bootstrapping para el vector vect
manual_boots = function(num_boots, vect){
  # num_boots: número total de muestras generadas por bootstrapping 
  # vect: vector del ques se generará el remuestreo con reemplazo
  matriz = matrix(nrow = num_boots, ncol = length(vect))
  # Cada fila de la matriz es una muestra generada por bootstrapping
  for (rep in 1:num_boots){
    # la función sample me permite seleccionar elementos del vector vect con la misma probabilidad. 
    # Replace = TRUE, para obetner una muestra con reemplazo
    matriz[rep, ] = sample(vect, size = length(vect), replace = TRUE) 
  }
  return(matriz)
}

# Genero la simulación por bootstrapping
# matriz_boot = manual_boots(100, vect)

# Ejemplo 3: Bootstrapped residuals for AR(1) model ----

setwd("~/Scripts_personal_R/scripts_código_propio/simulacion/bootstrapping")
real_gdp = read_xls("Real.38134309.xls") %>% 
  mutate(DATE = as.Date(DATE))

# Se genera el crecimiento porcentual del PIB de los Estados Unidos 
gdp_growth = diff(log(ts(real_gdp$RGDP, start = c(1947, 1), frequency = 4)))
# Como objeto xts: 
gdp_xts = xts(real_gdp$RGDP, order.by = real_gdp$DATE)
gdp_growth_xts = diff(log(gdp_xts))

# Gráfica de crecimiento real del GDP 
plot(gdp_growth_xts)

# Para el presente ejemplo, se va a construir intervalos de confianza del 90 %
# para el coeficiente de un proceso AR(1) a partir de simulación por bootstrapping 

boot_ar1_process = function(num_boots, conf_interval, serie_original){
  # Vars:
  ## num_boots: número total de muestras que van a ser generadas por bootstrapping
  ## conf_interval: intervalo de confianza para los parámetros del modelo AR(1)
  ## serie_original: serie observada (en estre caso el crecimiento porcentual del PIB)
  ###
  # Dataframe que va a almacenar cada uno de los coeficientes del AR(1) que son
  # obtenidos mediante la estimación de modelos AR(1) sobre las 
  # muestras generadas por bootrapping
  df_coef_boot = tibble(ar1_coef = double())
  ###
  # Procedimento de Bootstrapped residuals para un proceso AR(1)
  # Paso 1: Estimar el modelo y calcular los residuales de este
  # Se estima un modelo AR(1) para el crecimiento porcentual del PIB de USA
  reg1 = arima(serie_original, order = c(1,0,0), include.mean = T, method = "ML")
  reg_coef = reg1$coef # coeficientes del modelo AR(1)
  res = reg1$residuals # residuales del modelo AR(1)
  # Nota: se calcula la longitud de la serie original ya que es fundamental para construir las muestras por bootstrapping
  series_size = length(serie_original)
  # Paso 4: Repetir los pasos 2 y 3 muchas veces 
  for (boot in 1:num_boots){
    # Paso 2: Generar bootstrapping para los residuales y para la serie {y_t}
    # bootstrapping de los residuales 
    # Nota: Observar que el núcleo del bootstrapping se encuentra en la función sample
    # Nota: Observar que la muestra del bootstrapping para los residuales tiene 50 
    #       observaciones más que los residuales originales. Ésto, se debe a que
    #       se van a eliminar las primeras 50 observaciones de las series generadas
    #       por boostrapping para mitigar los efectos que pueda tener la selección 
    #       de la condción inicial para el modelo
    boot_res = sample(res, size = series_size + 50, replace = TRUE)
    # Generar la secuencia {y_t^*} 
    ## Para la condición inicial para la simulación de las series y por boostrapping 
    ## se selecciona un elemento de la serie original como condición inicial en la
    ## simulación 
    initial_cond = sample(serie_original, size = 1) # condición inicial para el modelo AR(1)
    y = c(initial_cond) # se va a generar un nuevo y por cada muestra obtenida por bootstrapping
    # En este for loop básicamente se está simulando un proceso AR(1) utilizando los residuales obtenidos por bootstrapping
    # En otras palabras se está generando la muestra de la serie por bootstrapping
    # Cada vector y al final debería tener una tamaño igual a series_size 
    # (inicialmente va a tener 50 elementos de más pero los primeros 50 elementos de la serie simulada 
    # por bootstrapping se eliminan para aislar y mitigar el efecto de las condiciones iniciales
    # como sugiere Enders)
    for (elem in 2:(series_size + 50)){
      # ojo: Cada serie original construida por bootstrapping se construye usando los coeficientes originales
      y_new = reg_coef[2] + reg_coef[1] * y[elem - 1] + boot_res[elem] # genero la secuencia de {y_t} usando los residuales obtenidos por bootstrapping
      y = append(y, y_new) # se construye la serie y por bootstrapping de manera iterativa
    }
    y = y[-c(1:50)] # El vector simulado y tiene el mismo tamaño que la serie_original 
    # Paso 3: Usar la secuencia {y_t^*} obtenida por 
    #         bootstrapping para reestimar los coeficientes del AR(1)
    reg_boot = arima(y, order = c(1,0,0), include.mean = TRUE, method = "ML") # estimación de un AR(1) para cada serie generada por bootstrapping
    # se extran los coeficientes del AR(1) estimados a partir de la muestra generada por bootstrapping
    reg_coef_boot = reg_boot$coef
    # Se van agregando cada uno de los parámetros del modelo AR(1) estimado por la muestra generada por bootstrapping
    # en un data.frame que se va a utilizar para encontrar los intervalosy de confianza 
    df_coef_boot[boot, ] = reg_coef_boot[1]
  }
  # df_final consiste en el dataframe que contiene todos los valores del coeficiente
  # del AR(1) obtenidos de las estimaciones usando las series simuladas por bootstrapping
  # pero ordenando de manera ascendiente el valor de dichos coeficientes
  # Es necesario ordenar el valor de dichos coeficientes de manera ascendente dado que 
  # asi es como se identificara el intervalo de confianza inferior y el intervalo de 
  # confianza superior generado por bootstrapping
  df_final = df_coef_boot %>% 
    arrange(ar1_coef)
  # Selección de los intervalos de confianza 
  conf_inf = ((1 - (conf_interval/100))/2) * num_boots # encuentro en que posición se encuentra el intervalo de confianza inferior
  conf_sup = num_boots - conf_inf # encuentro en que posición se encuentra el intervalo de confianza superior
  intervals = c(df_final$ar1_coef[conf_inf], df_final$ar1_coef[conf_sup]) # encuentro los intervalos de cofianza para el parámetro AR(1)
  return(intervals) 
}

# Para un intervalo de confianza del 90 % y simulando 10000 series mediante 
# bootstrapping se obtiene que los intervalos de confianza están dados por: 
prueba = boot_ar1_process(num_boots = 10000, conf_interval = 90, serie_original = gdp_growth)

# Nota: Lo interesante del procedimiento anterior es que no asume ninguna
#       distribución en los errores. Por el contrario, la simulación por
#       bootstrapping lo que busca es utilizar la misma muestra observada
#       de los errores (los residuales) para encontrar los intervalos de 
#       confianza del proceso AR(1) sin tener que asumir ninguna distribución
#       teórica como se haría en un procedimiento con inferencia clásica
#       o convencional. 

# Nota: Los valores que da Enders en su manual suplementario son
## intervalo inferior: 0.2652
## intervalo superior: 0.4524
# Que se puede observar son muy cercanos a los valores encontrados en 
# esta simulación 



