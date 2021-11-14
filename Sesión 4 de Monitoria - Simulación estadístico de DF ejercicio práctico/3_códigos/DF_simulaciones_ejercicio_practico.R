##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2021-I
#      SESIÓN MONITORIA: Derivación y uso de pruebas de raíz unitaria -  
#                        Prueba Dickey Fuller Aumentada (ADF)
##____________________________________________________________________________________
##_____________________________________________________________________________________

# Limpiar el Environment
# rm(list = ls()) 

# Importación de paquetes
library(tseries)
library(stargazer)
library(dynlm)    # Paquete que me permite realizar MCOD
library(urca)     # Paquete que me permite realizar pruebas de raíz unitaria 
library(tidyverse)  #Conjunto de paquetes que incluyen ggplot2 para realizar la parte gráfica

# La primera parte del script consistirá en realizar una serie de simulaciones de Monte Carlo, de tal forma que se 
# pueda obtener la distribución empírica del estadístico de Dickey-Fuller para diferentes especificaciones de la
# prueba, a saber: prueba con tendencia e intercepto, prueba con intercepto pero sin tendencia y prueba sin tendencia ni intercepto

# Preparación de las simulaciones 
set.seed(1)    # Se inicia una semilla para que los valores no cambien cada vez que se ejecute el código
n = 100        # n denota el número de observaciones que tendrá cada serie simulada (es decir, cada serie simulada tendrá 100 datos)
N = 10000      # N denota el número total de series que se van a simular, en total se van a simular 10.000 series

# Otros parámetros importantes: 
trend = 1:n # (tendencia deterministica lineal)
drift = 0.5 # (deriva)

#---- Funciones para la simulación ----

# Simulacion de monte carlo para el estadistico de DF
simulacion_DF = function(n, N, tipo_prueba, trend = 1:n, drift = 0.1){
  # trend y drift tienen unos valores por default que el usuario puede modificar incluyendo otros valores para dichas variables a la hora de llamar la función
  # (Recuerden que el estadístico de DF surge como el estadístico-t de una regresión, cuya forma funcional depende de los terminos determíniscos que tenga la prueba DF)
  # sim_df va a ser un data frame que irá almacenando los valores del parámtero de interés(el que acompaña y_(t-1) en la regresión que se usa para obtener el estadístico DF)
  # y almacena también el estadístico t (es decir el valor que se obtiene del estadístico de DF para la serie simulada)
  # de DF que se obtiene enc ada iteración del código
  
  # Nota: Recuerden que si la serie no es estacionaria, el valor del parámetro de interés debería ser cero (hipótesis nula para una prueba DF)
  # El data frame que se creeara tendrá dimensiones de 10000 x 2
  # Las filas del data frame almacenarán los resultados de cada iteración de la simulación 
  sim_df = data.frame(parametro_interes = double(), DF_statistic = double())
  if(tipo_prueba == "drift+trend"){
    # El siguiente for es donde se llevará a cabo la simulación de Monte Carlo. En este for, se simularán las series (de 100 datos cada una) 10000 veces
    # Cada iteración del for simulará una nueva serie de 100 datos (en total se realizarán 10000 iteraciones, es decir, se simularán 10000 series de tiempo)
    # Nota: La parte aleatoria proviene de la porción de código: rnorm(obs, mean = 0, sd =1) donde se simula el error como un ruido blanco normal independiente (iid) 
    #       con media cero y varianza 1 
    for(i in 1:N){
      # En cada iteración del for se hacen dos pasos: 
        # 1. Se simula la serie de tiempo
      yt = arima.sim(model= list(order = c(0,1,0)), n = n-1, innov = rnorm(n, mean = 0, sd =1)) + drift + trend # se simula la serie de tiempo
        # 2. Se obtiene el estadístico t de Dickey Fuller asociado a la anterior simulación.
        #    Para ello se estima el modelo correspondiente al tipo de prueba de DF que se vaya a efectuar 
        #    (Recuerden que el estadístico de DF surge como el estadístico-t de una regresión, cuya forma funcional depende de los terminos determíniscos que tenga la prueba de DF)
        #    Para hacer la regresión correspondiente a la prueba de DF se usa el comando dynlm que es un comando que me perite estimar modelos por MCOD (minimos cuadrados ordinarios dinamicos)
        #       diff(yt,1) es para sacar la primera diferencia de la serie y L(yt,1) es para sacar la serie rezagada (nota: para que el comando funcione hay que proveer objetos ya sean ts o zoo) 
        #       trend(yt) es para sacar la tendencia determinística de la serie 
      summa = summary(dynlm(diff(yt, 1) ~ L(yt, 1) + trend(yt))) # se estima el modelo correspondiente a una prueba DF con tendencia y deriva
      sim_df[i,] = c(summa$coef[2,1], summa$coef[2,3])       # Los resultados de los dos pasos anteriores se almacenan en cada fila del data frame
    }
  # Lo que se hizo para el estadístico de DF con drift y trend también se puede hacer para los otros estadísticos de DF con solo drift y sin terminos determinísticos
  }else if(tipo_prueba == "drift"){
    for(i in 1:N){
      yt = arima.sim(model= list(order = c(0,1,0)), n = n, innov = rnorm(n, mean = 0, sd =1)) + drift # se simula la serie de tiempo
      summa = summary(dynlm(diff(yt, 1) ~ L(yt, 1))) # se estima el modelo correspondiente a una prueba DF con solo deriva
      sim_df[i,] = c(summa$coef[2,1], summa$coef[2,3])
    }
  }else if(tipo_prueba == "none"){
    for(i in 1:N){
      yt = arima.sim(model= list(order = c(0,1,0)), n = n, innov = rnorm(n, mean = 0, sd =1)) # se simula la serie de tiempo
      summa = summary(dynlm(diff(yt, 1) ~ L(yt, 1) - 1)) # se estima el modelo correspondiente a una prueba DF sin términos determinísticos
      sim_df[i,] = c(summa$coef[1,1], summa$coef[1,3])
    }
  }
  return(sim_df)
}

# Histograma para el parámetro de interés en la prueba de DF (el que acompaña y_{t-1})
histogram_rho = function(df, titulo, x_lab, color_llenado, num_bins = 30){
  x11()
  histog = df %>%
    ggplot(aes(x = parametro_interes)) +
    geom_histogram(color = "black", fill = color_llenado, bins = num_bins) + 
    theme_light() +
    ggtitle(titulo) +
    ylab("Frencuencia") +
    xlab(x_lab)
  return(histog)
}

# Histograma para el estadístico de DF

histogram_DF = function(df, titulo, x_lab, color_llenado, num_bins = 30){
  x11()
  histog = df %>%
    ggplot(aes(x = DF_statistic)) +
    geom_histogram(color = "black", fill = color_llenado, bins = num_bins, aes(y = ..density..)) + 
    geom_density() + 
    theme_light() +
    ggtitle(titulo) +
    ylab("Densidad") +
    xlab(x_lab)
  return(histog)
}

#---- Simulación DF con drift y trend ---- 

# Simulación de monte carlo para una estadístico DF con trend y drift
df_trend_drift = simulacion_DF(n = n, N = N, tipo_prueba = "drift+trend", trend = trend, drift = drift)

# Histograma para el parámetro de interés en la prueba de DF (el que acompaña y_{t-1})
histogram_rho_trend_drift = histogram_rho(df_trend_drift,
                                        "Rho estimado prueba Dickey-Fuller con drift y trend con N = 10000 y n = 100", 
                                        "Valores rho", "red1"); histogram_rho_trend_drift

# Histograma del estadístico-t de DF con trend y drift
hist_DF_trend_drift = histogram_DF(df_trend_drift, 
                            "Estadístico Dickey-Fuller con drift y trend con N = 10000 y n = 100",
                            "Valores estadístico de DF", "turquoise1"); hist_DF_trend_drift

# Valores críticos estadístico-t de DF con trend y drift
critical_trend_drift = round(quantile(df_trend_drift$DF_statistic, c(0.1, 0.05, 0.01)), 2); critical_trend_drift

#---- Simulación DF con drift pero sin trend ---- 

# Simulación de monte carlo para una estadístico DF con drift
df_drift = simulacion_DF(n = n, N = N, tipo_prueba = "drift", trend = trend, drift = drift)

# Histograma para el parámetro de interés en la prueba de DF (el que acompaña y_{t-1})
histogram_rho_drift = histogram_rho(df_drift,
                                        "Rho estimado prueba Dickey-Fuller solo con drift con N = 10000 y n = 100", 
                                        "Valores rho", "purple1"); histogram_rho_drift

# Histograma del estadístico-t de DF solo con deriva
hist_DF_drift = histogram_DF(df_drift, 
                            "Estadístico Dickey-Fuller solo con drift con N = 10000 y n = 100",
                            "Valores estadístico de DF", "orange1"); hist_DF_drift

# Valores críticos estadístico-t de DF solo con drift
critical_drift = round(quantile(df_drift$DF_statistic, c(0.1, 0.05, 0.01)), 2); critical_drift

#---- Simulación DF sin drift ni trend ---- 

# Simulación de monte carlo para una estadístico DF sin términos determinísticos 
df_none = simulacion_DF(n = n, N = N, tipo_prueba = "none", trend = trend, drift = drift)

# Histograma para el parámetro de interés en la prueba de DF (el que acompaña y_{t-1})
histogram_rho_none = histogram_rho(df_none,
                                        "Rho estimado prueba Dickey-Fuller sin términos desterminísticos con N = 10000 y n = 100", 
                                        "Valores rho", "pink2"); histogram_rho_none

# Histograma del estadístico-t de DF sin trend y sin deriva
hist_DF_none = histogram_DF(df_none, 
                            "Estadístico Dickey-Fuller sin términos desterminísticos con N = 10000 y n = 100",
                            "Valores estadístico de DF", "yellow1"); hist_DF_none

# Valores críticos estadístico-t de DF solo con drift
critical_none = round(quantile(df_none$DF_statistic, c(0.1, 0.05, 0.01)), 2); critical_none

#---- Comparación pruebas de DF  ---- 

# Construcción de un data frame con todos los valores críticos para las diferentes especificaciones de la prueba de DF
critical_df = data.frame(sign_10 = double(), sign_5 = double(), sign_1 = double())
critical_df[1,] = critical_trend_drift
critical_df[2,] = critical_drift
critical_df[3,] = critical_none
names(critical_df)[1] = "10%"; names(critical_df)[2] = "5%"; names(critical_df)[3] = "1%"
# Data frame con todos los valores críticos para las diferentes especificaciones prueba de DF simulada
critical_df

# Contrucción de un data frame con todas las series simuladas (incluida la siulación de una normal estándar) para usar en la 
# realización de las gráficas de las densidades tanto de las diferentes distribuciones de cada uno de las especificaciones del estadístico t de DF
# como de la distribución normal 
df_todos = data.frame(DF_none = df_none$DF_statistic, DF_drift = df_drift$DF_statistic, 
                      DF_trend_drift = df_trend_drift$DF_statistic, Normal_estándar = rnorm(10000)) %>% 
            pivot_longer(cols = 1:4, names_to = "Distribuciones", values_to = "Valores_distribuciones") # la función pivot_longer es del paquete tidyr (paquete como dplyr que hacer parte del tidyverse)

# Gráfica de todas las funciones de densidad para cada uno de los estadísticos t de Dickey fuller para las diferentes especificaciones y la densidad para 
# una distribución normal
x11()
density_comparacion = df_todos %>% 
  ggplot(aes(x = Valores_distribuciones, color = Distribuciones)) +
  geom_density() + 
  theme_light() +
  ggtitle("Comparación de funciones de densidad de diferentes\nespecificaciones del estadísticos de DF vs Normal\n(todas simuladas)") +
  ylab("Densidades") +
  xlab("Valores diferentes distribuciones DF y distribución normal"); density_comparacion 
  
  
  
  
