library(tidyverse)
library(readxl)
library(xts)
library(forecast)

# Bootsttrapping an AR(p) process

obs =  350 # cambiar el valor para ajustar el número de datos de todas las series

# Simulación de un AR(3) ----

set.seed(12345)
yt = ts(arima.sim(model= list(order = c(3,0,0), ar=c(0.3, 0.2, 0.1)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1)))
mean(yt)

autoplot(yt)

# Bootstrapping an AR(3) process ----

boot_ar3_process = function(num_boots, serie_original, order_AR){
  df_coef_boot = tibble(ar1_coef = double(), ar2_coef = double(), ar3_coef = double(), intercept = double()) # Creating the tible for storing the AR coefficients
  # First estimation for storing the coefficients and residuals used for bootstrapping the series subsequently 
  reg1 = arima(serie_original, order = c(order_AR,0,0), include.mean = T, method = "ML") # Estimating the first model 
  reg_coef = reg1$coef # store the coefficientes since they will be used when estimaring the bootstrap series
  res = reg1$residuals # store the residuals of the first estimation since they will be used for bootstrapping the series
  series_size = length(serie_original) # length of the original series (used to generate bootstrap series of the same size)
  # Bootstrapping process
  for (boot in 1:num_boots){
    boot_res = sample(res, size = series_size + 50, replace = TRUE) # When the actual bootstrap takes place
    # Manually simulate the AR(3) process
    y = c(serie_original[1], serie_original[2], serie_original[3]) # initialize each bootstrapped yt 
    # The boostrapped series will have 50 more observations which its 50 first obs. will eventually be removed
    for (elem in 4:(series_size + 50)){
      y_new = reg_coef[4] + reg_coef[1] * y[elem - 1] + reg_coef[2] * y[elem - 2] + reg_coef[3] * y[elem - 3] + boot_res[elem] # create the bootstrapped yt series
      y = append(y, y_new) 
    }
    y = y[-c(1:50)] # Delete the first 50 observations to remove the initicial conditions effect
    reg_boot = arima(y, order = c(3,0,0), include.mean = TRUE, method = "ML") # Estimate the AR(3) for each bootstrapped series
    df_coef_boot[boot, ] = t(reg_boot$coef) # Fill the dataframes with the estimated coefficients of each bootstrapped sample
  }
  return(df_coef_boot) # returned the AR(3) coefficients obtained from the bootrapped sample
}

# Simulando 10000 series mediante bootstrapping se obtiene los 
# coeficeintes del AR(3) obtenidos por boots
# prueba = boot_ar3_process(num_boots = 10000, serie_original = yt, order_AR = 3)

# Notes: 
## Bootstrapping 10000 series from an AR(3) process is computationally demanding 
## it requires at least aprox. 5 minutes to execute the function

## The bootstrapped results will depend not on the coefs. of the actual data 
## generating process coefficients but on the coefs that results of the 
## estimation with the actual data


# Finding the mean and interval coefs. from an specific parameter 

parameter_info = function(coef_variable, conf_interval){
  ordenado = sort(coef_variable) # sort the vector so I can find the confidence intervals
  num_boots = length(coef_variable) # find the actual size of the vector (length)
  # encuentro la media y los intervalos de cofianza para el parámetro de interés
  mean_ordenado = mean(ordenado)
  conf_inf = ((1 - (conf_interval/100))/2) * num_boots
  conf_sup = num_boots - conf_inf
  intervals = c(ordenado[conf_inf], mean_ordenado, ordenado[conf_sup])
  return(intervals)
}

glimpse(prueba)

# Generate the confidence intervals for the AR coeficients
ar1_param = parameter_info(coef_variable = prueba$ar1_coef, conf_interval = 95)
ar2_param = parameter_info(coef_variable = prueba$ar2_coef, conf_interval = 95)
ar3_param = parameter_info(coef_variable = prueba$ar3_coef, conf_interval = 95)
