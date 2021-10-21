# Simulación de Monte Carlo Enders: suma de dos dados jutstos (ejemplo pag. 204) ----

# La idea es simular la probabilidad de obtener un determinado valor
# dado la suma de los resultados de dos dados justos

library(tidyverse)

# función que premite realizar la simulación de Monte Carlo
monte_carlo = function(rep){
  # set_number_dado simular el lanzamiento de un dado
  set_number_dado = function(random){
    number = 0
    if (random <= 1/6){
      number = 1
    }else if(1/6 < random &  random <= 2/6){
      number = 2
    }else if(2/6 < random & random <= 3/6){
      number = 3
    }else if(3/6 < random & random <= 4/6){
      number = 4
    }else if(4/6 < random & random <= 5/6){
      number = 5
    }else{
      number = 6
    }
    return(number)
  }
  # vector que almacena los resultados de la simulación de monte carlo
  vect = c()
  # simula rep veces el lanzamiento de dos dados y sumo sus resultados
  for (number in 1:rep){
    # genero los dos lanzamientos de los dos datos
    random1 = set_number_dado(runif(1)) # la distribución que se escogió para hacer la simulación fue una distribución uniforme (para garantizar tener un dado justo)
    random2 = set_number_dado(runif(1))
    vect = append(vect, random1 + random2) 
  }
  df = as_tibble(vect) %>% 
    mutate(value = as.factor(value))
  return(as_tibble(vect))
}

# Se realiza la simulación de Monte Carlo
prueba = monte_carlo(100000)# se repite 100000 veces el lanzamiento de los dos dados para garantizar alcanzar la distribución teórica

# Histograma que muestra que se alcanzan la distribución teórica propuesta por Enders
# en el ejemplo de la pag. 204 
graph = prueba %>% 
  ggplot(aes(x = value)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme_light(); graph

# El historgrama representa una distribución empírica igualita a la distribución teórica discreta para la suma del resultado de los dos dados
