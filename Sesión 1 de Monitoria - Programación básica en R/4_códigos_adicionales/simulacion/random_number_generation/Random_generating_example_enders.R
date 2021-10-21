# Bibliotecas
library(numbers)

# Random generating process ----

# En este código se simula el método de generación de números aleatorios 
# que propone Enders en la sección 4.2 del manual suplementario
# utilizando la misma metodología

# Parameters
seed = 1
lambda = 9947
m = 732289
alpha = 67

gen_random_numbers = function(rep, seed, lambda, m, alpha){
  z = seed
  vect = c()
  for (number in 1:rep){
    z = mod(lambda * z + alpha, m)
    y = z/m
    vect = append(vect, y)
  }
  return(vect)
}

random_numbers = gen_random_numbers(100, seed, lambda, m, alpha)
plot(as.ts(random_numbers))

# Nota: Los valores en el Enders están erroneos
