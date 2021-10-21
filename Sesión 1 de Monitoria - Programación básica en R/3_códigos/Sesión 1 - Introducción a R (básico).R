##_______________________________________________________________________________________________________
##_______________________________________________________________________________________________________
             #UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
                                 #ECONOMETRIA II - 2021-I
                        #SESIÓN #1 MONITORIA : INTRODUCCIÓN A RSTUDIO
##_______________________________________________________________________________________________________
##_______________________________________________________________________________________________________

#---- 1. INTRODUCCIÓN A R. ----

remove(list = ls()) # Codigo para limpiar el Environment 


#1.1. Operaciones aritméticas

# Número PI
pi
pi^2 

#Valor Absoluto:  abs(#)
abs(-6.8)

# Redondeo: round(#)
round(7.739032)

# Redondeo con "n" decimales: round(#,n)
round(7.7390032,3)

# Eliminar decimales: trunc(#)
trunc(9.518190)

# Raíz cuadrada: sqrt(#)
sqrt(81)

# Funciones trigonométricas: sin(#);cos(#);tan(#)
sin(1)
sin(pi)
cos(1)
tan(0.5)

#Logaritmos.

#Logaritmo natural: log(#)
log(10)
#Logaritmo en base n: log(#,n) - logn(#)
log(100,10)  
log10(100)

# Operaciones compuestas
round(log10(abs(-97)),3)

#---- 2. CREACIÓN DE OBJETOS. ----

#Crear el objeto.
z = 4

#Imprimir el objeto.
z

# Crear un objeto e imprimirlo a la vez.
x=3; x

# Operaciones con los objetos creados.
z + x; z*x; z/x; z-x

#Crear un objeto usando operaciones entre objetos.
y = (x-z);y

#Crear vectores: c()
Codigos = c(10,25,6,8,90,102); Codigos              
Colores = c("Rojo", "Azul", "Morado"); Colores

#Crear un vector secuencia 1 a 1.
Dias = 0:100; Dias

#Crar un vector secuencia que avance cada j unidades: seq(a,b,j)
Medicion = seq(1,20, 0.5); Medicion

#Crear un vector donde los objetos se repiten n veces: rep(objeto,n)
Numerico = rep(1,30)
Categorico = rep("mujer", 10)
Notas = c(1,1.2,1.6,4.5,4.7,3.6,3.9,2.5,5,5,3.2,1.9,0.5,
          0.6,0.5,3.5,0.2,0.5,4,4,4,4,4.6,5,5,5,4.6,4.8)
Dummy = c(rep(0,5), rep(1,10), rep(0,5))

#Creación de matrices: matrix(a:b,nrow = 4, byrow =TRUE) o matrix(a:b,nrow = 4, byrow = FALSE)

MAT1=matrix(1:10,nrow = 5,byrow = TRUE);MAT1 
MAT2=matrix(10:19,ncol=5, nrow = 2, byrow = FALSE);MAT2

#Creación de matrices a partir de vectores: cbind(a,b) o rbind(a,b)
a = c(1,4,5,7,9,7,10,6,5,7,9)
b = c(3,4,6,8,2,3,11,1,2,5,6)
MAT3 = cbind(a,b)
MAT4 = rbind(a,b)


#---- 3.Operaciones entre vectores ----

#Número de objetos en el vector: length(vector)
length(a) 
length(Colores)

#Tipo de objeto en el vector: class(vector)
class(b)
class(Colores)

# clases de vectores

vect_char = c("primero", "segundo", "tercero")
vect_num = c(1, 2, 3)
vect_logic = c(TRUE, FALSE, F, T)

# transformar un vector de un tipo a otro tipo 
## por ejemplo transformar un vector de tipo character en un vector de tipo factor
vect_fact = as.factor(vect_char)

#Sumar/Restar un número j a los elementos del vector: a + j
j = 5

a + j 

#Multiplicar un n?mero j a los elementos del vector: b*j
b*j 

#Aplicar una potencia n a los elementos del vector: a^n
a^3

#Aplicar logaritmo a los elementos del vector: log(a) ? log(b,n)
log(a)
n = 2
log(b,n)

#Multiplicar todos los elementos de un vector: prod(a)
prod(a)

#Sumar todos los elementos de un vector: sum(b)
sum(b)

#Máximo y Mínimo de un Vector: min(a), max(a)
min(a)
max(a)

#Media del vector: mean()
mean(Notas) 

#Median del vector: median()
median(Notas)

#Desviaci?n est?ndar del vector: sd()
sd(Notas)

#Varianza del vector: var()
var(Notas)

#Frecuencia Acumulada de los datos del vector: cumsum()
cumsum(a)

#cuartiles del vector: quantile()
quantile(Notas)

#Estadistica descriptiva y dispersión del vector
summary(Notas) #Sesgada o con cola hacia la izquierda

#Elementos únicos del vector: unique()
unique(Notas)

#Ordenar elementos de menor a mayor: sort()
sort(Notas) 

#Posición ordenada de menor a mayor
order(Notas)

#Ordenar elementos de mayor a menor: sort(,decreasing=TRUE)
sort(Notas,decreasing = TRUE)       

#Seleccionar ciertos elementos del vector por posición 
a[2];a[3];a[4]
a[c(2,3,4)]

#Eliminar ciertos elementos del vector por posición
a[-c(1,5)]  

#---- 4. Operaciones entre matrices ----

#Sumar una constante j a la matriz
MAT1 + 10

#Suma de matrices
MAT3 + t(MAT4)

#Multiplicar una constante j a la matriz
MAT3*10

#Producto de matrices
dim(MAT3)
dim(MAT4)
MAT3%*%MAT4

#Matriz Transpuesta: t() 
t(MAT1) 

#Matriz identidad: diag()
diag(10)

#Matriz diagonal: diag()
Mat_diag = diag(1:10)

#Dimensiones de una matriz: dim()
dim(MAT3)

#Inversa de una matriz: solve()
solve(Mat_diag)

#Determinante de la matriz: det()
det(Mat_diag)

# Que pasa con la inversa de una matriz no cuadrada: solve()
#solve(MAT2)

#Que pasa con el determinante de una matriz no cuadrada: det()
#det(MAT1)

#Mostrar el elemento ij de la matriz: MAT[i,j]
MAT2[2,1]
MAT2[1,2]

#Mostrar los elementos de la fila i: MAT[i,]
MAT3[1,]

#Mostrar los elmentos de la columna j: MAT[,j]
MAT3[,1]

#Agregar nombres a las filas y columnas de una matriz
rownames(MAT1) = c("a","b","c","d","e")
colnames(MAT1) = c("f","g")
MAT1

#---- 5. Operaciones lógicas ----

#Elementos mayores o iguales a j en el vector
a>=4

#Elementos menores o iguales a j en el vector
a<=4
length(a[a<=4]) #Cuántos elementos satisfacen la condición

#Elementos iguales a j en el vector
a==4

#Elementos diferentes a i en el vector
b!=6

#Dos afirmaciones verdaderas: "&"
a>=1 & b==3

#Al menos una es cierta: "|"
a>=1 | b==3

#Negación: "!"
!a<0 #a no es menor que cero

#---- 6.Crear funciones en R ----

#Crear una función definiendo los argumentos y la expresión
funcion_simple = function(x,y,z,j=3){
 w = x + y + z + j
 return(w)
}

#Evaluar la función para ciertos valores de los argumentos
funcion_simple(2,3,10)
funcion_simple(2,3,10,1)
funcion_simple(c(1:10),c(1:10),c(1:10))

#---- 7. Manejo de ggplot2 y dplyr ----

# Importar conjunto de paquetes del tidyverse (incluye ggplot y dplyr)
library(tidyverse)

#view(mtcars)
glimpse(mtcars)

# grafica de ggplot2
grafica = mtcars %>% 
  ggplot(aes(x = mpg, y = hp)) +
  geom_point(aes(color = factor(cyl))) +
  geom_smooth(method='lm', formula= y~x, se = F) +
  theme_classic()

grafica


# manipulación de base de datos usando dplyr

# Creación de variables apartir de variables antiguas
manip1 = mtcars %>%  
  mutate(mult = mpg * cyl, sqr_mpg = mpg^2) 

# Filtrar bases de datos a partir de los valores de una variable 
manip2 = mtcars %>% 
  filter(cyl > 4)

# Para sacar estadística descriptiva de una base de datos
manip3 = mtcars %>% 
  group_by(cyl) %>% 
  summarize(n  = n()) 

#---- 8. Opcional: Estructuras de control y ciclos en R ----

# ciclo while
i = 0
while (i<5){
  print(i)
  i = i + 1    # condición de actualización
}

i

# ciclo for
for (j in 1:10){
  print(j)
}

# esctructura de flujo: if-else (condicionales)

n1 = 5
n2 = 6


if (n2 %% 2 == 0){
  print("Es un número par")
}else{
  print("Es un número impar")
}

#---- 9. Tarea: Analizar y entender la siguiente función ----

# Función más elaborada: construya una función que imprima si la suma de los elementos
# de un vector es par o impar y que retorne la suma de los elementos del vector

funcion_2 = function(vect)
{
  suma = 0
  for (i in vect)
  {
    suma = suma + i    
  }
  if (suma %% 2 == 0){
    print("La suma de los elementos del vecotr es par")
  }else{
    print("La suma de los elementos del vecotr es impar")
  }
  return(suma)
}

vect_prueba = c(0, 2, 4, 5, 7)
respuesta = funcion_2(vect_prueba)
respuesta
