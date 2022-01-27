##______________________________________________________________________________
##______________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2020-II
#               SESIÓN MONITORIA : Modelos de elección discreta
##______________________________________________________________________________
##______________________________________________________________________________

#Activar los paquetes que se van a utilizar
library(foreign);library(car); library(lmtest); library(stargazer);
library(wooldridge);library(dplyr);library(broom) 

### Funciones paquete Broom: 
# tidy
# glance
# augment

### Para profundizar más sobre el paquete broom
vignette("broom") 

### Para revisar qué funciones son compatibles con el paquete broom
vignette("available-methods")

#Cargar la base de datos
data<- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta")
help(mroz)
attach(data)

#Variables
#A data.frame with 753 observations on 22 variables:
#inlf: =1 if in lab frce, 1975
#hours: hours worked, 1975
#kidslt6: # kids < 6 years
#kidsge6: # kids 6-18
#age: woman's age in yrs
#educ: years of schooling
#wage: est. wage from earn, hrs
#repwage: rep. wage at interview in 1976
#hushrs: hours worked by husband, 1975
#husage: husband's age
#huseduc: husband's years of schooling
#huswage: husband's hourly wage, 1975
#faminc: family income, 1975
#mtr: fed. marg. tax rte facing woman
#motheduc: mother's years of schooling
#fatheduc: father's years of schooling
#unem: unem. rate in county of resid.
#city: =1 if live in SMSA
#exper: actual labor mkt exper
#nwifeinc: (faminc - wage*hours)/1000
#lwage: log(wage)
#expersq: exper^2

# Visualizar base de datos 
View(glimpse(mroz))

#########################################################
############# Modelo de probabilidad lineal #############
#########################################################

#Estimar un MPL para la probabilidad de que la mujer haya pertenecido a la fuerza laboral.
MPL=lm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,data = data); summary(MPL)

### Data frames del paquete broom: 

# Summary statistic para la regresión en formato de data frame
tidy_mpl = tidy(MPL); tidy_mpl 

# Data frame con info. adicional sobre la regresión
glance_mpl = glance(MPL); View(glance_mpl)  
help("glance")

# Data frame de datos expandido con desviación estándar poblacional estimada, residuales, fitted values y más argumentos
augment_mpl = augment(MPL); View(augment_mpl)  
help("augment")

#.hat	Diagonal of the hat matrix
#.sigma	Estimate of residual standard deviation when corresponding observation is dropped from model
#.cooksd	Cooks distance, cooks.distance()
#.fitted	Fitted values of model
#.se.fit	Standard errors of fitted values
#.resid	Residuals
#.std.resid	Standardised residuals

#Estimar el MPL con errores robustos a la heterocedasticidad p(1-p)
coeftest(MPL,vcov=hccm) # Heteroscedasticity-Corrected Covariance Matrix 


################
## Predicciones 

# Acuérdense de esta predicción porque va a ser utilizada también para los modelos probit y logit 

#Predicción para dos mujeres fuera de la muestra original
MPL.pred = list(nwifeinc=c(100,0),educ=c(5,17),exper=c(0,30),expersq=c(0,900),
                age=c(20,52),kidslt6=c(2,0),kidsge6=c(0,0)) 

predict(MPL,MPL.pred) 

############################################
# Haciendo predicciones con el paquete broom

# Características para dos mujeres fuera de la muestra
MPL.pred2 <- data.frame(nwifeinc=c(100,0),educ=c(5,17),exper=c(0,30),expersq=c(0,900),
                        age=c(20,52),kidslt6=c(2,0),kidsge6=c(0,0))

# predicción con broom para las dos mujeres fuera de la muestra
augment(MPL,newdata = MPL.pred2, type.predict="response")

# observen que el MPL predijo probabilidades menores a 0 y mayores a 1 

###############################################################
###################### Modelo logit ###########################
###############################################################

#Estimar un modelo LOGIT: "family" para que estime un GLM.
LOGIT = glm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,
           family = binomial(link = logit),data = data); summary(LOGIT)

# Comandos broom para 
tidy_logit = tidy(LOGIT);View(tidy_logit)
glance_logit = glance(LOGIT);View(glance_logit)   # inlcuye logLik 
augment_logit = augment(LOGIT);View(augment_logit)  # augmented data frame que incluye residuales y valores estimados como columnas 

# predict sin más argumentos calcular los z_ajustados = b_0 + b_1 * x_1 + b_2 * x_2 + ... + b_n * x_n (bs coeficientes estimados)
LOGIT.FIT = predict(LOGIT) #Valores ajustados de la función índice dentro de la muestra.
LOGIT.FIT

# predict con type="response" calcular P(Y = 1) = exp(z_ajustado)/(1 - exp(z_ajustado)) 
# que es la probabilidad de exito para los diferentes valores z_ajustados en el modelo para un modelo logit
# Existen 3 formas diferntes de hacerlo: 

# forma 1
LOGIT.pred = predict(LOGIT, type="response") 
LOGIT.pred

# forma2
LOGIT.pred_2 = plogis(LOGIT.FIT) #plogis me da la funcion acumulada de probabilidad para una variable aleatoria logit estandar
LOGIT.pred_2

# forma3
LOGIT.pred_3 = exp(LOGIT.FIT)/(1 + exp(LOGIT.FIT))
LOGIT.pred_3

# Como se observa las tres formas son equivalentes 

###############################################################################################
# predicción para el logit con el comando broom (para dos mujeres fuera de la muestra original)

# Características de las dos mujeres fuera de la muestra
LOGIT.pred2 <- data.frame(nwifeinc=c(100,0),educ=c(5,17),exper=c(0,30),expersq=c(0,900),
                        age=c(20,52),kidslt6=c(2,0),kidsge6=c(0,0))

# predicción con broom para las dos mujeres fuera de la muestra
# Fijense que para el modelo logit los valores predichos por el modelo ahora si tienen sentido al ser valores
# mayores a cero y menores a 1 y por ende pueden interpretarse como probabilidades
z = augment(LOGIT,newdata = LOGIT.pred2)  # para encontrar z 
p_exito = augment(LOGIT,newdata = LOGIT.pred2, type.predict="response") # para encontrar P(Y = 1) = exp(z)/ 1 - exp(z)
View(p_exito)

# Pseudo R^2 de McFadden

#Pseudo R^2 de McFadden para comparar dos modelos de elección discreta
# 1- (logaritmo verosimilitud modelo completo)/(logaritmo verosimilitud del modelo restringido con solo un intercepto)
# is defined as 1- L1/L0, where L0 represents the log likelihood for the "constant-only" model and L1 is the log likelihood for the full model with constant and regressors.

# La funcion loglik permite calcular la funcion log likelihood para el modelo LOGIT
logLik(LOGIT)

#Pseudo McFadden R^2 usando Residual deviance y NUll deviance
1 - LOGIT$deviance/LOGIT$null.deviance 

#Pseudo McFadden R^2 usando el log de la funx. de max. verosimilitud para el modelo completo y para el modelo
# solo con el intercepto
## loglik(LOGIT): funx. de max. verosimilitud modelo completo
## loglik(LOGIT): funx. de max. verosimilitud modelo solo intercepto
LOGIT_null <- glm(inlf~1, family = binomial, data = data)
1- logLik(LOGIT)/logLik(LOGIT_null)

#################################################################################
### Análsis/intepretación de resultados de un modelo logit a partir de odds ratio

# Los odds ratio tienen sentido solo en el modelo logit 

#Sea Mi = Pr(yi=1|X) = exp(z)/1+exp(z), entonces se define el odd-ratio como Mi/1-Mi = exp(z)
### z es b_0 + b_1 * x_1 + b_2 * x_2 + ... + b_n * x_n
### odd-ratio se interpreta como el cociente de la probabilidad de que el evento ocurra sobre que no ocurra el evento
#Si linealizamos el ODD lo interpretamos como un modelo log-lin   log(Mi/1-Mi) = z
#ODDs Ratios para el modelo LOGIT: ODDs = p.éxito/p.fracaso. e.g. 0.75/0.25, la probabilidad de éxito es de 3 a 1

##################################################################################
## Ejemplo de odds ratio para dos valor predichos para mujeres fuera de la muestra

# Se va usar las predicciones que se encontraron 
# de las dos mujeres fuera de la muestra en LOGIT.pred2 

odds=exp(z$.fitted);odds
 
############################
## Cociente entre odd ratios exp(betas)

# El cociente entre odds ratio se puede interpretar como:
### qué tanto es más probable que se dé el evento de éxito cuando ocurre un aumento en 1 unidad 
### para un determinado regresor manteniendo los demás regresores fijos

# En otras palabras, el cociente de odds ratio mide cuánto es más probable que se dé la alternativa 
# de éxito al agregar una unidad de un regresor (manteniendo los demás constantes) frente al caso  
# base de no agregar la unidad adicional al regresor

# Por tanto, para cada regresor es posible calcular un cocientre odds ratio

# cociente entre odds ratio 
c.odds=exp(coefficients(LOGIT));c.odds    # Para cada parámetro estimado se puede calcular el cociente entre odds ratio

log.odds= coefficients(LOGIT);log.odds 
stargazer(c.odds, log.odds, type="text") #Un año más de educación hace un 22% más probable entrar al mercado laboral que no entrar.

################################################################
###################### Modelo probit ###########################
################################################################

#Estimar un modelo PROBIT. 
PROBIT = glm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,
             family=binomial(link=probit),data=data)
summary(PROBIT)

# Comandos broom para probit
tidy_probit =tidy(PROBIT)
glance_probit =glance(PROBIT)   # inlcuye logLik 
augment_probit = augment(PROBIT);View(augment_probit)  # augmented data frame que incluye residuales y valores estimados como columnas 

# predict sin más argumentos calcular los z_ajustados = b_0 + b_1 * x_1 + b_2 * x_2 + ... + b_n * x_n (bs coeficientes estimados)
PROBIT.FIT = predict(PROBIT) #Valores ajustados dentro de la muestra.
PROBIT.FIT

# predict con type="response" calcula la probabilidad de éxito para cada observación 
# es decir, se calcula la probabilidad de exito para los diferntes valores z_ajustados en el modelo para un modelo probit
# Existen 2 formas diferntes de hacerlo: 

# forma1
PROBIT.pred = predict(PROBIT, type="response")
View(PROBIT.pred)

# forma2
PROBIT.pred_2 = pnorm(PROBIT.FIT) #pnorm me da la funcion acumulada de probabilidad para una variable aleatoria normal estandar

# No se agrega la forma integral porque seria la integral asociada a la funciona de distribucion acumulada de una normal

###############################################################################################
# predicción para el probit con el comando broom (para dos mujeres fuera de la muestra original)

# Características de las dos mujeres fuera de la muestra
PROBIT.pred2 <- data.frame(nwifeinc=c(100,0),educ=c(5,17),exper=c(0,30),expersq=c(0,900),
                           age=c(20,52),kidslt6=c(2,0),kidsge6=c(0,0))

# predicción con broom para las dos mujeres fuera de la muestra
# Fijense que para el modelo probit los valores predichos por el modelo ahora si tienen sentido al ser valores
# mayores a cero y menores a 1 y por ende pueden interpretarse como probabilidades
z_norm = augment(PROBIT,newdata = PROBIT.pred2)  # para encontrar z 
p_exito_norm = augment(PROBIT,newdata = PROBIT.pred2, type.predict="response") # para encontrar P(Y = 1|X)

#Pseudo McFadden R^2 usando Residual deviance y NUll deviance
logLik(PROBIT)
1 - PROBIT$deviance/PROBIT$null.deviance  #los modelos son estadisticamente equivalentes.

#Pseudo McFadden R^2 usando el log de la funx. de max. verosimilitud para el modelo completo y para el modelo
# solo con el intercepto
## loglik(LOGIT): funx. de max. verosimilitud modelo completo
## loglik(LOGIT): funx. de max. verosimilitud modelo solo intercepto
PROBIT_null <- glm(inlf~1, family = binomial, data = data)
1- logLik(PROBIT)/logLik(PROBIT_null)

##########################################################
###### Efectos parciales para modelo logit y probit ######
##########################################################

# Recuerden que el término efecto parcial es equivalente al término efecto marginal (son sinónimos)

####################################
# Average partial/marginal Effects # 
####################################

# Exiten principalmente dos formas de calcular efectos marginales: 
### partial effect at the average (PEA)
#### 1. Calculo el promedio de todas las variables o la moda (dependiendo del tipo de variable que estoy 
####    trabajando)
#### 2. Frente a dicho resultado calculo el efecto marginal 

### average partial effect (APE)
#### 1. Cálculo el efecto marginal de cada una de las observaciones
#### 2. Cálculo el promedio de cada uno de los efectos marginales cálculados en la primera etapa 

# En la práctica es más común calcular el APE dado que su interpretación es más directa y es más fácil 
# compararlo que el MPL por ejemplo

#################################################################
# Calculando los efectos marginales utilizando el comando margins 

# El comando margins me permite calcular los efectos marginales de manera automática

library(margins)

# Nota: Muy importante colocar I(exper^2) para agregar la experiencia al cuadrado en el modelo 
# dado que así se garantiza que los efectos marginales queden bien cálculados

# Modelo Logit
LOGIT = glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,
            family = binomial(link = logit),data = data); summary(LOGIT)
# Modelo Probit
PROBIT = glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,
             family=binomial(link=probit),data=data); summary(PROBIT)

#####################################
# Para calcular el APE usando margins 
margins(LOGIT, type = "response") # Es necesario colocar type=response para que calculen los efectos marginales
margins(PROBIT, type = "response") 

#####################################
# Para calcular el PEA usando margins

# fijense que lo único que hay que proveer es un data.frame con las variables evaluadas en sus 
# medias muestrales
margins(LOGIT, type = "response", data.frame(nwifeinc = mean(nwifeinc), educ = mean(educ), age = mean(age), exper = mean(exper), kidslt6 = mean(kidslt6),kidsge6 = mean(kidsge6))) 
margins(PROBIT, type = "response", data.frame(nwifeinc= mean(nwifeinc), educ = mean(educ), 
                                             age = mean(age), exper = mean(exper), kidslt6 = mean(kidslt6),
                                             kidsge6 = mean(kidsge6))) 
 
# Para calcular el 2do efecto marginal que propone la profe en la diapositiva 22 de D4-LogityProbit.PDF

# Pasar de kidslt6 = 0 a kidslt6 = 1 y ver el efecto marginal de dicho cambio 
# (me interesa el efecto marginal causado por un cambio en una unidad de kidslt6)
# Además se usa la media de nwifeinc, la media de educ, la media de exper, la media de expersq
# la media de age, kidsge6 es 1  

# De manera manual para logit
p_1_logis = plogis(0.425 - 0.0213 * mean(nwifeinc) + 0.221 * mean(educ) 
            + 0.206 * mean(exper) - 0.0032 * mean(expersq) - 0.0880 * mean(age) + 0.0601 - 1.44 )

p_2_logis = plogis(0.425 + -0.0213 * mean(nwifeinc) + 0.221 * mean(educ) 
            + 0.206 * mean(exper) - 0.0032 * mean(expersq) -0.0880 * mean(age) + 0.0601)

marg_effect_1_logis = p_1_logis - p_2_logis
marg_effect_1_logis

# De manera manual para probit  

p_1_norm = pnorm(0.270 - 0.012 * mean(nwifeinc) + 0.131 * mean(educ) 
             + 0.123 * mean(exper) - 0.0019 * mean(expersq) - 0.053 * mean(age) + 0.036 - 0.868*1 )

p_2_norm = pnorm(0.270 - 0.012 * mean(nwifeinc) + 0.131 * mean(educ) 
                 + 0.123 * mean(exper) - 0.0019 * mean(expersq) - 0.053 * mean(age) + 0.036)

marg_effect_1_norm = p_1_norm - p_2_norm

# Usando margins:
margins(LOGIT, type = "response", data.frame(nwifeinc=mean(nwifeinc), educ = mean(educ), 
                                             age = mean(age), exper = mean(exper), kidslt6 = 0, kidsge6 = 1))
margins(PROBIT, type = "response", data.frame(nwifeinc=mean(nwifeinc), educ = mean(educ), 
                                             age = mean(age), exper = mean(exper), kidslt6 = 0, kidsge6 = 1))

# Para calcular el 2do efecto marginal que propone la profe en la diapositiva 23 de D4-LogityProbit.PDF

# Pasar de exper = 10 a exper = 11 y ver el efecto marginal de dicho cambio 
# (me interesa el efecto marginal causado por un cambio en una unidad de exper)
# Además se usa la media de nwifeinc, la media de educ, la media de expersq
# la media de age, kidsge6 es 1, kidslt6 es 0   

# De manera manual para logit
p_1_logis = plogis(0.425 - 0.0213 * mean(nwifeinc) + 0.221 * mean(educ) 
             + 0.206 * 11 - 0.0032 * 11^2 - 0.0880 * mean(age) + 0.0601)

p_2_logis = plogis(0.425 + -0.0213 * mean(nwifeinc) + 0.221 * mean(educ) 
             + 0.206 * 10 - 0.0032 * 10^2 -0.0880 * mean(age) + 0.0601)

marg_effect = p_1_logis - p_2_logis

# De manera manual para probit
p_1_norm = pnorm(0.270 - 0.012 * mean(nwifeinc) + 0.131 * mean(educ) 
                 + 0.123 * 11 - 0.0019 * 11^2 - 0.053 * mean(age) + 0.036)

p_2_norm = pnorm(0.270 - 0.012 * mean(nwifeinc) + 0.131 * mean(educ) 
                 + 0.123 * 10 - 0.0019 * 10^2 - 0.053 * mean(age) + 0.036)

marg_effect = p_1_norm - p_2_norm;marg_effect

# Usando margins:
margins(LOGIT, type = "response", data.frame(nwifeinc=mean(nwifeinc), educ = mean(educ), 
                                             age = mean(age), exper = 10, kidslt6 = 0, kidsge6 = 1))
margins(PROBIT, type = "response", data.frame(nwifeinc=mean(nwifeinc), educ = mean(educ), 
                                             age = mean(age), exper = 10, kidslt6 = 0, kidsge6 = 1))

# Como pueden observar los cálculos realizados de manera manual así como de manera automática usando margins
# son muy cercanos 

#_______________________________________________________________________________________
#OTRO EJEMPLO: PROBABILIDAD QUE SE OTORGUE UN CRÉDITO SEGÚN VARIABLES SOCIOECONÓMICAS
#________________________________________________________________________________________
#Cargamos la base de datos
data("loanapp")
help("loanapp")
attach(loanapp)

#Descripción de las variables
#occ: occupancy
#loanamt: loan amt in thousands
#action: type of action taken
#msa: msa number of property
#suffolk: =1 if property in suffolk co.
#appinc: applicant income, $1000s
#typur: type of purchaser of loan
#unit: number of units in property
#married: =1 if applicant married
#dep: number of dependents
#emp: years employed in line of work
#yjob: years at this job
#self: =1 if self employed
#atotinc: total monthly income
#cototinc: coapp total monthly income
#hexp: propose housing expense
#price: purchase price
#other: other financing, $1000s
#liq: liquid assets
#rep: no. of credit reports
#gdlin: credit history meets guidelines
#lines: no. of credit lines on reports
#mortg: credit history on mortgage paym
#cons: credit history on consumer stuf
#pubrec: =1 if filed bankruptcy
#hrat: housing exp, percent total inc
#obrat: other oblgs, percent total inc
#fixadj: fixed or adjustable rate?
#term: term of loan in months
#apr: appraised value
#prop: type of property
#inss: PMI sought
#inson: PMI approved
#gift: gift as down payment
#cosign: is there a cosigner
#unver: unverifiable info
#review: number of times reviewed
#netw: net worth
#unem: unemployment rate by industry
#min30: =1 if minority pop. > 30percent
#bd: =1 if boarded-up val > MSA med
#mi: =1 if tract inc > MSA median
#old: =1 if applic age > MSA median
#vr: =1 if tract vac rte > MSA med
#sch: =1 if > 12 years schooling
#black: =1 if applicant black
#hispan: =1 if applicant Hispanic
#male: =1 if applicant male
#reject: =1 if action == 3
#approve: =1 if action == 1 or 2
#mortno: no mortgage history
#mortperf: no late mort. payments
#mortlat1: one or two late payments
#mortlat2: > 2 late payments
#chist: =0 if accnts deliq. >= 60 days
#multi: =1 if two or more units
#loanprc: amt/price
#thick: =1 if rep > 2
#white: =1 if applicant white

#Estimamos los tres modelos
MPL1 = lm(approve~white+male+married+unem+dep+appinc+sch+mortno+mortlat1+mortlat2, data=loanapp)
PROBIT1 = glm(approve~white+male+married+unem+dep+appinc+sch+mortno+mortlat1+mortlat2,family = binomial(link = probit),data=loanapp)
LOGIT1 = glm(approve~white+male+married+unem+dep+appinc+sch+mortno+mortlat1+mortlat2,family = binomial(link = logit),data=loanapp)
stargazer(MPL1,PROBIT1,LOGIT1, type="text")

#calculamos las probabilidades ajustadas
MPL1.pred = fitted.values(MPL1) # recordar que en el MPL los fitted se pueden interpretar directamente como probabilidades
PROBIT1.pred= predict(PROBIT1,type="response")
PROBIT1.fit= predict(PROBIT1)
LOGIT1.pred= predict(LOGIT1,type="response")
LOGIT1.fit= predict(LOGIT1)
summary(MPL1.pred);summary(PROBIT1.pred);summary(LOGIT1.pred)

#Calculamos el Pseudo-R2 de McFadden
PROBIT1_null <- glm(approve~1, family = binomial, data = loanapp)
LOGIT1_null <- glm(approve~1, family = binomial, data = loanapp)
cbind(1- logLik(PROBIT1)/logLik(PROBIT1_null),1- logLik(LOGIT1)/logLik(LOGIT1_null))

# Odds Ratios modelo LOGIT (recordar que los odds ratio solo tienen sentido para modelos LOGIT)
## Se calculan los odds ratio para cada observación
odds1 = exp(LOGIT1.fit)

#Cociente de Odds Ratios modelo LOGIT
c.odds1=exp(coefficients(LOGIT1));c.odds1
log.odds1= coefficients(LOGIT1);log.odds1
stargazer(c.odds1, log.odds1, type="text")  

##################################################
#Cálculo de efectos parciales o efectos marginales

# APE (Average Partial Effect)
margins(PROBIT1, type = "response") # Es necesario colocar type=response para que calculen los efectos marginales
margins(LOGIT1, type = "response") 

