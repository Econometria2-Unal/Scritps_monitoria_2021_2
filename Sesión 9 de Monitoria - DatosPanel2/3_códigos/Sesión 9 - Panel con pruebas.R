##______________________________________________________________________________
##______________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECON?MICAS
#                         ECONOMETRIA II - 2021-I
#                 SESIÓN #10 MONITORIA : Modelos para datos panel.
##______________________________________________________________________________
##______________________________________________________________________________

rm(list = ls()) #limpiar el enviroment 

#Paquetes 
# install.packages("plm")         #Panel linear models 
# install.packages("gplots")      #Tools for plotting data 
# install.packages("stargazer")   #Tablas más estéticas 
# install.packages("haven")       #Importar datos 
# install.packages("sandwich")    #Estimador de erores robustos 
# install.packages("lmtest")      #Provee herramientas para hacer inferencia estadística para modelos paramétricos lineales
# install.packages("tseries")     
# install.packages("wooldridge")  # Contiene data sets con los que se pueden realizar ejercicios econométricos
# install.packages("tidyverse")
 
library(plm);library(gplots);library(stargazer)
library(haven);library(sandwich);library(lmtest)
library(tseries);library(wooldridge);library(tidyverse)

# Nota: Foreign es una biblioteca que es muy freuentemente usada para importar bases
#       de datos de stata (.dta es el archivo de base de datos de stata)
#       No obstante, dicho paquete genera inconvenientes a la hora de importar cierto
#       bases de datos provenientes de ciertas versiones de stata
#       El paquete haven es mucho más general que el paquete foreign y no genera 
#       inconvenientes a la hora de importar bases de datos tipo .dta. Por tanto, 
#       al igual que en el presente script, se recomienda usar la función read_dta a la
#       hora de trabajar bases de datos de stata. 

#Base de datos: Wooldridge 14.4 ¿Ha cambiado la educación a lo largo del tiempo?
Education=read_dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta")  
attach(Education)
# Manera de visualizar toda la base de datos desde R
View(Education)
# Manera compacta de ver una base de datos
glimpse(Education)

#Tratar la base de datos como un panel de datos
panel.Education = pdata.frame(Education, index = c("nr","year"))
help("pdata.frame")

#Estructura del codigo ---------------------------------------------------#
#Nombre <- pdata.frame(Base_datos, index=c("Var_indivuo","Var_tiempo"))
#-------------------------------------------------------------------------#

#Para conocer las dimensiones del panel
pdim(panel.Education)

#Para determinar si las variables cambian a lo largo del tiempo
pvar(panel.Education)

#Distribución de la población.
plotmeans(lwage ~ married, main="Distribución de la población", data=panel.Education)

#Definición de las variables
exper2 = expersq
panel.Education$yr = factor(panel.Education$year)#Una Dummy para cada año de la muestra

#Estimación MCO combinados -E.F.- E.A.----------------------------------------------------

#Mínimos cuadrados combinados.
Pooled = plm(lwage~educ+black+hisp+exper+exper2+married+union+yr,
             data=panel.Education, model="pooling")
summary(Pooled)

#Modelo de efectos aleatorios. 
Random = plm(lwage~educ+black+hisp+exper+exper2+married+union+yr, 
             data=panel.Education , model="random")
summary(Random)

#Modelo de efectos fijos.
Fixed = plm(lwage~exper2+married+union+yr,
            data=panel.Education, model="within")
summary(Fixed)

#Modelo de primeras diferencias.
FD = plm(lwage~exper2+married+union+yr,
            data=panel.Education, model="fd")
summary(FD)

#Presentación de resultados.
stargazer(Pooled,Random,Fixed,FD, type="text",           
          column.labels=c("OLS","RE","FE", "PD", "BT"),keep.stat=c("n","rsq"))

## NOTA ACLARATORIA SOBRE BETWEEN ------------------------------------------------## 
## El modelo between en plm realiza una transformación diferente a la que vieron  
## en clase, pues tiene en cuenta las variaciones que puedan existir entre grupos 
## de individuos. Una anotación importante es que con este tipo de modelos se
#pierden obervaciones. 

## Mientras que el modelo de efectos fijos o "within" tiene en cuenta 
## las variaciones de los sujetos y no entre sujetos. 

## En otras palabras, "within" usa la media de cada individuo, mientras que "between" 
## usa la media de todos los datos para un año especifico.

## Para evaluar efectos especificos en los modelos que estimen con PLM, puden usar 
## el argumento " effect" y este permite "individual" , "time", "twoways"
## (efectos individuales y de tiempo) y "nested" (anidados). Por defectos PLM usa "twoways"
##---------------------------------------------------------------------------------------##

#Selección del modelo ---------------------------------------------------------------------

#¿es mejor Efectos fijos o Efectos Aleatorios?
#Test de Hausman para comparar EF vs EA
phtest(Fixed, Random) #Ho: Los modelos son equivalentes estadisticamente, por eficiencia elijo EA. 

#Test de Primeras diferencias de Wooldridge para comparar EF vs PD
pwfdtest(lwage~exper2+married+union+yr, data=panel.Education,h0= "fe") #H0 = corr(Uit,Uit-1) = 0
pwfdtest(lwage~exper2+married+union+yr, data=panel.Education,h0= "fd") #H0 = errores diferenciados no correlacionados
#La prueba no es concluyente, tanto los errores diferenciados como sin diferenciar tienen correlación serial. 

#Pooled VS Efectos Aleatorios
#Prueba de Multiplicadores de Lagrange de Breusch-Pagan para E.A
plmtest(Pooled,type = "bp")  #Ho:Mejor Pooled porque var(ai)=0
#H1:Se prefiere EA 

#Prueba Breush-Pagan ( Ho: Homocedasticidaad) 
bptest(Pooled) # si p-value>5% posiblemente es Pooled  

# ¿El efecto es individual, temporal, o ambos?
plmtest(Pooled,"time","bp")     #Ho:Efectos temporales no significativos 
plmtest(Pooled,"individual","bp")     #Ho:Efectos indivuduales no significativos
plmtest(Pooled,"twoways","bp")     #Ho:Efectos temporales e individuales no significativos


#Validación de supuestos------------------------------------------------------------------

#Prueba de heterocedasticidad
bptest(Pooled);bptest(Random);bptest(Fixed); bptest(FD)

#Test Breusch-Godfrey para autocorrelaci?n de orden p
bgtest(Pooled);bgtest(Random);bgtest(Fixed);bgtest(FD)

#Breusch-Pagan LM test for cross-sectional dependence in panels
pcdtest(Pooled,test = "lm");pcdtest(Random,test = "lm");pcdtest(Fixed,test = "lm");#pcdtest(FD,test = "lm")      

#Correci?n de correlaci?n serial para EF.
# MCOV=vcovHC.plm(Fixed, method=c("arellano"))          
MCOV1=vcovHC(Fixed, method="arellano")
# coeftest(Fixed,MCOV)
coeftest(Fixed,MCOV1)
help("vcovHC.plm")
         # Arellano : Soluciona heterosedasticidad y autocorrelacion serial 
         # White1 - Whit 2 : Soluciona heteroscedaticidad. 

#Análisis de Normalidad.
hist(residuals(Fixed))

qqnorm(residuals(Fixed))
qqline(residuals(Fixed))

library(car)
Residuales=residuals(Fixed)
qqPlot(Residuales)

#Es deseable que se ajusten a la linea de tendencia para cumplir normalidad 

jarque.bera.test(residuals(Fixed))  

#OTRO EJEMPLO:----------------------------------------------------------------------------

#En este ejercicio se utiliza la base de datos JTRAIN.RAW para determinar el efecto del
#subsidio a la capacitaci?n laboral en las horas de capacitación por empleado. El modelo 
#básico para los tres años es: 
#hrsemp~ Bo + S1d88 + S2d89+ B1grant + B2grant_1 + B3lemploy + ai + uit

#Utilizaremos la base de datos Jtrain
data("jtrain")
attach(jtrain)

##Definimos el objeto como un panel de datos
panel.jtrain = pdata.frame(jtrain, index = c("fcode","year"))

#Analizamos las caracteristicas de 
View(jtrain)
pdim(panel.jtrain) 
pvar(panel.jtrain)

#Creamos la dummy para cada uno de los a?os
years <- factor(panel.jtrain$year)  

# Modelo M?nimos cuadrados combinado
Pool.jtrain = plm(hrsemp~grant + grant_1 + lemploy,
                  data=panel.jtrain, model="pooling", effect = "twoways")

#(MCO Combinados + Time effect + Individual effects)
Pool.E1 =plm(hrsemp~ grant + grant_1 + lemploy + factor(year)+factor(fcode),
            data=panel.jtrain, model="pooling")

Pool.E2= lm(hrsemp~ grant + grant_1 + lemploy + factor(year)+factor(fcode), 
             data = panel.jtrain)

stargazer(Pool.jtrain, Pool.E1, Pool.E2, 
          keep = c("grant", "gran_1", "lemploy","constant"),type = "text", style = "AER")
# -----------------------------------------------------------------------------#
# si se dan cuenta estimar MCOC incluyendo  efectos de tiempo e individuales se puede 
# realizar con el comando plm y lm y los resultados son equivalentes.
# -----------------------------------------------------------------------------#

#Estimamos el modelo de EF, EA y PD

EF.jtrain = plm(hrsemp~years+ grant + grant_1 + lemploy,
                    data=panel.jtrain, model="within") 

PD.jtrain = plm(hrsemp~years+ grant + grant_1 + lemploy,
                data=panel.jtrain, model="fd")

EA.jtrain = plm(hrsemp~years+ grant + grant_1 + lemploy,
                data=panel.jtrain, model="random")

#Resultados
summary(EF.jtrain); summary(PD.jtrain); summary(EA.jtrain)

stargazer(EF.jtrain,PD.jtrain,EA.jtrain,type="latex", 
          column.labels=c("Efectos Fijos", "Primeras Diferencias", 
          "Efectos Aleatorios"),keep.stat=c("n","rsq"))
 
#Un modelo de variables binarias se realiza con el comando lm y lo unico que hay 
# que incluir dentro de las variables explicativas, son dummies relacionadas a 
# los individuo. Recuerde que incluir o no la constante afecta el n?mero de dummies
# que se incluiran en el modelo para evitar multicolinealidad. 

#--------------------------------------------------------------------------------------------------
#QUEDA COMO EJERCICIO REALIZAR LAS PRUEBAS RESPECTIVAS PARA COMPARAR LOS MODELOS,As?
#COMO LA RESPECTIVA VALIDACI?N DE SUPUESTOS Y SU RESPECTIVA CORRECCI?N,CUANDO SEA NECESARIO.
#--------------------------------------------------------------------------------------------------
