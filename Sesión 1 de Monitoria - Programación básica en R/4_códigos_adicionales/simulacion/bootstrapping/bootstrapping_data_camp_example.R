library(tidyverse)

# Visualization of iris dataset
head(iris)
glimpse(iris)

# Library used to perform the bootstrap
library(boot)

# Function that creates statistics out of resampled data 
# foo returns a vector containing the statistic(s) of interest
foo <- function(data, indices, cor.type){
  # Main args: 
  ## data: the dataset from which the data comes from
  ## indices: A vector containing the indices of elements from a dataset that were picked to create a bootstra sample
  # additional args:
  ## cor.type: to select the specific type of correlation to be used (not fundamental, that is why it is additional)
  dt<-data[indices,]
  c(
    cor(dt[,1], dt[,2], method=cor.type),
    median(dt[,1]),
    median(dt[,2])
  )
}

# Performing the boostrap of the 3 statistics defined by foo
# using the function boot
set.seed(12345)
myBootstrap <- boot(iris, foo, R=1000, cor.type='s')

# results returned by the the nonparametrics bootstrapped performed above: 
myBootstrap$t0
myBootstrap$t
myBootstrap

# Before computing CIs, it's always worth to take a look at the distribution 
# of bootstrapas realizations

# Different plots for 
plot(myBootstrap, index=1) # plot for Spearman's correlation
plot(myBootstrap, index=2) # plot for median1
plot(myBootstrap, index=3) # plot for median2

# From plot1 its seems the correlation bootstrapped coefficients seems
# quite normal-like 

boot.ci(myBootstrap, index = 1) # this function is used for finding the confidence intervals of the bootstapped samples

boot.ci(myBootstrap, index=1, type=c('basic','perc')) # for selecting only basic and perc confs. intervals
boot.ci(myBootstrap, index=1, type=c('norm'))$norm # for selecting only norm intervals
boot.ci(myBootstrap, index=1, type='basic')$basic # for selecting only basic intervals

# Confidence intervals for petal width median

boot.ci(myBootstrap, index=3)
plot(myBootstrap, index=3)

# Replication purposes

# indices=T
tableOfIndices<-boot.array(myBootstrap, indices=T)
dim(tableOfIndices)
tableOfIndices[1,]

# indices=F
tableOfIndices<-boot.array(myBootstrap, indices=F)
tableOfIndices[1,]

onceAgain = apply(tableOfIndices, 1, foo, data=iris, cor.type='s')
dim(onceAgain)

head(t(onceAgain))
head(myBootstrap$t)

all(t(onceAgain)==myBootstrap$t)
