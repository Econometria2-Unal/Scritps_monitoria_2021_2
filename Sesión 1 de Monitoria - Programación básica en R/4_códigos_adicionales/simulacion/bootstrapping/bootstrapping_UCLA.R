library(tidyverse)

# Use of sample function ----

#using sample to generate a permutation of the sequence 1:10
sample(10) # sample without replacement 

#bootstrap sample from the same sequence
sample(10, replace=T) # sample with replacement

#boostrap sample from the same sequence with 
#probabilities that favor the numbers 1-5 
# (i.e. a higher probability for the first 5 numbers)
prob1 <- c(rep(.15, 5), rep(.05, 5))
prob1
# veryfing prob1 is a correct probability function
sum(prob1) # its sum is equal to 1

# Bootstrap sample where there is a highter probability for obtaining
# the first 5 numbers
sample(10, replace=T, prob=prob1)

#sample of size 5 from elements of a matrix 
#creating the data matrix
y1 <- matrix( round(rnorm(25,5)), ncol=5)
y1

#saving the sample of size 5 in the vector x1
x1 <- y1[sample(25, 5)]
x1

#sampling the rows of the a matrix
#creating the data matrix
y2 <- matrix( round(rnorm(40, 5)), ncol=5)
y2

#saving the sample of rows in the matrix x2
x2 <- y2[sample(8, 3),  ] # 8 is the number of rows (we are selecting 3 of 8 rows)
x2

# A bootstrap example ----

# Objective: Obtain SE for the estimate of the median
# use of lapply and sapply functions in conjunction with the sample function 

#calculating the standard error of the median
#creating the data set by taking 100 observations 
#from a normal distribution with mean 5 and stdev 3
#we have rounded each observation to nearest integer
data <- round(rnorm(100, 5, 3))
data[1:10]

#obtaining 20 bootstrap samples 
#display the first of the bootstrap samples
resamples <- lapply(1:20, function(i) sample(data, replace = T)) # returns a list
resamples[1]

#calculating the median for each bootstrap sample 
r.median <- sapply(resamples, median) # returns a vector 
r.median

#calculating the standard deviation of the distribution of medians
sqrt(var(r.median))

#displaying the histogram of the distribution of the medians 
hist(r.median)

#function which will bootstrap the standard error of the median
b.median <- function(data, num) {
  resamples <- lapply(1:num, function(i) sample(data, replace=T)) # a list
  r.median <- sapply(resamples, median) # a vector
  std.err <- sqrt(var(r.median))
  list(std.err=std.err, resamples=resamples, medians=r.median)   
}

#generating the data to be used (same as in the above example)
data1 <- round(rnorm(100, 5, 3))

#saving the results of the function b.median in the object b1
b1 <- b.median(data1, 30) # We are creating 30 bootstrap samples out of data1

#displaying the first of the 30 bootstrap samples
b1$resamples[1]

#displaying the standard error
b1$std.err

#displaying the histogram of the distribution of medians
hist(b1$medians)

#we can input the data directly into the function and display 
#the standard error in one line of code
b.median(rnorm(100, 5, 2), 50)$std.err

#displaying the histogram of the distribution of medians
hist(b.median(rnorm(100, 5, 2), 50)$medians)

# built in bootstrapping functions ----

#R example of the function boot 
#bootstrap of the ratio of means using the city data included in the boot package
#make sure the boot package is installed using install.packages("boot")
library(boot)
#obtaining the data from the package
data(city)

#R example of the function boot 
#bootstrap of the ratio of means using the city data included in the boot package
library(boot)
#obtaining the data from the package
data(city)

#defining the ratio function
## w: weights
## d: dat(data frame)
ratio <- function(d, w) sum(d$x * w)/sum(d$u * w) # this function returns the statistic from which it will be performed the bootstap 

#using the boot function
# stype = "w" denotes weights 
boot(city, ratio, R=999, stype="w") # ordirnary nonparametric bootstrap 

