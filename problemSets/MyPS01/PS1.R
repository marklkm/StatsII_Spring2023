############################################
## ASDS - Applied Stats 2 - Problem Set 1 ##
## Due - February 12, 2023 ##
############################################

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load the required libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c(),  pkgTest)

# set working directory wd for the current folder
setwd("/Users/mark/desktop/ASDS-S2-2023/ASDS-Applied-Stats-Sem-2-2023/MyPS1")

############################################
############################### QUESTION 1
############################################


set.seed(123) 
# as suggested in Question 1 to create reproducible results
# and so you get the same answers

# need to generate 1,000 Cauchy random variables

r <- 1000 # call this r
data1 <- rcauchy(r, location = 0, scale = 1)

# generate the Empirical Cumulative Distribution Function for the
# empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)

# generate the Test Statistic using the P value (p) we were given in R 
D <- max(abs(empiricalCDF - pnorm(data)))
pi <- 3.142
p_value <- sqrt(2*pi)*sum((1)^2)*(pi)^2/(8*(D)^2)

# incorporated the given function 
# Kolmogorov-Smirnov test (KS)

KS <- function(data) {
  ECDF <- ecdf(data)
  empiricalCDF < ECDF(data)
  # create the Test Statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  
  sqroot <- sqrt(2*pi)
  power_value <- ((1)^2)*((pi)^2)/(8*(D)^2)
  p_value <- sqroot*sum(exp(power_value))
  output <- list(D, p_value)
  return(output)
}

ks.test(data1, "pnorm") # using the KS test R function

# RESULT
# One-sample Kolmogorov-Smirnov test
# data:  data1
# D = 0.13573, p-value = 2.22e-16
# alternative hypothesis: two-sided

# Outputs
# D = 0.13573
# p-value = 2.22e-16


############################################
############################### QUESTION 2
############################################

# Given code
set.seed(123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# I can estimate the Regression using the lm() function
coef(lm(data$y ~ data$x))
# this gives
# (Intercept)      data$x 
# 0.1391874   2.7266985 


# create log normal likelihood function 
# using Tutorial 3 and the given code above to derive log-likelihood function 

# 
norm_likelihood1 <- function(outcome, input, parameter) {
  n <- nrow(input) # number of rows
  k <- ncol(input)
  beta   <- parameter[1:k] # numbers or Betas or β
  sigma1 <- parameter[k+1]^2 # sigma helps to test the significance 
  e      <- outcome - input%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma1) - ( (t(e) %*% e)/ (2*sigma1) )
  return(-logl)
}

# generate the norm_likelihood function and 
# show that you get the equivalent results to using lm
# another way to generate the log likelihood function

norm_likelihood2 <- function(outcome, input, parameter) {
  n <- ncol(input)
  beta <- parameter[1:n]
  sigma <- sqrt(parameter[1+n])
  -sum(dnorm(outcome, input %*% beta, sigma, log=TRUE))
}

# print our estimated coefficients (intercept and beta_1)

# here the above functions norm_likelihood1  and norm_likelihood2 
# can be run through the built in optim() function using the 
# Newton Raphson algorithm which is an iterative procedure that can be used 
# to calculate MLEs

results_norm1 <- optim(fn=norm_likelihood1, outcome=data$y, input=cbind(1, data$x), par=c(1,1,1), hessian=T, method="BFGS")
results_norm2 <- optim(fn=norm_likelihood2, outcome=data$y, input=cbind(1, data$x), par=c(1,1,1), hessian=T, method="BFGS")

results_norm1$par
# Intercept = 0.1396113
# Beta = 2.7266403

results_norm2$par
# Intercept = 0.1404966
# Beta = 2.7265080

# confirm that we get the same thing in with glm()
coef(lm(data$y~data$x))

# Results
# (Intercept)      data$x 
# 0.1391874   2.7266985 