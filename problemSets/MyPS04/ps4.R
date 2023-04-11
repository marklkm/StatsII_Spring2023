

#########################################################
################### Problem Set 3 #######################
## ASDS - Applied Stats II - Spring - 2023 ##
## Mark Likeman - 19312796 ##
## Due Date Sunday, April 9, 2023
#########################################################



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

# adding additional libraries for this problem set
# which are the eha library and 'survival' library
lapply(c("tidyverse",
         "stargazer",
         "ggfortify",
         "survival",
         "survminer",
         "eha",
         "nnet",
         "dplyr",
         "MASS",
         "plotly",
         "ggplot2"),  pkgTest)


# set working directory
setwd("/Users/marklikeman/desktop/ASDS-2023/applied-stats-2-2023/problemset04")
# getwd()


# Read in the data and have a look at it
data(infants)
glimpse(infants)
infants
table(infants)

# inspecting the data
str(infants)
names(infants)

table(infants$sex)
# girl  boy 
#  34   71

prop.table(table(infants$sex))
# girl       boy 
# 0.3238095 0.6761905 

# recode 
infants$gender <- NA
infants$gender[infants$sex=='boy'] <- 0
infants$gender[infants$sex=='girl'] <- 1

# histogram to look at the visual of the age and sex variables
plot_ly(infants, x = ~age, color = ~sex) %>%
  add_histogram()


## Looking at the variables 

# stratum - triplet no. each triplet consists of one infant whose mother died (a case) and two controls, 
# i.e. infants whos mother did not die. Matched on the covariates below: 

# enter - Age (in days) of case when its mother died
# exit - Age (in days) at death or right censoring (at age 365 days)
# event - Follow up ends with death (1) or right censoring (0)
# mother - dead for cases, alive for controls
# age - Mother's age at infant's birth
# sex - The infant's sex 
# parish - Birth parish, either Nedertornea or not Nedertornea (other)
# civst - Civil status of mother, married or unmarried
# ses - Socio economic status of mothe, either farmer or not farmer (other)
# year - Year of birth of the infant



#########################################################
# Question 1
#########################################################

# We're interested in modeling the historical causes of child mortality. 
# We have data from 26855 children born in Skellefteå, Sweden from 1850 to 1884.
# Using the "child" dataset in the eha library, fit a Cox Proportional Hazard Model usinf mother's age 
# and infant's gender as covariates. 
# Present and interpret the output.

# In order to fit a Cox Proportional Hazard Model the coxph() function from the 'survival' package is required
# this needs to be in a Surv() format which represents time to event data.
# the first argument is time variable

### Testing with Weibull proportional hazard model

# Proportional hazards model with parametric baseline hazard(s). 
# Allows for stratification with different scale and shape in each stratum, and left truncated and right censored data.

fit <- coxreg(Surv(enter, exit, event) ~ strata(stratum) + mother, data
              = infants)
fit
fit.w <- phreg(Surv(enter, exit, event) ~ mother + parish + ses, data =
                 infants)
summary(fit.w) ## Weibull proportional hazards model.
plot(fit.w)

coef(fit.w) # for extracting the coefficients from the model


infants_s <- with(infants, Surv(enter, exit, event))
stargazer(infants)

# The Kaplan-Meier method is a non parametric statistic that allows you to estimate the survival function

# plotting Kaplan-Meier method
kaplan <- survfit(infants_s ~ 1, data = infants)
summary(kaplan, times = seq(0, 15, 1))

plot(kaplan, main = "Kaplan-Meier Method", xlab = "Days", ylim = c(0.5, 1)) 
autoplot(kaplan)

# plotting the sex covariates using the infants gender , girl and boy
kaplan_sex <- survfit(infants_s ~ sex, data = infants)
autoplot(kaplan_sex)

# running the Cox Proportional Hazard Model, using mothers age and infants gender as the covariates 
cox <- coxph(Surv(enter, exit, event) ~ age + sex, data = infants)
summary(cox)

############ Interpretation

# there is a .485 decrease in the expected log of the hazard for male babies in comparison to female babies,
# while holding the age of the moether constant

# there is .04 decrease in the expected log of of the hazrd each time the mothers age increases by 1 year or 1 unit increase
# while holding the sex of infant constant

# coef    exp(coef) se(coef)                  z Pr(>|z|)
# age    -0.04044   0.96037  0.04507 -0.897    0.370
# sexboy -0.48518   0.61559  0.44224 -1.097    0.273


### hazard ratio for male babies is .61 compared to that of female babies or 62 male babies die for every 100 female babies
exp(coef(cox))
# age       sexboy 
# 0.9603673 0.6155879 


stargazer(cox)


####################################### Dependent variable  is : infant_s
# testing the out of the coefficients
coef(cox_s)
# age      sexboy 
# -0.04043946 -0.48517752 

exp(coef(cox_s))
# age    sexboy 
# 0.9603673 0.6155879 


cox.w <- phreg(Surv(enter, exit, event) ~ age + sex, data = infants)
summary(cox.w)
plot(cox.w)

# Covariate             Mean       Coef     Rel.Risk   S.E.    LR p
# age                  27.127    -0.050     0.951     0.045   0.2376 
# sex                                                         0.4029 
# girl                0.317     0         1 (reference)
# boy                 0.683    -0.375     0.687     0.444

# Events                    21 
# Total time at risk         21616 
# Max. log. likelihood      -154.85 
# L3 R test statistic         2.10 






