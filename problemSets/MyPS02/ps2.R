
#########################################################
################### Problem Set 2 #######################
## ASDS - Applied Stats II - Spring - 2023 ##
## Mark Likeman - 19312796 ##
## Due Date Sunday, Feb. 19, 2023
#########################################################

library(stargazer)
library(readr)
library(dplyr)
library(ggplot2)

# t <- c(12,11,13)
# COPY <- data.frame(t)
# COPY

#########################################################
# Question 1
#########################################################

load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))

# how the data is being stored in the dataset

str(climateSupport)
names(climateSupport)
# inspect
head(climateSupport)
tail(climateSupport)

# response variable - choice or observe changes (what's measured)
# explanatory variable - countries and sanctions or what changes as a result
# (what changes)

# tables shows inputs as factors or only 3 countries with sanctions
# 20 counties and with no sanctions (baseline)

table(climateSupport$countries)
table(climateSupport$sanctions)

# factors to numeric values
# converting a vector or a factor to a numeric vector 
climateSupport$choice <- as.numeric(as.factor(climateSupport$choice))-1

# 1 = supported and 0 = did not support

# countries
climateSupport$countries <- as.numeric(as.factor(climateSupport$countries))-1

# sanctions
climateSupport$sanctions <- as.numeric(as.factor(climateSupport$sanctions))-1

# fit an additive model. provide the summary output
# depends on countries + sanctions
# the data here is binary  1,0 for a logit regression
additive_model <- glm(choice ~ countries + sanctions, data = climateSupport, 
                 family = binomial(logit))

summary(additive_model)
# Results

# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.14458    0.04518  -3.200  0.00137 ** 
# countries    0.32436    0.02689  12.062  < 2e-16 ***
# sanctions   -0.12353    0.01964  -6.291 3.15e-10 ***

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null deviance: 11783  on 8499  degrees of freedom
# Residual deviance: 11597  on 8497  degrees of freedom
# AIC: 11603

# Number of Fisher Scoring iterations: 4

# calculate p-value of overall Chi-Square statistic
1-pchisq(11783-11597, 8499-8497)
# [1] 0

# since this p-value is less than .05, we reject the null hypothesis
# there is a significant relationship between the combination of 
# countries and sanctions and the final choice selected 

####

# trying to discount the global null hypothesis

summary_glm <- summary(additive_model)
anova(additive_model, glm, test = "Chisq")

# The null hypothesis (H0) of the ANOVA is no difference in means, 
# and the alternative hypothesis (Ha) is that the means are different 
# from one another.
# here the anova gives p-value of 2.2e-16  or 2 ^ -16
# and. can reject the null hypothesis

exp(summary_glm$coefficients[1 ,1])/(1 + exp(summary_glm$coefficients[1 ,1]))
bl = summary_glm$coefficients [1 ,1]
exp(bl + summary_glm$coefficients[2 ,1])/(1 + exp(bl + summary_glm$coefficients
    [2,1]))


levels(climateSupport$sanctions)

climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, ordered = F), ref = "5%")

climateSupport$sanctions

# fitting the additive model

c_logit <- glm(formula = choice ~ countries + sanctions, family = "binomial", data = climateSupport)
summary(c_logit)

# or c_logit <- glm(formula = choice ~ countries + sanctions, family = "binomial", data = climateSupport)
# summary(c_logit)
# 


# Coefficients:
#                   Estimate Std. Error  z value  Pr(>|z|)    
# (Intercept)       0.24743   0.04406   5.616    1.95e-08 ***
#  countries.L      0.45845   0.03810   12.033   < 2e-16 ***
#  countries.Q     -0.00995   0.03806  -0.261    0.79374    
# sanctionsNone    -0.19185   0.06216  -3.086    0.00203 ** 
#  sanctions15%    -0.32510   0.06224  -5.224    1.76e-07 ***
#  sanctions20%    -0.49542   0.06228  -7.955    1.79e-15 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 11783  on 8499  degrees of freedom
# Residual deviance: 11568  on 8494  degrees of freedom

# 11783 = null deviance and 11568 = residual deviance
# find the p-value for the chi-square test statistic
pchisq(11783-11568, 5, lower.tail = F) 

1-pchisq(11783-11568, 8499-8494)

# this gives
# 1.749304e-44 



# logit model
# period functions as omnibus selector (the kitchen sink additive model)
# ~ . will select countries (ord) and sanctions (fct)

climate_logit <- glm(choice ~ ., family = binomial(link="logit"), data = climateSupport)
summary(climate_logit)

reg_exp <- exp(coef(climate_logit))
stargazer(reg_exp, type = "text")


# not supported or supported
#  the dependent variable is binary(0/1, True/False, Yes/No) in nature
t_glm <- glm(choice ~ 1, data = climateSupport, family=binomial(link = "logit"))
summary(t_glm)

exp(summary_glm$coefficients [1 ,1])/(1 + exp(summary_glm$coefficients [1 ,1]))

#########################################################
# Question 2
#########################################################


# something to do with how does the baseline countries = 20 and sanctions = 5%

# see lecture 4 notes

########## Part (a) ##########

# previous results from Question 1
# # Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.14458    0.04518  -3.200  0.00137 ** 
# countries    0.32436    0.02689  12.062  < 2e-16 ***
# sanctions   -0.12353    0.01964  -6.291 3.15e-10 ***

log_odds1 <- exp((-0.144558) + (0.32436)*2 + (-0.12353)*1)/(1 + exp ((-0.144558) + (0.32436)*2 + (-0.12353)*1))
log_odds1
# result
# 0.5940255

log_odds2 <- exp((-0.144558) + (0.32436)*2 + (-0.12353)*2)/(1 + exp ((-0.144558) + (0.32436)*2 + (-0.12353)*2))
log_odds2
# result
# 0.5639238

# interpertation of additive model week 4 lecture

odds_difference <- log_odds1 - log_odds2
odds_difference
# result
# 0.5639238


########## Part (b) ##########

# referencing week 4 lecture 4 notes
# using the exp(coefficients) function to create the log odds ratio
# 5% sanctions
odds_1 <- exp((-0.144558) + (0.32436)*0 + (-0.12353)*1)/(1 + exp ((-0.144558) + (0.32436)*0 + (-0.12353)*1))

odds_1
# result
# 0.4333765

exp(odds_1)
# result
# 1.542457

# 15% 
odds_2 <- exp((-0.144558) + (0.32436)*0 + (-0.12353)*2)/(1 + exp ((-0.144558) + (0.32436)*0 + (-0.12353)*2))

odds_2
# result
# 0.4033279

exp(odds_2)
# result
# 1.496798

odds_diff <- odds_1 - odds_2
odds_diff
# result
# 0.03004869

# interpertation
# an increase 0.03004869 of log odds of a participant supporting the climate policy when
# the sanctions are increased from 5% to 15% 


# or trying another way (just experimenting)
# the coefficients are exponentiated: exp(coef(fit))
fit <- glm(choice ~ countries + sanctions, data = climateSupport, family="binomial")
exp(cbind(Odds_and_OR=coef(fit), confint(fit)))
# result
# 0.456359


########## Part (c) ##########

# with 80 coiuntries and no sanctions 

odds_3 <- exp((-0.144558) + (0.32436)*1 + (-0.12353)*0)/(1 + exp ((-0.144558) + (0.32436)*1 + (-0.12353)*0))
odds_3

# result
# 0.5448298

# interaction model

interaction_model <- glm(choice ~ countries*sanctions, data = climateSupport, family = binomial(logit))
summary(interaction_model)

# results

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -0.148144   0.057311  -2.585  0.00974 ** 
#  countries            0.328007   0.045036   7.283 3.26e-13 ***
#  sanctions           -0.121111   0.030987  -3.908 9.29e-05 ***
# countries:sanctions -0.002455   0.024288  -0.101  0.91950   
