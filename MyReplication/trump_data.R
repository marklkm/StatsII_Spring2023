#Replication code for "Unconditional Support for Trump's Resistance Prior to Election Day"
#Brendan Hartnett and Alexandra Haver
#PS: Political Science and Politics

#Read data into R
library(readr)
library(ggplot2)
library(dplyr)
library(tidystats)
library(tidyr)
library(tidyverse)
library(zoo)
library(ggpubr)
library(stargazer)
library(aod)
library(broom)
library(ggpubr)

##### my code 

hist(dat$margin) # not normally distributed 

continuous <-select_if(dat, is.numeric)
summary(continuous)
names(continuous)

cor(dat$margin, dat$trumploss)
# -0.03814968

marg <- lm(margin ~ trumploss, data = dat)


summary(marg)
stargazer(marg)

str(dat)



# Interpetation of Anova
# the margin variable has a low sum of squares and a high P-Val which means there is not much variation that can
# be explained by the interaction between trumploss and margin. 
interaction <- aov(trumploss ~ margin, data = dat)

summary(interaction)

#############




dat <- read_csv("/Users/marklikeman/desktop/ASDS-2023/applied-stats-2-2023/replication_project/trump-resistance-to-election-day/trump_dataset.csv")

#Subset only likely Trump voters, those those who had already voted, and leaners
dat <- subset(dat, presvote2020==2 | presvote2020_voted==2 | presvote_2020lean==2)


## Recode support for Trump's resistance ####
# Basic Trump lose

table(dat$trumplose)
prop.table(table(dat$trumplose))


# Recode support for Trump's resistance as dummy variable
dat$trumploss <- NA
dat$trumploss[dat$trumplose==2] <- 0 # concede defeat
dat$trumploss[dat$trumplose==1] <- 1 # resists results of the election 
table(dat$trumploss)
prop.table(table(dat$trumploss))


#Weighted mean for percent supporting Trump's resistance
weighted.mean(dat$trumploss, dat$nationalweight)

#### Figure 1 Trump resist vs Pop Vote Margin Loess graph using x ~ y formula ####
#weight data using national weights
library(ggplot2)
plot_trump <- ggplot(dat, aes(x=margin, y=trumploss*100, weight=nationalweight)) + 
  geom_smooth(colour = "black", se=T, span=1, level=.95) + theme_bw() + 
  xlab("Biden's popular vote margin") + 
  ylab("% Trump voters, Trump should resist results") + 
  scale_color_manual(values="#000000", "#000000") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100) +
  labs(caption="n=510.") + scale_x_continuous(breaks=c(1, 3, 6, 9, 12, 15))
plot_trump
plot(plot_trump)

dat %>%
  mutate(loess = predict(loess(trumploss ~ margin, data = dat))) %>%
  ggplot(aes(margin, trumploss)) +
  geom_point(color = "grey50") +
  geom_line(aes(y = loess))

#################### MODEL 1 

#### Model 1: Test for Trump margin significance using logit model####

# using the glm (generalized linear model) function.
model1 <- glm(trumploss~margin, data=dat, weights=nationalweight)
summary(model1)
stargazer(model1)




mylogit <- glm(trumploss ~ margin, data = dat, family = "binomial")
summary(mylogit)

#glm(formula = trumploss ~ margin, family=binomial, data=dat)


a2 <- glm(trumploss~margin,family="quasipoisson",data=dat, weights=nationalweight)
summary(a2)
stargazer(a2)
plot(a2)

continuous <-select_if(dat, is.numeric)
summary(continuous)
names(continuous)

##### MY WEIGHTS TEST
weights <- dat$nationalweight
model <- glm(trumploss ~ margin, data = dat, weights = weights)
summary(model)
####### END

coef(model1)
model1
confint(model1)
stargazer(model1)
plot(model1)

test <- lm(formula = trumploss ~ margin, data = dat)
plot(test)


myprobit <- glm(trumploss ~ margin, family = binomial(link = "logit"), data = dat)
summary(myprobit)
confint(myprobit)


#Recode demographics ####

#Recode Party Identification
table(dat$pid3)
dat$partyid <- NA
#Code all who identified as Democrats or Independents as 'Non-Republican'
dat$partyid[dat$pid7<5] <- "Non-Republican"
#Code those who identified as other/not sure as 'Non-Republican
dat$partyid[dat$pid7==8] <- "Non-Republican"
#Code Lean Republican
dat$partyid[dat$pid7==5] <- "Lean Republican"
#Code Republican
dat$partyid[dat$pid7==6] <- "Republican"
#Code Strong Republicans
dat$partyid[dat$pid7==7] <- "Strong Republican"
#Descriptive table of party ID
table(dat$partyid)
#Order Party Identification
dat$partyid <- factor(dat$partyid, levels = c("Non-Republican", "Lean Republican", 
                                              "Republican", "Strong Republican"))
table(dat$partyid)

#Recode Level of Education
dat$education3 <- NA
table(dat$education)
#Code those who did not graduate high school, only graduated high school, or 
#went to vocational school as 'no -college'
dat$education3[dat$education<4] <- "No college"
#Code those who have some college experience but no degree, or an associates degree
#as 'Some college'
dat$education3[dat$education==4 | dat$education==5] <- "Some college"
#Code those who have a bachelors degree as 'college degree'
dat$education3[dat$education>5] <- "College degree"
#Descriptive degree of education 
table(dat$education3)
#Order Level of Education
dat$education3 <- factor(dat$education3, levels = c("No college", "Some college", 
                                                    "College degree"))
table(dat$education3)

#Recode Household Income
table(dat$hhi)
dat$hhinc <- NA
#Less than $25k
dat$hhinc[dat$hhi<4] <- "Less than $25,000"
#Between $25k-75k
dat$hhinc[dat$hhi>3 & dat$hhi < 14] <- "$25,000-$74,999"
#Between $75k-125k
dat$hhinc[dat$hhi>13 & dat$hhi < 20] <- "$75,000-$124,999"
#Over $125k
dat$hhinc[dat$hhi>19] <- "Over $125,000"
#Descriptive table of household income
table(dat$hhinc)
#Order Household Income
dat$hhinc <- factor(dat$hhinc, levels = c("Less than $25,000", "$25,000-$74,999", 
                                          "$75,000-$124,999", "Over $125,000"))
table(dat$hhinc)

#Recode Age into Categorical Variable
dat$agecat <- NA
#Under age 35
dat$agecat[dat$age<35] <- "Under 35"
#Between 35 years old and 50
dat$agecat[dat$age>34 & dat$age<50] <- "35-49"
#Between 50 and 65
dat$agecat[dat$age>49 & dat$age<65] <- "50-64"
#Over 65
dat$agecat[dat$age>65] <- "65 and over"
#Descriptive table of age categories
table(dat$agecat)
#Order age category 
dat$agecat <- factor(dat$agecat, levels = c("Under 35", "35-49", "50-64", "65 and over"))
table(dat$agecat)

#Recode gender
table(dat$gender)
dat$gen <- NA
#Male
dat$gen[dat$gender==1] <- "Male"
#Female
dat$gen[dat$gender==2] <- "Female"
#Descriptive table of gender identity
table(dat$gen)

#Recode News interest
#so that higher values indicate more interest in the news
dat$newsinterest <- NA
dat$newsinterest[dat$newsint==1] <- 4
dat$newsinterest[dat$newsint==2] <- 3
dat$newsinterest[dat$newsint==3] <- 2
dat$newsinterest[dat$newsint==4] <- 1
#Descriptive table of news interest
table(dat$newsinterest)

#Acknowledgment of racism
#so that higher values indicate more acknowledgment of racism
table(dat$acknowledgment)
dat$ackracism <- NA
dat$ackracism[dat$acknowledgment==1] <- 6
dat$ackracism[dat$acknowledgment==2] <- 5
dat$ackracism[dat$acknowledgment==3] <- 4
dat$ackracism[dat$acknowledgment==4] <- 3
dat$ackracism[dat$acknowledgment==5] <- 2
dat$ackracism[dat$acknowledgment==6] <- 1
#Descriptive table of acknowledgment of racism
table(dat$ackracism)


############################### MODEL 2

### Model 2: Test for Trump margin significance using logit model and demographics ####
model2 <- glm(trumploss ~ margin + factor(agecat) + factor(education3) + factor(hhinc) 
              + factor(partyid) + factor(gen) + newsinterest + ackracism, 
              data=dat, weights=nationalweight)
model2

summary(model2)
stargazer(model2)



dat$newsinterest <- factor(dat$newsinterest)
mylogit <- glm(trumploss ~ newsinterest, data = dat, family = binomial(link="logit"))

summary(mylogit)
stargazer(mylogit)
confint(mylogit)

############################### MY MODEL 3


model3 <- glm(trumploss ~ margin + factor(agecat) + factor(education3) 
              + factor(partyid) + factor(gen) + ackracism, 
              data=dat, weights=nationalweight)
model3
summary(model3)
stargazer(model3)


############################### END MY MODEL 3


#Table 1: Models 1 and 2 ####
#Export for paper
library(stargazer)
stargazer(model1, model2, type="html", out="Table_1_regression_models.doc", 
          intercept.bottom = T, intercept.top = F, digits=4, single.row=T)

#Recode Trump Qualitative Code####
# Subset data for anyone who provided at least one reason for Trump to resist
# Recode reasons to dummy  variables in data set to get proportion
#of total respondents (147)  
# Need to subset only for when tres11 is not NA, 
#because any respondent in this proportion 
# would have given a valid answer (not NA) to this question
table(dat$tres11)
trumprea <- subset(dat, dat$tres11!="NA")

#Recode responses into categories as to why they support resistance  
# Support Trump
trumprea$supportrump <- 0
trumprea$supportrump[trumprea$tres11=="Support Trump"] <- "Support Trump"
trumprea$supportrump[trumprea$tres22=="Support Trump"] <- "Support Trump"
trumprea$supportrump[trumprea$tres33=="Support Trump"] <- "Support Trump"
table(trumprea$supportrump)
prop.table(table(trumprea$supportrump))

# Democrats are radicals
trumprea$demrad <- 0
trumprea$demrad[trumprea$tres11=="Democrats are Radical"] <- "Democrats are Radicals"
trumprea$demrad[trumprea$tres22=="Democrats are Radical"] <- "Democrats are Radicals"
trumprea$demrad[trumprea$tres33=="Democrats are Radical"] <- "Democrats are Radicals"
table(trumprea$demrad)

# Election Irregularities
trumprea$elecirreg <- 0
trumprea$elecirreg[trumprea$tres11=="Distrust Election"] <- "Election Irregularities"
trumprea$elecirreg[trumprea$tres22=="Distrust Election"] <- "Election Irregularities"
trumprea$elecirreg[trumprea$tres33=="Distrust Election"] <- "Election Irregularities"
table(trumprea$elecirreg)

# Voter Fraud / Vote by Mail
trumprea$tmail <- 0
trumprea$tmail[trumprea$tres11=="Voter Fraud"] <- "Voter Fraud/Mail in Ballots"
trumprea$tmail[trumprea$tres22=="Voter Fraud"] <- "Voter Fraud/Mail in Ballots"
trumprea$tmail[trumprea$tres33=="Voter Fraud"] <- "Voter Fraud/Mail in Ballots"
table(trumprea$tmail)

# Democrats are Corrupt
trumprea$dcur <- 0
trumprea$dcur[trumprea$tres11=="Democrats are Corrupt"] <- "Democrats are Corrupt"
trumprea$dcur[trumprea$tres22=="Democrats are Corrupt"] <- "Democrats are Corrupt"
trumprea$dcur[trumprea$tres33=="Democrats are Corrupt"] <- "Democrats are Corrupt"
table(trumprea$dcur)

# Biden is incompetent
trumprea$binc <- 0
trumprea$binc[trumprea$tres11=="Biden is Incompetent"] <- "Biden is Incompetent"
trumprea$binc[trumprea$tres22=="Biden is Incompetent"] <- "Biden is Incompetent"
trumprea$binc[trumprea$tres33=="Biden is Incompetent"] <- "Biden is Incompetent"
table(trumprea$binc)

# Other
trumprea$tother <- 0
trumprea$tother[trumprea$tres11=="Other"] <- "Other"
trumprea$tother[trumprea$tres22=="Other"] <- "Other"
trumprea$tother[trumprea$tres33=="Other"] <- "Other"
table(trumprea$tother)

#### Number of Trump Respondents Giving Reason for Tables ####
table(trumprea$supportrump)
table(trumprea$demrad)
table(trumprea$elecirreg)
table(trumprea$tmail)
table(trumprea$dcur)
table(trumprea$binc)
table(trumprea$tother)

#### Proportions of Trump Respondents Giving Reason for Tables ####
prop.table(table(trumprea$supportrump))
prop.table(table(trumprea$demrad))
prop.table(table(trumprea$elecirreg))
prop.table(table(trumprea$tmail))
prop.table(table(trumprea$dcur))
prop.table(table(trumprea$binc))
prop.table(table(trumprea$tother))

####Recode Categories for general motivating theme ####
#partisanship and negative partisanship
trumprea$partisan <- NA
trumprea$partisan <- 0
trumprea$partisan[trumprea$supportrump=="Support Trump"] <- 1
trumprea$partisan[trumprea$demrad=="Democrats are Radicals"] <- 1
prop.table(table(trumprea$partisan))
#Concerns about election legitimacy
table(trumprea$partisan)
trumprea$electionconcerns <- NA
trumprea$electionconcerns <- 0
trumprea$electionconcerns[trumprea$elecirreg=="Election Irregularities"] <- 1
trumprea$electionconcerns[trumprea$tmail=="Voter Fraud/Mail in Ballots"] <- 1
prop.table(table(trumprea$electionconcerns))
table(trumprea$electionconcerns)
#other themes
trumprea$others <- NA
trumprea$others <- 0
trumprea$others[trumprea$tother=="Other"] <- 1
trumprea$others[trumprea$binc=="Biden is Incompetent"] <- 1
trumprea$others[trumprea$dcur=="Democrats are Corrupt"] <- 1
table(trumprea$others)
prop.table(table(trumprea$others))

#### t-test for misinformation and election concerns compared to partisan reason ####
t.test(trumprea$electionconcerns, trumprea$partisan, alternative = "greater", 
       var.equal = FALSE)