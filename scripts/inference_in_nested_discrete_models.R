##################
#
# THIS IS INCOMPLETE
#
##########################################


# Inference in nested discrete models

# load libraries
library(ggplot2)
#for very large numbers, that come up as intermediate results when the 
#data is sampled
library(Brobdingnag)
#multinomial logistic regression:
library(nnet)

# generate data:
nObs <- 10000

age <- runif(nObs, min=18, max=90)
income <- runif(nObs, min=10000, max=100000)

#normalize all values!
age.norm <- (age-mean(age))/sd(age)
income.norm <- (income-mean(income))/sd(income)

#age.norm <- age
#income.norm <- income

#three evaluations: 0,1,2 (bad, okay, good)
beta0 <- c(0.1, 0.2, 0.6)
betaInc <- c(0.2, 0.7, 0.1)
betaAge <- c(0.5, 0.3, 0.4)

logOdds1 <- as.brob(beta0[1] + betaInc[1]*income.norm + betaAge[1]*age.norm)
logOdds2 <- as.brob(beta0[2] + betaInc[2]*income.norm + betaAge[2]*age.norm)
logOdds3 <- as.brob(beta0[3] + betaInc[3]*income.norm + betaAge[3]*age.norm)
prob1 <- as.numeric(exp(logOdds1) / (exp(logOdds1) + exp(logOdds2) + exp(logOdds3)))
prob2 <- as.numeric(exp(logOdds2) / (exp(logOdds1) + exp(logOdds2) + exp(logOdds3)))
prob3 <- 1-prob1-prob2

#now sample for each observation the category:

#for illustration, use a little convoluted sampling method:
evaluation <- vector(length=nObs)
for(i in 1:nObs){
  evaluation[i] <- sample(c("good", "ok", "bad"), size=1, prob=c(prob1[i], prob2[i], prob3[i]))
}

#compile the data:
  
sample.df <- data.frame(age,income,evaluation=as.factor(evaluation))
#and relevel for easier interpretation:
sample.df$evaluation <- relevel(sample.df$evaluation, ref = "bad")

fit <- multinom(evaluation ~ income +  age , data = sample.df)
summary(fit)

#do some manual checks:
head(data.frame(prob3,prob1,prob2))
head(fitted(fit))
#looks good at first sight

#however the coefficients seem so off - what is going on?
