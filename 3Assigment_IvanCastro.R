rm(list=ls()) #ensure a clean R environment 
library(knitr)  #for referenced tables with kable()
library(xtable) #makes data frame for kable
library(printr) #automatically prints output nicely
library(effects)#Graphical and tabular effect displays
library(car) #Companion to Applied Regression
library(AER) #Applied Econometrics with R
library(broom) #for tidy lm output and function glance()
library(stats) #R stat functions
library(lmtest)#for coeftest() and other test functions 
library(stargazer) #nice and informative tables
library(ggplot2)

houses <- read.csv(file='/Users/mccla/OneDrive/Escritorio/WTW/br2_ps3.csv') 
summary(houses)
#tail(houses)

price <- houses[,1]
age <- houses[,5]
sqft <- houses[,2]
sqft100 <- sqft/100

###---------Question 1
#---- Model -----
model_ols <- lm(log(price) ~ sqft100 + age + I(age^2), data = houses)
summary(model_ols)
#-- Residuals --
res <- residuals(model_ols)
res_sq <- res^2 
hist(res)

###---------Question 2
par(mfrow = c(1, 2))
plot(age,res, type = "p")
plot(sqft100,res, type = "p")
#graphics.off()

###---------Question 3
## Breusch-Pagan heteroskedasticiy/LM test
alpha <- 0.01      #significance level
#The test equation:
model_res <- lm(res_sq ~ age + sqft100, data = houses) #auxiliary regression
#summary(model_res)
gmod_res <- glance(model_res) 
#Number of Betas in model #Chi-square is always a right-tail test
S <- gmod_res$df  #number of regressors in the auxiliary regression
chisqcr <- qchisq(1-alpha, S) #chi-square
Rsqres <- gmod_res$r.squared   #R-square
N <- gmod_res$nobs   #num of observations
chisq <- N*Rsqres   #chi-square statistic = N*R^2
chisq
pval <- 1-pchisq(chisq,S)
pval  # p close to 0, reject H0. hetero does exist

###--------Question 4
model_var <- lm(log(res_sq) ~ age + sqft100, data = houses)
summary(model_var)
## White's correction Variance model
cov2 <- hccm(model_var, type="hc1") #package 'car', "hc1"white correction
var.HC1 <- coeftest(model_var, vcov.=cov2)  #the same as summary()
var.HC1 

## White's correction original Model 
cov1 <- hccm(model_ols, type="hc1") #package 'car', "hc1"white correction
house.HC1 <- coeftest(model_ols, vcov.=cov1)  #the same as summary()
house.HC1   #heteroskedasticity-consistent se 

###--------Question 5
var_hat <- exp(fitted(model_var))
w <- 1/var_hat
model_gls <- lm(log(price) ~ sqft100 + age + I(age^2), weights = w, data = houses)
summary(model_gls)
gmod_gls <- glance(model_gls) 

###--------Question 7
## Breusch-Pagan heteroskedasticiy/LM test
res_gls <- residuals(model_gls)
res_sq2 <- res_gls^2 

alpha2 <- 0.01      #significance level
#The test equation:
model_res_gls <- lm(res_sq2 ~ age + sqft100, data = houses) #auxiliary regression  mod_residuals
summary(model_res_gls)
gmod_res_gls <- glance(model_res_gls) 

S2 <- gmod_res_gls$df  #number of regressors in the auxiliary regression
chisqcr2 <- qchisq(1-alpha2, S2) #chi-square
Rsqres2 <- gmod_res_gls$r.squared   #R-square
N <- gmod_res_gls$nobs   #num of observations
chisq_gls <- N*Rsqres2   #chi-square statistic = N*R^2
chisq_gls
pval2 <- 1-pchisq(chisq_gls,S)
pval2  # p close to 0, reject H0. hetero does exist

