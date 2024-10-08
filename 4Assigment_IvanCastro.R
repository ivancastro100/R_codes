rm(list=ls()) #Removes all items in Environment!
library(dynlm) #for `dynamic linear models and ts regression`
library(orcutt) # for the `cochrane.orcutt()` function
library(nlWaldTest) # for the `nlWaldtest()` function
library(zoo) # for time series functions (not much used here)
library(pdfetch) # for retrieving data (just mentioned here)
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for kable()
library(forecast) 

dataUS <- read.csv(file='/Users/mccla/OneDrive/Escritorio/WTW/growth47-test.csv')
#is.ts(dataUS)

#quarterly data,starting time and ending time
us_gdp.ts <- ts(dataUS, start=c(1947,2), end=c(2019,1), frequency=4)

### Time series plot GDP
plot(us_gdp.ts[,"g"], ylab="GDP")

#### Question 2.1 ####
#AR(2)
ar2 <- dynlm(g ~ L(g,1) + L(g,2), data = us_gdp.ts)
s.ar2 <- summary(ar2)
kable(tidy(s.ar2), digits=4, caption="The GDP AR(2) model")
# Residuals 
res2 <- resid(s.ar2)


### Time series plot Residuals
plot(res2, ylab="Res AR(2)")
abline(h=0, lty=2) #add line at mean of ehat=0

### Residuals Lagged scatterplot 
ggL1 <- data.frame(cbind(res2,lag(res2,-1)))
names(ggL1) <- c("res","resL1")
plot(ggL1)
meang <- mean(ggL1$res, na.rm=TRUE) #na.rmskip NA values
abline(v=meang, lty=2)  #add straight line
abline(h=mean(ggL1$resL1, na.rm=TRUE), lty=2)

# Correlogram Residuals AR(2)
corrgm2 <- acf(res2, lag.max=24)
plot(corrgm2, main = "Correlogram AR(2)")

### Lagrange Multiplier test (Breusch-Godfrey test) AR(2)
bg_test2.1 <- bgtest(ar2, order=2, type = "Chisq", fill = 0)
bg_test2.2 <- bgtest(ar2, order=2, type = "Chisq", fill = NA)
dfr.2 <- data.frame(rbind(bg_test2.1[c(1,2,4)],bg_test2.2[c(1,2,4)]))
kable(dfr.2, caption="Breusch-Godfrey test for AR(2)")

#gmod <- glance(ar2)
#gmod$r.squared * (gmod$nobs - 2)

#### Question 2.2 ####
#AR(3)
ar3 <- dynlm(g ~ L(g,1) + L(g,2) + L(g,3), data = us_gdp.ts)
s.ar3 <- summary(ar3)
kable(tidy(s.ar3), digits=4, caption="The GDP AR(3) model")
# Residuals 
res3 <- resid(s.ar3)

### Time series plot Residuals AR(3)
plot(res3, ylab="Res AR(3)")
abline(h=0, lty=2) #add line at mean of ehat=0


# Correlogram Residuals AR(3)
corrgm3 <- acf(res3, lag.max=24)
plot(corrgm3, main = "Correlogram AR(3)")

### Lagrange Multiplier test (Breusch-Godfrey test) AR(3)
bg_test3.1 <- bgtest(ar3, order=3, type = "Chisq", fill = 0)
bg_test3.2 <- bgtest(ar3, order=3, type = "Chisq", fill = NA)
dfr.3 <- data.frame(rbind(bg_test3.1[c(1,2,4)],bg_test3.2[c(1,2,4)]))
kable(dfr.3, caption="Breusch-Godfrey test for AR(3)")

#gmod3 <- glance(ar3)
#gmod3$r.squared * (gmod3$nobs)

#### Question 2.3 ####
## Forecasting
y3 <- us_gdp.ts[,"g"]
ar3g <- ar(y3, aic=FALSE, order.max=3, method="ols")
fcst3 <- data.frame(forecast(ar3g, 3))
fcst3$Point.Forecast

# plot the forecasts produced by the AR(3) model of US GDP 
# and their interval estimates
plot(forecast(ar3g,3), main = "Forecasts AR(3)")

## long-hand calculation 
# df = 281
t <- qt(1-0.05/2, 281)

sig <- summary(ar3)$sigma
sig2 <- sig * sqrt(1+ ar3$coefficients[2]**2)
sig3 <- sig * sqrt((ar3$coefficients[2]**2 + ar3$coefficients[3])**2 
                   + 1 + ar3$coefficients[2]**2)

# step1 ahead: 2019Q2
fcst3$Point.Forecast[1]-t*sig
fcst3$Point.Forecast[1]+t*sig

# step2 ahead: 2019Q3
fcst3$Point.Forecast[2]-t*sig2
fcst3$Point.Forecast[2]+t*sig2

# step3 ahead: 2019Q4
fcst3$Point.Forecast[3]-t*sig3
fcst3$Point.Forecast[3]+t*sig3

