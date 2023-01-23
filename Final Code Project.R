#clear previous work from global environment and console
rm(list=ls()) #clears the global environment
cat("\f")  #clears the console

#Reference: 
#Becker, R. and Sinko, A. (2015). R GARCH - ECLR. [online] eclr.humanities.manchester.ac.uk. Available at: http://eclr.humanities.manchester.ac.uk/index.php/R_GARCH.

mp<-c("tidyverse","rugarch","rmgarch","Metrics","quantmod","PerformanceAnalytics","fBasics","rcompanion","tseries","forecast")
lapply(mp,require,character.only=T)



#Downloading the data
tickers = c("^GSPC", "BTC-USD")
getSymbols(tickers, src = 'yahoo',  from = "2017-01-01", 
           to  = "2021-12-31", auto.assign = TRUE, warnings = FALSE)

Bitcoin.prices = `BTC-USD`
data = cbind(GSPC$GSPC.Close, Bitcoin.prices$`BTC-USD.Close`)
View(data)

#check for presence of missing values
sum(is.na(data))
#ommiting the missing values
data = na.omit(data)

#Plot the Prices 
chart_Series(data$GSPC.Close, name = "Plot of S&P 500 daily Prices")
chart_Series(data$BTC.USD.Close, name = "Plot of Bitcoin daily Prices")

#ACF of the Prices daily
acf(data$GSPC.Close, na.action = na.pass) %>% 
  autoplot(main = "ACF of the S&P 500 Prices Daily")
acf(data$BTC.USD.Close, na.action=na.pass) %>% 
  autoplot(main = "ACF of the Bitcoin Prices Daily")


rtns = Return.calculate(data, method = "log")
rtns = na.omit(rtns)
colnames(rtns) <- c("S&P 500 Returns", "Bitcoin returns")
View(rtns)

#Plot the Returns
chart_Series(rtns$`S&P 500 Returns`, theme = chart_theme(), clev = 0,
             name = "Plot of the S&P 500 Returns")
chart_Series(rtns$`Bitcoin returns`, theme = chart_theme(), clev = 0,
             name = "Plot of the Bitcoin Returns")

#ACF of the returns
acf(rtns$`S&P 500 Returns`, na.action=na.pass) %>% 
  autoplot(main = "ACF of the S&P 500 Daily Returns")
acf(rtns$`Bitcoin returns`, na.action=na.pass) %>% 
  autoplot(main = "ACF of the Bitcoin Daily Returns")


plotNormalHistogram(rtns$`S&P 500 Returns`, main = "Histogram of S&P 500 Returns",
                    xlab = "S&P 500 Daily returns")
plotNormalHistogram(rtns$`Bitcoin returns`, main = "Histogram of Bitcoin Returns",
                    xlab = "Bitcoin Daily returns")



basicStats(data$GSPC.Close)
basicStats(data$BTC.USD.Close)
basicStats(rtns$`S&P 500 Returns`)
basicStats(rtns$`Bitcoin returns`)

# Dickey Fuller test on the prices and returns 
library(fUnitRoots)
adfTest(as.numeric(data$GSPC.Close))
adfTest(as.numeric(data$BTC.USD.Close))

adfTest(as.numeric(rtns$`S&P 500 Returns`))
adfTest(as.numeric(rtns$`Bitcoin returns`))

#Test for ARCH effect
library(nortsTest)
Lm.test(rtns$`S&P 500 Returns`, lag.max = 3, alpha = 0.05)
Lm.test(rtns$`Bitcoin returns`, lag.max = 3, alpha = 0.05)


spc_model <- ugarchspec(variance.model = list(garchOrder = c(1,1),model="sGARCH"),mean.model = list(armaOrder = c(0,0)))

SPg11fit <- ugarchfit(spc_model,out.sample = 300,data = rtns$`S&P 500 Returns`)
SPg11fit


gch_coef = SPg11fit@fit$coef 
gch_coef


gch_var <- SPg11fit@fit$var 
head(gch_var)
gch_var = xts(gch_var, order.by = index(rtns[1:957,]))
colnames(gch_var) = c("Conditional Volatility")
plot(gch_var, main = "Conditional Volatility of S&P 500 by GARCH(1,1)")

#extracting the estimated residuals
garch_res <- (SPg11fit@fit$residuals) ;head(garch_res) 
garch_res = xts(garch_res, order.by = index(rtns[1:957,]))
colnames(garch_res) = c("Standardized residuals")
plot(garch_res, main = "Standardized Residuals of S&P 500 by GARCH(1,1)")


spc2 <- ugarchspec( variance.model = list(garchOrder = c(1,1),model="sGARCH"),mean.model = list(armaOrder = c(0,0)))

BTCg11fit <- ugarchfit(spc2, out.sample = 300, data = rtns$`Bitcoin returns`)
BTCg11fit@fit$coef
BTCg11fit

gch_coef = BTCg11fit@fit$coef
gch_coef

gch_var <- BTCg11fit@fit$var
head(gch_var)
gch_var = xts(gch_var, order.by = index(rtns[1:957,]))
colnames(gch_var) = c("Conditional Volatility")
plot(gch_var, main = "Conditional Volatility of Bitcoin by GARCH(1,1)")

#extracting the estimated residuals
garch_res <- (BTCg11fit@fit$residuals) ;head(garch_res) 
garch_res = xts(garch_res, order.by = index(rtns[1:957,]))
colnames(garch_res) = c("Standardized residuals")
plot(garch_res, main = "Standardized Residuals of Bitcoin by GARCH(1,1)")


#Multivariate GARCH model constructed  in R by (Becker and Sinko, 2015)

#MULTIVARIATE GARCH 
# DCC (MVN)
#We are using the rmgarch package in this situation since it has a lot of beneficial features. We are utilising it to build a multivariate volatility model for the returns of Bitcoin and the S&P 500.
require(rmgarch)
#The kind of GARCH model we want to estimate must first be determined, and this must be told to R.
#We use the ugarchspec() function to tell R what kind of model it is. The Multivariate GARCH is being employed in this situation.
#The uspec.n function displays a list of all relevant model requirements.
uspc.n = multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(0,0)))))
#What results from this command?
#You'll realise that ugarchspec offers a mean-based AR(1)-GARCH(1,1) model. model = list[armaOrder = c(0, 0)] Using replicate, we duplicate this model twice (2, ugarchspec..). (because the S&P 500 and Bitcoin are our two assets).
print(uspc.n)
#We will now estimate these univariate GARCH models using the multifit command.
mltf = multifit(uspc.n, rtns)
print(mltf)
#After storing the findings, we run multf into the command window to see the estimated parameters for these two models. However, we will now go ahead and define the DCC model.
#The dccspec function is used to define the correlation specification.
spc1 = dccspec(dccOrder = c(1, 1), uspec = uspc.n, distribution = 'mvnorm')
print(spc1)
#Finally, we may estimate the model using the dccfit function.
ft1 = dccfit(spc1, data = rtns,  fit = mltf, fit.control = list(eval.se = TRUE))
#We want to estimate the model as specified in spec1 using the data in returns. If the estimation method fits, the standard errors for the estimated parameters will be provided. The syntax is control = list(eval.se = TRUE). We should apply the previously calculated univariate models as they were saved in multf, which is why fit = multf is important.
print(ft1)
#Print(ft1) gives us the specification results to analyse from readings.

# plot method
plot(ft1, which = 1) # 1: Conditional Mean (vs Realized Returns)
plot(ft1, which = 2) # 2: Conditional Sigma for each series (vs Realized Absolute Returns)
plot(ft1, which = 3) # 3: Conditional Covariance
plot(ft1, which = 4) # 4: Conditional Correlation

#extracting covariance series
conditional.covariance = rcov(ft1)
ts.plot(conditional.covariance[1,2,]) #this is the same plot produced in line 163

#extracting correlation series
conditional.correlations = rcor(ft1)
ts.plot(conditional.correlations[1,2,]) #this is the same plot produced in line 162

#Residual Analysis
dcc.residuals <- residuals(ft1)
plot(dcc.residuals$`S&P 500 Returns`, main = "Residuals on S&P 500")
plot(dcc.residuals$`Bitcoin returns`, main = "Residuals on Bitcoin")


#Reference: 
#Becker, R. and Sinko, A. (2015). R GARCH - ECLR. [online] eclr.humanities.manchester.ac.uk. Available at: http://eclr.humanities.manchester.ac.uk/index.php/R_GARCH.

