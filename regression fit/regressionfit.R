final <- read.table("~/Desktop/temp/final.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
# View(final)
`colnames<-`(final,c("year","consumption","price","gdp"))
final[,4]<-log(final[,4])
final

# For 
consumption=final[,2]
price=final[,3]
gdp=final[,4]
dfr<-data.frame(consumption,price,gdp)
ltrain<-dfr[1:30,]
lholdo<-dfr[31:54,]
lmfit<-lm(consumption~price+gdp,data=ltrain)
summary(lmfit)

# Call:
#   lm(formula = consumption ~ price + gdp, data = ltrain)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.010 -4.069 -2.165  5.566  9.008 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 387.3048    43.0455   8.998 1.30e-09 ***
#   price        -0.1729     0.1273  -1.359    0.185    
# gdp          30.8921     5.1075   6.048 1.86e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.62 on 27 degrees of freedom
# Multiple R-squared:  0.6749,	Adjusted R-squared:  0.6509 
# F-statistic: 28.03 on 2 and 27 DF,  p-value: 2.58e-07
par(mfrow=c(3,3))
plot.ts(ltrain$consumption,xlab="log(consumption)",main="1960-1989")
plot(lmfit$resid)
acf(lmfit$resid)
pacf(lmfit$resid)

df_residual=diff(lmfit$resid,1)
plot(df_residual,main="df_residual")
acf(df_residual,main="acf df_residual")
pacf(df_residual,main="pacf df_residual")
# after differencing once, residuals seem ok? test ARIMA(0,1,2) model

# test MA2
fitma2<-arima(lmfit$resid,order=c(0,1,2),method="CSS")
print(fitma2)
# Coefficients:
#   ma1     ma2
# 0.6359  0.1441
# s.e.  0.1839  0.1579
# 
# sigma^2 estimated as 1.673:  part log likelihood = -48.61

# combime estimate regression and ARIMA paraeters
fitma2x<-arima(ltrain$consumption, order=c(0,1,2),xreg=ltrain[,2:3],method="CSS")
print(fitma2x)
# Coefficients:
#   ma1     ma2    price      gdp
# 0.8408  0.4544  -0.0461  34.4739
# s.e.  0.2316  0.1830   0.0452   7.6775
# 
# sigma^2 estimated as 1.364:  part log likelihood = -45.65
par(mfrow=c(2,1))
acf(fitma2x$residuals)
pacf(fitma2x$residuals)

# import out of sample fc error functions


predict(fitma2x,newxreg=lholdo[,2:3] ,n.step=24)$pred


# Time Series:
#   Start = 31 
# End = 54 
# Frequency = 1 

# 31       32       33       34       35       36       37       38       39       40       41       42       43       44 
# 658.9926 659.1229 660.3247 661.3581 662.7566 663.6085 664.7271 666.2930 668.1074 669.4705 670.3969 670.9457 671.4838 672.2841 
# 45       46       47       48       49       50       51       52       53       54 
# 673.2032 673.7238 674.2147 674.4851 673.2836 673.9290 674.0407 673.1855 673.8608 674.6550 


fc<-c(658.9926,659.1229,660.3247,661.3581,662.7566,663.6085,664.7271,666.2930,668.1074,669.4705,670.3969,670.9457,671.4838,672.2841,
  673.2032,673.7238,674.2147,674.4851,673.2836,673.9290,674.0407,673.1855,673.8608,674.6550)
fcerror<-sqrt(sum((fc-lholdo[,1])^2)/24)
fcerror

fclist=matrix(0, nrow = 24, ncol = 2)
fclist[,1]=lholdo[,1]
fclist[,2]=fc
`colnames<-`(fclist, c("lholdo","fc"))

# lholdo       fc
# [1,] 658.16 658.9926
# [2,] 657.13 659.1229
# [3,] 658.30 660.3247
# [4,] 659.09 661.3581
# [5,] 660.07 662.7566
# [6,] 660.09 663.6085
# [7,] 661.58 664.7271
# [8,] 661.94 666.2930
# [9,] 662.95 668.1074
# [10,] 664.81 669.4705
# [11,] 664.93 670.3969
# [12,] 665.06 670.9457
# [13,] 664.87 671.4838
# [14,] 665.86 672.2841
# [15,] 667.76 673.2032
# [16,] 668.00 673.7238
# [17,] 667.33 674.2147
# [18,] 667.23 674.4851
# [19,] 664.16 673.2836
# [20,] 662.07 673.9290
# [21,] 662.59 674.0407
# [22,] 662.01 673.1855
# [23,] 660.76 673.8608
# [24,] 661.48 674.6550
