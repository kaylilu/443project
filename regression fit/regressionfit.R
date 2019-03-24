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

ltrain<-dfr[1:35,] # 1960-1994
lholdo<-dfr[36:55,] # 1995-2014

# ltrain<-dfr[1:30,]
# lholdo<-dfr[31:54,]
lmfit<-lm(consumption~price+gdp,data=ltrain)
summary(lmfit)
# Call:
#   lm(formula = consumption ~ price + gdp, data = ltrain)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -6.946 -4.727 -2.508  4.579 10.538 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 457.54194   34.55091  13.243 1.57e-14 ***
#   price        -0.05632    0.12168  -0.463    0.647    
# gdp          22.51027    4.08378   5.512 4.48e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.787 on 32 degrees of freedom
# Multiple R-squared:  0.618,	Adjusted R-squared:  0.5941 
# F-statistic: 25.88 on 2 and 32 DF,  p-value: 2.06e-07


par(mfrow=c(3,3))
plot.ts(ltrain$consumption,xlab="log(consumption)",main="1960-1994")
plot(lmfit$resid);acf(lmfit$resid);pacf(lmfit$resid)

df_residual=diff(lmfit$resid,1)
plot(df_residual,main="df_residual");acf(df_residual,main="acf df_residual");pacf(df_residual,main="pacf df_residual")
# after differencing once, residuals seem ok? test ARIMA(0,1,2) model

# test MA2
fitma2<-arima(lmfit$resid,order=c(0,1,2),method="CSS")
print(fitma2)
# Call:
#   arima(x = lmfit$resid, order = c(0, 1, 2), method = "CSS")
# 
# Coefficients:
#   ma1     ma2
# 0.8780  0.4250
# s.e.  0.1649  0.1317
# 
# sigma^2 estimated as 1.315:  part log likelihood = -52.9

# combime estimate regression and ARIMA paraeters
fitma2x<-arima(ltrain$consumption, order=c(0,1,2),xreg=ltrain[,2:3],method="CSS")
print(fitma2x)
# Call:
#   arima(x = ltrain$consumption, order = c(0, 1, 2), xreg = ltrain[, 2:3], method = "CSS")
# 
# Coefficients:
#   ma1     ma2    price      gdp
# 0.8746  0.4756  -0.0450  34.7705
# s.e.  0.2118  0.1708   0.0359   6.6042
# 
# sigma^2 estimated as 1.193:  part log likelihood = -51.24
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


fc<-c(660.6018,661.7351,663.3123,665.1326,666.5152,667.4645,668.0114,668.5560,669.3687,670.3075,670.8541,671.3646,671.6493,670.4746,671.0763,671.2134,670.3955,671.0795,671.8752,673.1484)
fcerror<-sqrt(sum((fc-lholdo[,1])^2)/20)
fcerror # 5.940635
displaylist[,7]=fc
`colnames<-`(displaylist,c("year","holdout","persist","avg","holt-winter","arima","arimax"))


# fclist=matrix(0, nrow = 24, ncol = 2)
# fclist[,1]=lholdo[,1]
# fclist[,2]=fc
# `colnames<-`(fclist, c("lholdo","fc"))

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
