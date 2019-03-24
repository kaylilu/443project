sumPetro <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/18W2/STAT443/project/443project/regression fit/sumPetro.txt", sep="", stringsAsFactors=FALSE)
#sumPetro <- read.csv("sumPetro.txt", sep="", stringsAsFactors=FALSE) #input your sumPetro.txt may need to change directory 
#View(sumPetro)

ltrain<-sumPetro[1:35,] # 1960-1994
lholdo<-sumPetro[36:55,] # 1995-2014
ltrain
lholdo
lholdo_consump<-lholdo[,2]
displaylist=matrix(0,nrow=20,ncol=7)
`colnames<-`(displaylist,c("year","holdout","persist","avg","holt-winter","arima","arimax"))
displaylist[,1]=lholdo[,1]
displaylist[,2]=lholdo[,2]
# ===============================Persistence=============================================
fc_1=c(ltrain[35,2],lholdo[1,2])
fc_1
for(i in 2:20){
  fc_1[i]=lholdo_consump[i-1]
}
fc_1
displaylist[,3]=fc_1
displaylist
#View(displaylist)
fcerror<-sqrt(sum((fc_1-lholdo_consump)^2)/20)
fcerror # 1.19906

# =============================avg of all the past==========================================
avg<-function(train,holdout){
  fcvec=c()
  cumsum=sum(train[,2])
  mse=0
  n=35
  fc=cumsum/n
  fcvec[1]=fc
  zt=holdout[1,2]
  fcerror=zt-fc
  mse=mse+fcerror^2
  for (i in c(2:20)){
    cumsum=cumsum+holdout[i-1,2]
    fc=cumsum/(n+i-1)
    fcvec[i]=fc
    zt=holdout[i,2]
    fcerror=zt-fc
    mse=mse+fcerror^2
  }
  return(fcvec)
  
}
ltrain
vec<-avg(ltrain,lholdo) 
vec
sqrt(sum((vec-lholdo[,2])^2)/20) #9.076252
length(vec)
displaylist[,4]=vec
displaylist
# ==================================HOLT WINTER================================================================
# Holt Winter function with trend
View(sumPetro)
myts<-ts(sumPetro[,2],start=c(1960),end=c(2014),frequency=1)
par(mfrow=c(1,3))
plot(myts,xlab="year",ylab="total petro consumption",main="total consumption of petroleum")
acf(myts,main="acf of total petro consumption");pacf(myts,main="pacf of total petro consumption")
hlfit<-HoltWinters(myts,gamma=F)
hlfit$fitted


alpha_hat=hlfit$alpha

beta_hat=hlfit$beta

vn=660.07

bn=0.9606617

n_holdout=20

#iteration 1

fcvec=c()

sse=0

fc=vn+bn

zt=lholdo_consump[1]

newfc=fc

fcvec[1]=fc

fcerror=zt-fc

sse=sse+fcerror^2

vprev=vn

bprev=bn


#iteration 2 to n_holdout

for (i in 2:n_holdout){
  
  vnew=alpha_hat*lholdo_consump[i-1]+(1-alpha_hat)*fc
  
  bnew=beta_hat*(vnew-vprev)+(1-beta_hat)*bprev
  
  fc=vnew+bnew
  
  fcvec[i]=fc
  
  zt=lholdo_consump[i]
  
  fcerror=zt-fc
  
  sse=sse+fcerror^2
  
  vprev=vnew
  
  bprev=bnew
  
}

fcvec
rmse=sqrt(sse/n_holdout)
#rmse=sqrt(sum((fcvec-lholdo_consump)^2)/n_holdout)
rmse # this is the rmse for linear exponential smoothing, 1.317906
displaylist[,5]=fcvec
displaylist

# =========================================================================================================================

#simple exponential smoothing; Holtwinters with only level
esmfit = HoltWinters(myts[1:35], beta = F, gamma = F)
esmfit$fitted
vn = esmfit$coefficients

# out of sample rmse calculation 1 step forcast
fcvec = c()
sse = 0
fc = vn
zt = lholdo_consump[1]
newv= vn
fcvec[1] = vn
fcerror = fc - zt
sse = sse + fcerror^2
alpha_hat = esmfit$alpha

#h step rmse calculation
for(i in 2:length(lholdo_consump)){
  newv = alpha_hat*lholdo_consump[i-1] + (1 - alpha_hat)*newv
  fc = newv
  fcvec[i] = newv
  zt = lholdo_consump[i]
  fcerror = zt-fc
  sse = sse + fcerror^2
}
rmse=sqrt(sse/length(lholdo_consump))
rmse #1.199091


#=============ARIMA============================
par(mfrow=c(2,3))
myts<-ts(ltrain[,2],start=c(1960),end=c(1994))
plot(myts, main = "petro")
acf(myts, main="acf of petro");pacf(myts, main="pacf of petro")
df<-diff(myts,1)
plot(df,main="differenced petro");acf(df, main="acf of diff");pacf(df,main="pacf of diff")
# test arima (0,1,1)?

fit<-arima(myts,order=c(0,1,1),method="CSS")
fit
# Coefficients:
#   ma1
# 0.5999
# s.e.  0.0984
# 
# sigma^2 estimated as 2.792:  part log likelihood = -65.7
<<<<<<< HEAD
predict(fit,n.ahead=20)$pred
fc_arima<-c(660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,
=======
predict(fit,n.ahead=20, prediction.interval = T)$pred
fc_arima<-c(660.7176,660.7176,660.7176,660.7176,660.7176,60.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,
>>>>>>> e2759d6b5a62865b5400c1314bc74830d8e3325a
            660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176)
length(fc_arima)
sqrt(sum((lholdo[,2]-fc_arima)^2)/20)

displaylist[,6]<-fc_arima
displaylist
<<<<<<< HEAD
`colnames<-`(displaylist,c("year","holdout","persist","avg","holt-winter","arima"))


# ==================ARIMAX==================================
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
#plot.ts(ltrain$consumption,xlab="log(consumption)",main="1960-1995",start=c(1960),end=c(1995))
plot(lmfit$resid,main="residual");acf(lmfit$resid,main="acf residual");pacf(lmfit$resid,main="pacf residual")

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
plot(fitma2x$resid, main="residuals ARIMAX")
acf(fitma2x$resid, main = "acf ARIMAX");pacf(fitma2x$resid, main= "pacf ARIMAX")



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
displaylist

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

=======

#try AR(1)

fitar1 = arima (myts, order = c(1,0,0), method = "CSS")
fitar1
predict(fitar1, n.ahead = 20, prediction.interval = T)

fc_ar1 = c(660.2028,660.3233, 660.4325, 660.5315, 660.6213, 660.7027, 660.7766, 660.8435, 660.9042, 
           660.9593, 661.0092,661.0544, 661.0955, 661.1327, 661.1664, 661.1970, 661.2248, 661.2499, 661.2727, 661.2934)

rmse = sqrt(sum((lholdo_consump -fc_ar1)^2)/length(lholdo_consump))
rmse #3.805953
>>>>>>> e2759d6b5a62865b5400c1314bc74830d8e3325a
