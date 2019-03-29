# Step 2: fitting models
sumPetro <- read.csv("sumPetro.txt", sep="", stringsAsFactors=FALSE)
ltrain<-sumPetro[1:35,] # 1960-1994
lholdo<-sumPetro[36:55,] # 1995-2014
lholdo_consump<-lholdo[,2]
displaylist=matrix(0,nrow=20,ncol=7)
`colnames<-`(displaylist,c("year","holdout","persist","avg","holt-winter","arima","arimax"))
displaylist[,1]=lholdo[,1]
displaylist[,2]=lholdo[,2]
# ===============================Persistence==========================================================================
fc_1=c(ltrain[35,2],lholdo[1,2])

for(i in 2:20){
  fc_1[i]=lholdo_consump[i-1]
}

displaylist[,3]=fc_1
displaylist

fcerror<-sqrt(sum((fc_1-lholdo_consump)^2)/20)
fcerror # 1.19906

# =============================avg of all the past=====================================================================

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

vec<-avg(ltrain,lholdo) 

sqrt(sum((vec-lholdo[,2])^2)/20) #9.076252

displaylist[,4]=vec
displaylist

# ==================================HOLT WINTER========================================================================
# Holt Winter function with trend

myts<-ts(sumPetro[,2],start=c(1960),end=c(2014),frequency=1)
par(mfrow=c(1,3))
plot(myts,xlab="year",ylab="total petro consumption",main="total consumption of petroleum")
acf(myts,main="acf of total petro consumption");pacf(myts,main="pacf of total petro consumption")
hlfit<-HoltWinters(myts,gamma=F)

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
rmse=sqrt(sum((fcvec-lholdo_consump)^2)/n_holdout)
rmse
displaylist[,5]=fcvec
displaylist


#============================ARIMA===============================================================================
par(mfrow=c(2,3))
myts<-ts(ltrain[,2],start=c(1960),end=c(1994))
plot(myts, main = "petro")
acf(myts, main="acf of petro");pacf(myts, main="pacf of petro")
df<-diff(myts,1)
plot(df,main="differenced petro");acf(df, main="acf of diff");pacf(df,main="pacf of diff")

fit<-arima(myts,order=c(0,1,1),method="CSS")
fc_arima = predict(fit,n.ahead=20)$pred

rmse = sqrt(sum((lholdo[,2]-fc_arima)^2)/20)
rmse
displaylist[,6]<-fc_arima
displaylist

#=======================ARIMAX=======================================================================================
# assuming common working directory:
final <- read.table("final.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
`colnames<-`(final,c("year","consumption","price","gdp"))
final[,4]<-log(final[,4])
consumption=final[,2]
price=final[,3]
gdp=final[,4]
dfr<-data.frame(consumption,price,gdp)

ltrain<-dfr[1:35,] # 1960-1994
lholdo<-dfr[36:55,] # 1995-2014
lmfit<-lm(consumption~price+gdp,data=ltrain)
summary(lmfit)

#plot residuals
par(mfrow=c(3,3))
plot(lmfit$resid,main="residual");acf(lmfit$resid,main="acf residual");pacf(lmfit$resid,main="pacf residual")

# plot differenced residuals
df_residual=diff(lmfit$resid,1)
plot(df_residual,main="df_residual");acf(df_residual,main="acf df_residual");pacf(df_residual,main="pacf df_residual")

#plot ARIMAX graphs
fitma2x<-arima(ltrain$consumption, order=c(0,1,2),xreg=ltrain[,2:3],method="CSS")
plot(fitma2x$resid, main="residuals ARIMAX");acf(fitma2x$resid, main = "acf ARIMAX")
pacf(fitma2x$resid, main= "pacf ARIMAX")


fc = predict(fitma2x,newxreg=lholdo[,2:3] ,n.step=24)$pred
fcerror<-sqrt(sum((fc-lholdo[,1])^2)/20)
fcerror


# ==================AIMAX with lagged varibales=======================
ts_consumption<-ts(consumption,start=c(1960),end=c(2014))
ts_gdp<-ts(gdp,start=c(1960),end=c(2014))
dconsump<-diff(ts_consumption);dgdp<-diff(ts_gdp)
cross=ccf(dgdp,dconsump,main="cross corr of dgdp&dconsump") # it seem dgdp lags dcomsump?? cosump -> gdp

plot(ts_gdp)
plot(lag(ts_gdp,1))

mydat=ts.intersect(ts_consumption,lag(ts_gdp,1))

fitarma00=arima(mydat[,1],order=c(0,1,0),xreg=mydat[,2])
fitarma00 # 2.625

fitarma10=arima(mydat[,1],order=c(1,1,0),xreg=mydat[,2]) 
fitarma10 # 1.991

fitarma01=arima(mydat[,1],order=c(0,1,1),xreg=mydat[,2])
fitarma01 # 2.015

fitarma11=arima(mydat[,1],order=c(1,1,1),xreg=mydat[,2]) 
fitarma11 # 1.959

fit=arima(mydat[1:35,1],order=c(1,1,1),xreg=mydat[1:35,2]) 
fit

fc<-predict(fit, n.step=19,newxreg=mydat[36:(54),2])$pred
rmse = sqrt(sum((mydat[36:54,1]-fc)^2)/19) # 2.473665
rmse

