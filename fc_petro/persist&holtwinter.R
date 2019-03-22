sumPetro <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/18W2/STAT443/project/443project/regression fit/sumPetro.txt", sep="", stringsAsFactors=FALSE)
#sumPetro <- read.csv("sumPetro.txt", sep="", stringsAsFactors=FALSE) #input your sumPetro.txt may need to change directory 
#View(sumPetro)

ltrain<-sumPetro[1:35,] # 1960-1994
lholdo<-sumPetro[36:55,] # 1995-2014
ltrain
lholdo
lholdo_consump<-lholdo[,2]
displaylist=matrix(0,nrow=20,ncol=6)
`colnames<-`(displaylist,c("year","holdout","persist","avg","holt-winter","arima"))
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
par(mfrow=c(3,1))
plot(myts,xlab="year",ylab="total petro consumption",main="total consumption of petroleum")
# from the plot, we observed that there's no seasonality, but trend.
acf(myts,main="acf of total petro consumption"); 
pacf(myts,main="pacf of total petro consumption")
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
par(mfrow=c(3,3))
myts<-ts(ltrain[,2],start=c(1960),end=c(1994))
plot(myts)
acf(myts);pacf(myts)
df<-diff(myts,1)
plot(df);acf(df);pacf(df)
# test arima (0,1,1)?

fit<-arima(myts,order=c(0,1,1),method="CSS")
fit
# Coefficients:
#   ma1
# 0.5999
# s.e.  0.0984
# 
# sigma^2 estimated as 2.792:  part log likelihood = -65.7
predict(fit,n.ahead=20, prediction.interval = T)$pred
fc_arima<-c(660.7176,660.7176,660.7176,660.7176,660.7176,60.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,
            660.7176,660.7176,660.7176,660.7176,660.7176,660.7176,660.7176)
length(fc_arima)
sqrt(sum((lholdo[,2]-fc_arima)^2)/20)

displaylist[,6]<-fc_arima
displaylist

#try AR(1)

fitar1 = arima (myts, order = c(1,0,0), method = "CSS")
fitar1
predict(fitar1, n.ahead = 20, prediction.interval = T)

fc_ar1 = c(660.2028,660.3233, 660.4325, 660.5315, 660.6213, 660.7027, 660.7766, 660.8435, 660.9042, 
           660.9593, 661.0092,661.0544, 661.0955, 661.1327, 661.1664, 661.1970, 661.2248, 661.2499, 661.2727, 661.2934)

rmse = sqrt(sum((lholdo_consump -fc_ar1)^2)/length(lholdo_consump))
rmse #3.805953
