petro <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/18W2/STAT443/project/443project/rebuilt data/petro.txt", sep="")
petro[2]+petro[3]
sumPetro=matrix(0, nrow = 55, ncol = 2)
sumPetro[,1]<-petro[,1]
sumPetro[,2]<-petro[,2]
for(i in 3:ncol(petro)){
  sumPetro[,2]=sumPetro[,2]+petro[,i]
}
sumPetro[,2]<-round(sumPetro[,2],2)
sumPetro
write.table(sumPetro, file="/Users/kayli/Desktop/temp/sumPetro.txt", row.names=FALSE, col.names=c("year","petro"))

myTs<-ts(sumPetro[,2],start=c(1960),end=c(2014),frequency=1)
plot(myTs)
acf(myTs)
pacf(myTs)

# ARIMA FIT Df=1
df1<-diff(myTs,1)
plot(df1)
acf(df1)
pacf(df1)
fit1<-arima(df1,c(0,0,1),method="CSS")
print(fit1) # sigma^2 estimated as 2.095
fit1_1<-arima(myTs,c(0,1,1),method="CSS")
print(fit1_1) # sigma^2 estimated as 2.21

# ARIMA FIT Df=2
df2<-diff(myTs,2)
plot(df2)
acf(df2)
pacf(df2)
fit2<-arima(df2,c(0,0,2),method="CSS")
fit2 # sigma^2 estimated as 1.968
fit2_1<-arima(df2,c(3,0,0),method="CSS")
fit2_1 # 2.784
fit2_2<-arima(df2,c(1,0,1),method="CSS")
fit2_2 # 2.362

sumPetro
dtrain<-sumPetro[1:41,]
dtrain
dhold<-sumPetro[42:nrow(sumPetro),]
dhold

fitarima011<-arima(dtrain[,2],c(0,1,1),method="CSS")
print(fitarima011) # 2.564


fitarima012<-arima(dtrain[,2],c(0,1,2),method="CSS")
print(fitarima012) # 2.19

p<-predict(fitarima012,n.ahead=14)



