sumPetro <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/18W2/STAT443/project/443project/regression fit/sumPetro.txt", sep="", stringsAsFactors=FALSE)
View(sumPetro)

ltrain<-sumPetro[1:35,] # 1960-1994
lholdo<-sumPetro[36:55,] # 1995-2014
ltrain
lholdo
lholdo_consump<-lholdo[,2]
displaylist=matrix(0,nrow=20,ncol=4)
`colnames<-`(displaylist,c("year","holdout","persist","holt-winter"))
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
fcerror<-sqrt(sum((fc_1-lholdo_consump)^2)/20)
fcerror # 1.19906


# ==================================HOLT WINTER================================================================
# Holt Winter function with trend
View(sumPetro)
myts<-ts(sumPetro[,2],start=c(1960),end=c(2014),frequency=1)
plot(myts)
# from the plot, we observed that there's no seasonality, but trend.
acf(myts); pacf(myts)
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
displaylist[,4]=fcvec
displaylist


