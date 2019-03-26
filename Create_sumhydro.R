#=====================================================================================================
#run data intialization code in prep.R before 
which( colnames(mat_hydro)=="RI" )
which( colnames(mat_hydro)=="VA" )
#rename in order to not change original matrix from prep.R
#removed states RI and VA for having irregular data
mat_hydro1 = mat_hydro[,-c(36,43)]

#ran same procedure in Create_sumPetro.R but for hydro
sumhydro=matrix(0, nrow = 55, ncol = 2)
sumhydro[,1]<-mat_hydro1[,1]
sumhydro[,2]<-mat_hydro1[,2]
for(i in 3:ncol(mat_hydro1)){
  sumhydro[,2]=sumhydro[,2]+mat_hydro1[,i]
}
sumhydro[,2]<-round(sumhydro[,2],2)
sumhydro
hydrots = ts(sumhydro[,2], start =c(1960) , end = c(2014), frequency = 1)
#plot graphs for sumhydro and it's differenced time series
par(mfrow=c(2,3))
plot(hydrots); acf(hydrots); pacf(hydrots)
diffhydro = diff(hydrots)
plot(diffhydro); acf(diffhydro) ; pacf(diffhydro)
