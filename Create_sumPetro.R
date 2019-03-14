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

            