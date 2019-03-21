
# import Op
Op
write.table(Op, file="/Users/kayli/Desktop/temp/op.txt", row.names=FALSE, col.names=colnames(Op))
sumPetro
sumPetro[,3]=Op[,2]
View(sumPetro)

sumPetro
GDPCA
GDPCA[,2]<-round(GDPCA[,2],2)


sumPetro[,4]=GDPCA[,2]
`colnames<-`(sumPetro,c("year","consumption","price","gdpca"))
write.table(sumPetro, file="/Users/kayli/Desktop/temp/final.txt", row.names=FALSE, col.names=colnames(sumPetro))
sumPetro



