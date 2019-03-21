#------------------------------------DATA INITIALIZATION-----------------------------------------
# import data
seds_all_states_long = read.csv("seds_all_states_long.csv")
states<-(unique(seds_all_states_long$state))
states
state_list<-(as.character(states))
length(state_list)
# Use of petroleum: PATCB.
# Seperate PATCB for 51 states from 1960-2014 and store them in a matrix m.
m=matrix(0, nrow = 55, ncol = length(state_list)+1)
m[,1]=c(1960:2014)


for(i in 1:length(state_list)){
  temp=subset(seds_all_states_long, state==state_list[i]& msn=="PATCB")
  m[,i+1]=log(temp$value)
}

# Put labels on the 52 cols: year, AL,...,DC
colnames(m)=c(1:52)
colnames(m)[1]="year"
for(i in 1:length(state_list)){
  colnames(m)[i+1]=state_list[i]
}


write.table(m, file="petro.txt", row.names=FALSE, col.names=colnames(m))

layout(matrix(c(1:9), 3, 3, byrow = TRUE))
plotAts<-function(s){
  myts<-ts(m[,s+1],start=c(1960),end=c(2014),frequency=1)
  plot(myts,main=colnames(m)[s+1])
}
#plots PATCB by state
for (i in 1:51){
  plotAts(i)
}
<<<<<<< HEAD
=======
# Overlay of all PATCB consumption by state
ts.plot(subset(m, select = c(state_list[1:25])),gpars= list(col=rainbow(25)))
ts.plot(subset(m, select = c(state_list[26:51])),gpars= list(col=rainbow(26)))
ts.plot(subset(m, select = c(state_list)),gpars= list(col=rainbow(51)))
>>>>>>> 73d46d01182409edc92fb17f54d89b44f1ef3345

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="/Users/kayli/Desktop/temp/petro") #change this to your local folder


#--------------------------------HYTCB-----------------------------------------
# seperate data for hydroelectric
mat_hydro=matrix(0, nrow = 55, ncol = length(state_list)+1)
mat_hydro[,1]=c(1960:2014)

for(i in 1:length(state_list)){
  temp=subset(seds_all_states_long, state==state_list[i]& msn=="HYTCB")
  mat_hydro[,i+1]=log(temp$value)
}

# Put labels on the 52 cols: year, AL,...,DC
colnames(mat_hydro)=c(1:52)
colnames(mat_hydro)[1]="year"
for(i in 1:length(state_list)){
  colnames(mat_hydro)[i+1]=state_list[i]
}
mat_hydro

# NOTE: By inspection, the following states have irregular data (DE LA MS NJ DC)
# subset(seds_all_states_long, state=="DE"& msn=="HYTCB") # no data
which( colnames(mat_hydro)=="DE" ) #9
# subset(seds_all_states_long, state=="LA"& msn=="HYTCB") # data not complete
which( colnames(mat_hydro)=="LA" ) #19
# subset(seds_all_states_long, state=="MS"& msn=="HYTCB") # no data
which( colnames(mat_hydro)=="MS" ) #25
# subset(seds_all_states_long, state=="NJ"& msn=="HYTCB") # very large neg data
which( colnames(mat_hydro)=="NJ" ) #31
# subset(seds_all_states_long, state=="DC"& msn=="HYTCB") # data not complete
which( colnames(mat_hydro)=="DC" ) #52

mat_hydro<-mat_hydro[,-c(9,19,25,31,52)]
write.table(mat_hydro, file="/Users/kayli/Desktop/temp/hydro/hydro.txt", row.names=FALSE, col.names=colnames(mat_hydro))
layout(matrix(c(1:9), 3, 3, byrow = TRUE))
plotAts<-function(s){
  myts<-ts(mat_hydro[,s+1],start=c(1960),end=c(2014),frequency=1)
  plot(myts,main=colnames(m)[s+1])
}

for (i in 1:(ncol(mat_hydro)-1)){
  plotAts(i)
}

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="/Users/kayli/Desktop/temp/hydro") #change this to your local folder


#--------------------------------BMTCB---------------------------------------------
# seprate data for biomass
mat_biom=matrix(0, nrow = 55, ncol = length(state_list)+1)
mat_biom[,1]=c(1960:2014)

for(i in 1:length(state_list)){
  temp=subset(seds_all_states_long, state==state_list[i]& msn=="HYTCB")
  mat_biom[,i+1]=log(temp$value)
}

# Put labels on the 52 cols: year, AL,...,DC
colnames(mat_biom)=c(1:52)
colnames(mat_biom)[1]="year"
for(i in 1:length(state_list)){
  colnames(mat_biom)[i+1]=state_list[i]
}
mat_biom

mat_biom<-mat_biom[,-c(9,19,25,31,52)]
write.table(mat_biom, file="/Users/kayli/Desktop/temp/biomass/biomass.txt", row.names=FALSE, col.names=colnames(mat_biom))
layout(matrix(c(1:9), 3, 3, byrow = TRUE))
plotAts<-function(s){
  myts<-ts(mat_biom[,s+1],start=c(1960),end=c(2014),frequency=1)
  plot(myts,main=colnames(m)[s+1])
}

for (i in 1:(ncol(mat_biom)-1)){
  plotAts(i)
}

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="/Users/kayli/Desktop/temp/biomass") #change this to your local folder


ts.plot(m,gpars= list(col=m))
# next, label the time series plots for different states 
# output the three time series matrix, store them in text file. 















#------------------------------BELOW: UNUSED CODE------------------------------------------------------------------
#unique(seds_all_states_long$msn)
#subset(seds_all_states_long, state=="OR")
#myts <- ts(log(temp$value), start=c(1960), end=c(2014), frequency=1)
# oregon, hydro elec HYTCB
OR<-subset(seds_all_states_long, state=="OR")
OR

OR_data <- subset(seds_all_states_long, state=="OR"& msn=="HYTCB",)
OR_data2<-cbind(OR_data$year,OR_data$value)

# Washington hydro elec HYTCB
WA<-subset(seds_all_states_long, state=="WA")
WA_data <- subset(seds_all_states_long, state=="WA"& msn=="HYTCB",)
WA_data
WA_data2<-cbind(WA_data$year,WA_data$value)
plot(WA_data2)


# MAINE
ME<-subset(seds_all_states_long, state=="ME")

ME_data <- subset(seds_all_states_long, state=="ME"& msn=="HYTCB",)
ME_data
# 
ME_data2<-cbind(ME_data$year,ME_data$value)
plot(ME_data2)

# PATCB??
