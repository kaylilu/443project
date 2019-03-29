#------------------------------------DATA INITIALIZATION-----------------------------------------
# import data assume working directory is common
seds_all_states_long = read.csv("seds_all_states_long.csv")
states<-(unique(seds_all_states_long$state))
state_list<-(as.character(states))

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

# Time-series of all PATCB consumption by state
u<-ts(subset(m, select = c(state_list)),start=c(1960),end=c(2014))


#--------------------------------HYTCB---------------------------------------------------------------------------------
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

# NOTE: By inspection, the following states have irregular data (DE LA MS NJ DC)
# delete states with irregular data out of matrix
mat_hydro<-mat_hydro[,-c(9,19,25,31,52)]
state_list1<-state_list[-c(8,18,24,30,51)]

# Time-series of all hydroelectic consumption
h<-ts(subset(mat_hydro, select = c(state_list1)),start=c(1960),end=c(2014))


#--------------------------------BMTCB--------------------------------------------------------------------------------
# seprate data for biomass

mat_biom=matrix(0, nrow = 55, ncol = length(state_list)+1)
mat_biom[,1]=c(1960:2014)

for(i in 1:length(state_list)){
  temp=subset(seds_all_states_long, state==state_list[i]& msn=="BMTCB")
  mat_biom[,i+1]=log(temp$value)
}

# Put labels on the 52 cols: year, AL,...,DC
colnames(mat_biom)=c(1:52)
colnames(mat_biom)[1]="year"
for(i in 1:length(state_list)){
  colnames(mat_biom)[i+1]=state_list[i]
}

mat_biom<-mat_biom[,-c(9,19,25,31,52)]

# Time-series for all biomass consumption
b<-ts(subset(mat_biom, select = c(state_list1)),start=c(1960),end=c(2014))


#---------------------------------------------------------------------------------------------------------------------
# plots the 3 overlay graphs

par(mfrow=c(1,3))
ts.plot(u,gpars= list(col=rainbow(51)),main="Table1. log(petro consumption) for each state",xlab="year",ylab="log petro consumption")
ts.plot(h,gpars= list(col=rainbow(51)),main="Table2. log(hydro consumption) for each state",xlab="year",ylab="log hydro consumption")
ts.plot(b,gpars= list(col=rainbow(51)),
        main="Table3. log(biomass consumption) for each state",
        xlab="year",
        ylab="log biomass consumption")


