# DATA INITIALIZATION
# import the data on your own!
states<-(unique(seds_all_states_long$state))
states
state_list<-(as.character.factor(states))
length(state_list)



# Use of petroleum: PATCB.
# Seperate PATCB for 51 states from 1960-2014 and store them in a matrix m.
m=matrix(0, nrow = 55, ncol = length(state_list)+1)
m[,1]=c(1960:2014)

i=1
while(i<=length(state_list)){
  temp=subset(seds_all_states_long, state==state_list[i]& msn=="PATCB")
  m[,i+1]=log(temp$value)
  i=i+1
}

# Put labels on the 52 cols: year, AL,...,DC
colnames(m)=c(1:52)
colnames(m)[1]="year"
i=1
while(i<=length(state_list)){
  colnames(m)[i+1]=state_list[i]
  i=i+1
}

m



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
