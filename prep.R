unique(seds_all_states_long$state)
unique(seds_all_states_long$msn)
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
