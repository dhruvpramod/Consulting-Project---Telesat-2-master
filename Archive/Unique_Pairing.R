# finding number of flight ID's

TelesatTimeData <- read_csv("TelesatTimeData.csv")


#KJFK -> EGLL
Data3 <- subset(TelesatTimeData, TelesatTimeData$Origin=="KJFK" & TelesatTimeData$Destination=="EGLL")

id <- data.frame(table(Data3$Flight.ID))

ids <- as.vector(id[["Var1"]])

for(i in 1:139){
  assign(paste("flight",i, sep = ""), subset(Data3, Data3$Flight.ID == ids[i]))
}
