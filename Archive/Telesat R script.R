library(zoo)
library(dplyr)
library(tidyr)
library(chron)
library(readxl)

#Departure Distribution
Telesat_Data<- read.csv("Telesat_Data.csv")

Aircraft_Types <- read_excel("Aircraft Types.xlsx")
names(Aircraft_Types)[1] <- "Type"
Types <- as.vector(Aircraft_Types[["Type"]])
TelesatCommercial <- data.frame(subset(Telesat_Data, (Telesat_Data$Type %in% Types) == FALSE))
write.csv(TelesatCommercial, "TelesatCommercial.csv")


TelesatDeparture <- TelesatCommercial
TelesatDepartureDelay <- TelesatDeparture[,c("Origin","Destination","Flight.ID","Filed.Departure.Time..UTC.","Actual.Departure.Time..UTC.","Time..UTC.")]

TelesatDepartureDelay$Filed.Departure.Time..UTC. <- as.character(TelesatDepartureDelay$Filed.Departure.Time..UTC.)
TelesatDepartureDelay <- separate(TelesatDepartureDelay, Filed.Departure.Time..UTC., into = c("Filed Departure Date","Filed Departure Time"), sep= " ")

TelesatDepartureDelay$Actual.Departure.Time..UTC.<- as.character(TelesatDepartureDelay$Actual.Departure.Time..UTC.)
TelesatDepartureDelay<-separate(TelesatDepartureDelay, Actual.Departure.Time..UTC., into = c("Actual Departure Date", "Actual Departure Time"), sep= " ")

TelesatDepartureDelay$Time..UTC.<-as.character(TelesatDepartureDelay$Time..UTC.)
TelesatDepartureDelay <- separate(TelesatDepartureDelay,Time..UTC.,into=c("Date","Time"), sep=" ")

write.csv(TelesatDepartureDelay, "TelesatDDD.csv")
TelesatDDD<-read.csv("TelesatDDD.csv")

ODP <- data.frame(table(TelesatDDD$Origin,TelesatDDD$Destination))
ODP <- ODP[which(ODP$Freq>30000),]
origin <- as.vector(ODP$Var1)
dest <- as.vector(ODP$Var2)

#Time Vector
tv <- seq.POSIXt(as.POSIXct("2019-11-11 00:00:00", tz = "EST"), as.POSIXct("2019-11-11 23:00:00", tz = "EST"), by = "60 min")
t <- strftime(tv, format="%H:%M:%S")
t <- chron(times = t)

for(i in 1:length(origin)) {
  x <- assign(paste(origin[[i]],dest[[i]],sep='.'),subset(TelesatDDD, TelesatDDD$Origin == origin[[i]] & TelesatDDD$Destination==dest[[i]]))
  file <- paste(origin[[i]],dest[[i]],sep='.')
  write.csv(x,paste(file,'csv', sep = '.'))
  id <- data.frame(table(x['Flight.ID']))
  id <- id[which(id[,2]>0),]
  ids <- as.vector(id[['Var1']])
  print(length(id[['Var1']]))
  delay.index<-duplicated(x['Flight.ID'])
  y <- x[!delay.index,c(2:8)]
  delay <- ifelse((as.numeric(difftime(strptime(x = as.character(y$Actual.Departure.Time), format = "%H:%M"),strptime(x = as.character(y$Filed.Departure.Time), format = "%H:%M"), units = 'mins')))<(-60),
                  (as.numeric(difftime(strptime(x = as.character(y$Actual.Departure.Time), format = "%H:%M"),strptime(x = as.character(y$Filed.Departure.Time), format = "%H:%M"), units = 'mins')))%%1440,
                  (as.numeric(difftime(strptime(x = as.character(y$Actual.Departure.Time), format = "%H:%M"),strptime(x = as.character(y$Filed.Departure.Time), format = "%H:%M"), units = 'mins'))))
  print(i)
  td <- strptime(y$Filed.Departure.Time, format = "%H:%M")
  td <- strftime(td, format = "%H:%M:%S")
  td <-chron(times = td)
  departure.index <- findInterval(td, t)
  z <- assign(paste(origin[[i]],dest[[i]],'delays',sep='.'),subset(data.frame(y,delay,departure.index)))
}




# #Assigning Time Vector Indices to Time stamps
# Flight<- Telesat_Data
# flight<-Flight[,c("Origin","Destination", "Flight.ID","Time..UTC.","Latitude","Longitude","Course", "Direction")]
# flight[,4]<-as.character(flight[,4])
# flight <- separate(flight,4,into=c("Date","Time"), sep=" ")
# t <- chron(times=t)
# flight$Time <- chron(times=flight$Time)
# interval.index <- findInterval(flight$Time, t)
# TelesatTimeData <- cbind.data.frame(flight, interval.index)
# write.csv(TelesatTimeData, "TelesatTimeData.csv")



