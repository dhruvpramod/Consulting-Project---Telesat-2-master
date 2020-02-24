library(zoo)
library(dplyr)
library(tidyr)
library(chron)
library(readxl)
library(tensorr)
library(RColorBrewer)


##---
# Function predicting flight departure delays
# dep.Data: Formatted and cleaned historical Data
# schedule: flight schedule containg origin, destination and scheduled departure time
# Requires t.60 and ODP.list to be run before the function is called
##---
delay.predict <- function(dep.Data,schedule){
  quant.list <- list()
  for(i in 1:length(schedule$Origin)){
    if(paste(schedule$Origin[i],schedule$Destination[i],sep = ".") %in% ODP.list){
      x <- subset(dep.Data, dep.Data$Origin == schedule$Origin[i] & dep.Data$Destination == schedule$Destination[i] & dep.index == findInterval(times(schedule$`Filed Departure Time`[i]),t.60))
      dup.fid <- duplicated(x$Flight.ID)
      x <- x[!dup.fid,]
      delay <- ifelse((as.numeric(difftime(strptime(x = as.character(x$`Actual Departure Time`), format = "%H:%M"),
                                           strptime(x = as.character(x$`Filed Departure Time`), format = "%H:%M"), units = 'mins')))<(-60),
                      (as.numeric(difftime(strptime(x = as.character(x$`Actual Departure Time`), format = "%H:%M"),
                                           strptime(x = as.character(x$`Filed Departure Time`), format = "%H:%M"), units = 'mins')))%%1440,
                      (as.numeric(difftime(strptime(x = as.character(x$`Actual Departure Time`), format = "%H:%M"),
                                           strptime(x = as.character(x$`Filed Departure Time`), format = "%H:%M"), units = 'mins'))))
      quant.list[[i]] <- quantile(delay, probs = seq(0,1,0.05))
      quant.list[[i]][which(is.na(quant.list[[i]]))] <- 0
    }
    else{
      quant.list[[i]] <- rep(0,21)
    }
  }
  delay.quantiles <- data.frame(do.call(rbind,quant.list))
  rx <- runif(length(delay.quantiles[,1]))
  ry <- floor(20*rx)/20
  proportion = (rx-ry)/0.05
  ind.quantile.min <- 20*ry+1
  ind.quantile.max <- ind.quantile.min + 1
  min.delay = delay.quantiles[cbind(seq_len(nrow(delay.quantiles)), ind.quantile.min)]
  max.delay = delay.quantiles[cbind(seq_len(nrow(delay.quantiles)), ind.quantile.max)]
  ODP.delay = round(min.delay + proportion*(max.delay-min.delay))
  which(is.na(ODP.delay))
  return(ODP.delay)
}


##---
# Function creates tensor for 
# h.Data: Historical Data formatted
# s.Data: Flight Schedule with predicted delays 
# Requires t.60 and t.5 list to be run before the function is called
##---
fl.prob.dist <- function(h.data,s.data){
  dep.in60 <- findInterval(times(s.data$`Filed Departure Time`),t.60)
  
  exp.dep <- strptime(s.data$`Filed Departure Time`, format = "%H:%M:%S",tz = "UTC") + s.data$delays*60
  exp.date <- as.Date(exp.dep, tz = "UTC")
  exp.time <- strftime(exp.dep, format = "%H:%M:%S", tz = "UTC")
  
  dep.in5 <- ifelse(exp.date == Sys.Date(),findInterval(times(exp.time),t.5),0)
  
  tnsr.x <- ceiling(max(h.data$lat.index) - min(h.data$lat.index))+5
  tnsr.y <- ceiling(max(h.data$lon.index) - min(h.data$lon.index))+5
  tnsr.z <- 288
  
  fl.tensor <- array(0,c(tnsr.x,tnsr.y,tnsr.z))
  
  for(i in 1:length(s.data[,1])){
    if(exp.date[i] == Sys.Date()){
      sub.data <- subset(h.data, h.data$Origin == s.data$Origin[i] & h.data$Destination == s.data$Destination[i])
      init.tensor <- array(0,c(tnsr.x,tnsr.y,tnsr.z))
      sub.data$ts.index <- sub.data$ts.index + dep.in5[i]
      for(j in 1:length(sub.data[,1])){
        if(sub.data$ts.index[j] <= tnsr.z){
          init.tensor[sub.data$lat.index[j],sub.data$lon.index[j],sub.data$ts.index[j]] = init.tensor[sub.data$lat.index[j],sub.data$lon.index[j],sub.data$ts.index[j]] + 1
        }
      }
    }
    fl.tensor <- fl.tensor + init.tensor
  }
  return(fl.tensor)
}

start.time <- Sys.time()

## Import Historical Flight Data
Telesat_Data<- read.csv("Telesat_Data.csv")

## Creating new dataset with commercial flights only
# Import Aircarft Type Data
Aircraft_Types <- read_excel("Aircraft Types.xlsx")
names(Aircraft_Types)[1] <- "Type"
Types <- as.vector(Aircraft_Types[["Type"]])

##---
# Data Cleaning and Formatting
##---
TelesatCommercial <- data.frame(subset(Telesat_Data, (Telesat_Data$Type %in% Types) == FALSE))

## Creating new dataset with required variables only
TelesatDepartureDelay <- TelesatCommercial[,c("Origin","Destination","Flight.ID","Filed.Departure.Time..UTC.","Actual.Departure.Time..UTC.","Time..UTC.","Latitude","Longitude")]

TelesatDepartureDelay$Filed.Departure.Time..UTC. <- as.character(TelesatDepartureDelay$Filed.Departure.Time..UTC.)
TelesatDepartureDelay <- separate(TelesatDepartureDelay, Filed.Departure.Time..UTC., into = c("Filed Departure Date","Filed Departure Time"), sep= " ")

TelesatDepartureDelay$Actual.Departure.Time..UTC.<- as.character(TelesatDepartureDelay$Actual.Departure.Time..UTC.)
TelesatDepartureDelay<-separate(TelesatDepartureDelay, Actual.Departure.Time..UTC., into = c("Actual Departure Date", "Actual Departure Time"), sep= " ")

TelesatDepartureDelay$Time..UTC.<-as.character(TelesatDepartureDelay$Time..UTC.)
TelesatDepartureDelay <- separate(TelesatDepartureDelay,Time..UTC.,into=c("Date","Time"), sep=" ")

## Time and Delay Indices
## Creating time vector with 1hr intervals
t.vec <- seq.POSIXt(as.POSIXct("2019-11-11 00:00:00", tz = "EST"), as.POSIXct("2019-11-11 23:00:00", tz = "EST"), by = "60 min")
t.60 <- strftime(t.vec, format="%H:%M:%S")
t.60 <- chron(times = t.60)

## Creating time indices for departure time
td <- strptime(TelesatDepartureDelay$`Filed Departure Time`, format = "%H:%M")
td <- strftime(td, format = "%H:%M:%S")
td <-chron(times = td)
dep.index <- findInterval(td, t.60)

## Creating time vector with 5min intervals
t.vec <- seq.POSIXt(as.POSIXct("2019-11-11 00:00:00", tz = "EST"), as.POSIXct("2019-11-11 23:00:00", tz = "EST"), by = "5 min")
t.5 <- strftime(t.vec, format="%H:%M:%S")
t.5 <- chron(times = t.5)

# Creating time indices for the tensor
TelesatDepartureDelay$Date <- as.Date(TelesatDepartureDelay$Date, format = "%m/%d/%Y")
tdiff <- as.numeric(difftime(paste(TelesatDepartureDelay$Date,TelesatDepartureDelay$Time,sep = " "),
                             paste(TelesatDepartureDelay$`Actual Departure Date`,paste(TelesatDepartureDelay$`Actual Departure Time`,"00",sep = ":"),sep = " ")),units = "mins")
ts.index <- ifelse(tdiff<=0 | tdiff/60>24,0,findInterval(times(paste(floor(tdiff/60),floor(tdiff%%60),"00",sep = ":")),t.5))


## Creating latitude and longitude Indices for the tensor
max.lon <- ceiling(TelesatDepartureDelay$Longitude[which.max(TelesatDepartureDelay$Longitude)])
min.lon <- floor(TelesatDepartureDelay$Longitude[which.min(TelesatDepartureDelay$Longitude)])

max.lat <- ceiling(TelesatDepartureDelay$Latitude[which.max(TelesatDepartureDelay$Latitude)])
min.lat <- floor(TelesatDepartureDelay$Latitude[which.min(TelesatDepartureDelay$Latitude)])

lat.index <- floor((TelesatDepartureDelay$Latitude - min.lat)/2)
lon.index <- floor((TelesatDepartureDelay$Longitude - min.lon)/2)

TelesatDepartureDelay <- cbind.data.frame(TelesatDepartureDelay,dep.index,lat.index,lon.index,ts.index)

dup.f.id <-  duplicated(TelesatDepartureDelay$Flight.ID)
bad.flights <- TelesatDepartureDelay[!dup.f.id, c("Flight.ID","ts.index")]
bad.flights <- bad.flights[which(bad.flights$ts.index>3),]

TelesatDepartureDelay <- TelesatDepartureDelay[which((TelesatDepartureDelay$Flight.ID %in% as.vector(bad.flights$Flight.ID))==FALSE),]
TelesatDepartureDelay <- TelesatDepartureDelay[which(TelesatDepartureDelay$ts.index>0),]

dup.f.id <-  duplicated(TelesatDepartureDelay$Flight.ID)
comp.Data <- TelesatDepartureDelay %>% select(-Time,-Latitude,-Longitude,-lat.index,-lon.index,-ts.index)
comp.Data <- comp.Data[!dup.f.id,]
x <- TelesatDepartureDelay %>% group_by(Flight.ID,ts.index) %>% summarize(lat.index = mean(lat.index),lon.index = mean(lon.index))
TelesatDepartureDelay <- merge(x,comp.Data, by = "Flight.ID")
TelesatDepartureDelay$lat.index <- round(TelesatDepartureDelay$lat.index)
TelesatDepartureDelay$lon.index <- round(TelesatDepartureDelay$lon.index)

#write.csv(TelesatDepartureDelay,"TelesatDepartureDelay.csv")

##---
# XXXXX
##---

##---
# Creating Traing and Testing Data
##---

## Creating a list of ODPs
ODP <- data.frame(table(TelesatDepartureDelay$Origin,TelesatDepartureDelay$Destination))
ODP <- ODP[which(ODP$Freq>0),]
ODP.list <- paste(ODP$Var1,ODP$Var2,sep=".")

## Choosing training set flights
train.ids <- vector()
for(i in 1:length(ODP[,1])){
  t.subset <- subset(TelesatDepartureDelay,
                     TelesatDepartureDelay$Origin == ODP$Var1[i] & TelesatDepartureDelay$Destination == ODP$Var2[i])
  f.id <- data.frame(table(t.subset$Flight.ID))
  f.id <- f.id[which(f.id[,2]>0),]
  if(length(f.id[,1])<2){
    f.ids <- as.vector(f.id[,1])
  } else{
    f.ids <- sample(as.vector(f.id[,1]),floor(0.8*length(f.id[,1])))
  }
  train.ids <- c(train.ids,f.ids)
}

train.Data <- subset(TelesatDepartureDelay,TelesatDepartureDelay$Flight.ID %in% train.ids)

test.Data <- subset(TelesatDepartureDelay,(TelesatDepartureDelay$Flight.ID %in% train.ids)==FALSE)
dup.fid <- duplicated(test.Data$Flight.ID)
test.Data <- test.Data[!dup.fid,c("Origin","Destination","Filed Departure Time")]
test.Data$`Filed Departure Time` <- strftime(strptime(test.Data$`Filed Departure Time`,format = "%H:%M"),format = "%H:%M:%S")

delays <- delay.predict(train.Data,test.Data)

test.Data <- cbind.data.frame(test.Data,delays)

##---
# XXXXX
##---

##---
# Validation
##---

val.Data <- subset(TelesatDepartureDelay,(TelesatDepartureDelay$Flight.ID %in% train.ids)==FALSE)

tdep <- strptime(val.Data$`Actual Departure Time`, format = "%H:%M",tz = "UTC")
tdep <- strftime(tdep,format = "%H:%M:%S", tz = "UTC")
ttime <- findInterval(times(tdep),t.5) + val.Data$ts.index

val.tensor <- array(0, c(length(flight.dist[,1,1]),length(flight.dist[1,,1]),288))

for (i in 1:length(val.Data$Flight.ID)) {
  print(i)
  if(ttime[i]<=288){
    val.tensor[val.Data$lat.index[i],val.Data$lon.index[i],ttime[i]] <- val.tensor[val.Data$lat.index[i],val.Data$lon.index[i],ttime[i]] + 1
  }
}

error <-rowSums(abs(val.tensor - flight.dist), dims = 2)/288

flight.dist <- fl.prob.dist(train.Data,test.Data)

flight.dist[which(flight.dist==0)] = -100

places <- list()
places[[1]] <- c(floor((51.5-min.lat)/2),floor((-0.1-min.lon)/2))
places[[2]] <- c(floor((48.9-min.lat)/2),floor((2.4-min.lon)/2))
places[[3]] <- c(floor((40.7-min.lat)/2),floor((-74-min.lon)/2))
places[[4]] <- c(floor((55.8-min.lat)/2),floor((37.6-min.lon)/2))
places[[5]] <- c(floor((34.1-min.lat)/2),floor((-118.2-min.lon)/2))
places[[6]] <- c(floor((43.7-min.lat)/2),floor((-79.4-min.lon)/2))
pl.names <- c("London", "Paris", "NY", "Moscow", "LA", "Toronto")

pl.d <- data.frame(do.call(rbind,places))

for(i in 1:6){
  flight.dist[pl.d$X1[i],pl.d$X2[i],] = 500
}

for(i in 1:288){
  png(filename = paste("t",i,".png",sep = ""))
  heatmap(flight.dist[,,i],Rowv = NA, Colv = NA,scale = "none",keep.dendro = FALSE,col=brewer.pal(n=9,name = "Spectral"))
  dev.off()
}


end.time <- Sys.time()
print(end.time - start.time)



