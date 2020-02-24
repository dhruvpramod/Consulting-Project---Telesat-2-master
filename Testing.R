places <- list()
places[[1]] <- c(floor((51.5-min.lat)/2),floor((-0.1-min.lon)/2))
places[[2]] <- c(floor((48.9-min.lat)/2),floor((2.4-min.lon)/2))
places[[3]] <- c(floor((40.7-min.lat)/2),floor((-74-min.lon)/2))
places[[4]] <- c(floor((55.8-min.lat)/2),floor((37.6-min.lon)/2))
places[[5]] <- c(floor((34.1-min.lat)/2),floor((-118.2-min.lon)/2))
places[[6]] <- c(floor((43.7-min.lat)/2),floor((-79.4-min.lon)/2))

pl.names <- c("London", "Paris", "NY", "Moscow", "LA", "Toronto")

pl.d <- data.frame(do.call(rbind,places))

pl.a <- array(0, c(length(flight.dist[,1,1]),length(flight.dist[1,,1])))

for (i in 1:6){
  pl.a[pl.d$X1[i],pl.d$X2[i]] = pl.a[pl.d$X1[i],pl.d$X2[i]] + 1
}

heatmap(pl.a,Rowv = NA, Colv = NA,scale = "none",keep.dendro = FALSE,col=c("white","red"))
