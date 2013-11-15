#---------------------------------------------------------#
#-- source code for clickable ant distance calculations --#
#-- Developed by K. Manlove for E. Sola-Garcia and -------#
#-- D. Hughes, 09 November 2013 --------------------------#
#---------------------------------------------------------#

#-- check that necessary packages are installed, and install any missing. --#
if("TeachingDemos" %in% rownames(installed.packages()) == FALSE) {install.packages("TeachingDemos")}
if("maptools" %in% rownames(installed.packages()) == FALSE) {install.packages("maptools")}
if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")}
#if("Grid2Polygons" %in% rownames(installed.packages()) == FALSE) {install.packages("Grid2Polygons")}
if("jpeg" %in% rownames(installed.packages()) == FALSE) {install.packages("jpeg")}
if("pixmap" %in% rownames(installed.packages()) == FALSE) {install.packages("pixmap")}

#-- load necessary packages --#
require(TeachingDemos)
require(maptools)
require(sp)
#require(Grid2Polygons)
require(jpeg)
require(pixmap)

OverlayGrid <- function(){
  #-- build a "map" that contains a bunch of grid cells --#
  #-- 1) build data frame of coordinates --#  
  no.x <- 128
  no.y <- 72
  coord.x <- seq(1:no.x)
  coord.y <- seq(1:no.y)
  overlay.coords<- data.frame(expand.grid(coord.x, coord.y))
  names(overlay.coords) <- c("x", "y")

  #-- 2) make this into a spatial polygon object --#
  coordinates(overlay.coords) = overlay.coords
  overlay.pixels <- as(overlay.coords, "SpatialPixelsDataFrame")
  overlay.grid <- as(overlay.pixels, "SpatialGridDataFrame")
  return(overlay.grid)
}
 
AntPlot <- function(overlay.grid = overlay.grid){
  #-- Bring in picture through navigation 
  ant.pic <- readJPEG(file.choose())

  #-- specify transparent color for grid --#
  transparent.col <- rgb(0, 0, 0, alpha = .2)

  #-- plot ant picture as background, and overlay transparent grid --#
  x11(width = 10, height = 8)
  par(mex = .2)
  image(overlay.grid, col = transparent.col)
  rasterImage(ant.pic, xleft = 0, ybottom = 0, xright = 128, ytop = 72)
}

AntClick <- function(ant.labels = ant.labels, overlay.grid = overlay.grid, image.width = image.width, image.height = image.height){
  ants <- rep(NA, length(ant.labels))
  ant.coords <- matrix(NA, nrow = length(ants), ncol = 2)
  
  overlay.grid.mat <- as.matrix(as.data.frame(overlay.grid))
  print(paste("Start clicking with ant ", ant.labels[1], sep = ""))
  
  for(i in 1:length(ant.labels)){
    ants[i] <- identify(coordinates(overlay.grid), n = 1)
    ant.coords[i, ] <- overlay.grid.mat[ants[i], 1:2]
    ifelse(i < length(ant.labels), print(paste("Next ant: ", ant.labels[i + 1], sep = "")), print("Stop"))
  }
  
  #-- rescale and rotate image appropriately --#
  center.points <- rep(NA, 3)
  center.coords <- matrix(NA, nrow = 3, ncol = 2)
  
  overlay.grid.mat <- as.matrix(as.data.frame(overlay.grid))
  print(paste("Click in lower right corner", sep = ""))
  
  for(i in 1:3){
    center.points[i] <- identify(coordinates(overlay.grid), n = 1)
    center.coords[i, ] <- overlay.grid.mat[center.points[i], 1:2]
    ifelse(i == 1, print(paste("Click in lower left corner", sep = "")), ifelse(i == 2, print(paste("Click in upper left corner", sep = "")), print(paste("Stop", sep = ""))))
  }
 
  dev.off()
  origin.coords <- center.coords[2, ]
  pixel.width <- as.vector(dist(center.coords))[1]
  pixel.height <- as.vector(dist(center.coords))[3]
  rotate.angle <- (acos(abs(center.coords[2, 2] - center.coords[3, 2]) / pixel.height))/180
  rotate.ant.x <- cos(rotate.angle) * ant.coords[ ,1] - sin(rotate.angle) * ant.coords[ ,2]
  rotate.ant.y <- sin(rotate.angle) * ant.coords[ ,1] + cos(rotate.angle) * ant.coords[ ,2]
  
  rescale.ant.x <- (rotate.ant.x - center.coords[2, 1]) / (pixel.width / image.width)
  rescale.ant.y <- (rotate.ant.y - center.coords[2, 2]) / (pixel.height / image.height)
  rescale.ant.x.out <- as.numeric(as.character(rescale.ant.x))
  rescale.ant.y.out <- as.numeric(as.character(rescale.ant.y))
  ant.coords.out <- cbind(rescale.ant.x.out, rescale.ant.y.out)
  
  return(ant.coords.out = ant.coords.out)
}
  
AntPostProcess <- function(ant.coords, ant.labels, video.code, time.stamp){
  ant.dists <- dist(ant.coords, diag = F, upper = F)
  out.data.frame <- data.frame(cbind(rep(video.code, dim(t(combn(ant.labels, 2)))[1]), rep(time.stamp, dim(t(combn(ant.labels, 2)))[1]), t(combn(ant.labels, 2)), as.vector(ant.dists)))
  names(out.data.frame) <- c("VideoCode", "TimeStamp", "FocalAnt", "OtherAnt", "Distance")
  ant.coord.out <- data.frame(cbind(rep(video.code, length(ant.labels)), rep(time.stamp, length(ant.labels)), ant.labels, ant.coords))
  names(ant.coord.out) <- c("VideoCode", "TimeStamp", "FocalAnt", "x", "y")
  return(list(out.data.frame = out.data.frame, ant.coord.out = ant.coord.out))
}

AntWrite <- function(ant.distance.data){
  print("Write out pairwise distances")
  write.csv(ant.distance.data[[1]], file = file.choose(new = TRUE))

  print("Write out ant x-y locations")
  write.csv(ant.distance.data[[2]], file = file.choose(new = TRUE))
}
