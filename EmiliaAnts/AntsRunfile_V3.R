#------------------------------------------------------#
#-- run code for clickable ant distance calculations --#
#-- Developed by K. Manlove for E. Sola-Garcia and ----#
#-- D. Hughes, 09 November 2013 -----------------------#
#------------------------------------------------------#

#-- source in functions by navigating to AntSource.R --#
source(file.choose())

#-- build overlay grid --#
overlay.grid <- OverlayGrid()

#-- specify the picture you want to look at by navigating --#
#-- to the appropriate jpeg --#
AntPlot(overlay.grid)

#-- label the ants you'll need to select in this image --#
ant.labels <- c("66/99g", "71g", "72h", "22g", "54h", "33h", "55h","11g", "3h","14h", "16h", "8i", "37h", "17h", "2i", "44g", "52g","19i", "32h", "78i", "46g")

#-- run the ant selection module.  The module will tell you --#
#-- which ant to click on next --#
image.width.in <- 10 #width of the chamber measured by Emilia
image.height.in <- 7.5  #height of chamber measured by Emilia-Each chamber is dif
ant.coords <- AntClick(ant.labels, overlay.grid, image.width = image.width.in, image.height = image.height.in)

#-- specify the video code and time stamp for this image
Video.in <- "Video1"
TimeStamp.in <- "Timestamp1"

#-- build the output dataframe --#
ant.distance.data <- AntPostProcess(ant.coords, ant.labels = ant.labels, video.code = Video.in, time.stamp = TimeStamp.in)

ant.coord.data <- ant.distance.data[[2]]
ant.coord.data

ant.coord.data <- ant.distance.data[[2]]
plot(as.numeric(as.character(ant.coord.data$y)) ~ as.numeric(as.character(ant.coord.data$x)))

#-- save the new data frame to the desired directory (Windows and maybe OS-X) --#
#--  you'll be prompted to save two dataframe separately: one containing --#
#-- pairwise distances, and the other containing x-y coordinates for each ant --#
#AntWrite(ant.distance.data = ant.distance.data)

#-- alternative write function for non-Windows operating systems --#
#-- fill in your path in the <specify> region of file.path below. --#
#-----Pairwise distance data sheet
file.path <- "c:/users/Fire Ant Observer/Desktop/Trop/video1pw.csv"
write.csv(ant.distance.data[[1]], file = file.path)

#---(x,y)coordinate data sheet
file.path <- "c:/users/Fire Ant Observer/Desktop/Trop/video1xy.csv"
write.csv(ant.distance.data[[2]], file = file.path)
