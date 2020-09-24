rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/stakeholder_plots_SA/")
#install.packages("digitize")
library(digitize)

setwd("VirginiaBeach")

nam <- dir()[grep(".jpg", dir())][3]
outnam <- paste0(unlist(strsplit(nam, ".jpg")), ".csv")

cal <- ReadAndCal(nam)     #  R will stop after clicking on (x1, x2, y1, y2)

co <- DigitData(col = 'purple')          #  set whatever color you prefer - hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 0, 10)  #  the numbers here should be: (earliest time (Jan or early year), latest time (Dec or last year), min Y, max Y)
m$spp <- "striper"                        # specify the species
m$x <- round(m$x); m
write.table(m, file=outnam, col.names=F, row.names=F, sep=",")  # change file name to match name of jpg

co <- DigitData(col = 'blue')          #  hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 0, 10)
m$spp <- "flounder"                 # specify the species
m$x <- round(m$x); m
write.table(m, file=outnam, col.names=F, row.names=F, sep=",", append = T)

co <- DigitData(col = 'green')          #  hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 0, 10)
m$spp <- "spades"                 # specify the species
m$x <- round(m$x); m
write.table(m, file=outnam, col.names=F, row.names=F, sep=",", append = T)

co <- DigitData(col = 'orange')          #  hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 0, 10)
m$spp <- "spanish mackerel"                 # specify the species
m$x <- round(m$x); m
write.table(m, file=outnam, col.names=F, row.names=F, sep=",", append = T)

co <- DigitData(col = 'pink')          #  hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 0, 10)
m$spp <- "swordfish"                  # specify the species
m$x <- round(m$x); m
write.table(m, file=outnam, col.names=F, row.names=F, sep=",", append = T)

windows()
d <- read.table(outnam, sep=",", header=F)
d
plot(d[,1], d[,2], type = "b", col = as.numeric (d[,3]))
legend("right", as.character(unique(d[,3])), pch = 1, col = unique(as.numeric(d[,3])))

