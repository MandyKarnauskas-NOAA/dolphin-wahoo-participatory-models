rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/stakeholder_plots")
#install.packages("digitize")
library(digitize)

setwd("Wanchese")

nam <- dir()[grep(".jpg", dir())][1]
outnam <- paste0(unlist(strsplit(nam, ".jpg")), ".csv")

cal <- ReadAndCal(nam)     #  R will stop after clicking on (x1, x2, y1, y2)

co <- DigitData(col = 'purple')          #  set whatever color you prefer - hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 1, 10)  #  the numbers here should be: (earliest time (Jan or early year), latest time (Dec or last year), min Y, max Y)
m$spp <- "BFT"                        # specify the species
write.table(m, file=outnam, col.names=F, row.names=F, sep=",")  # change file name to match name of jpg

co <- DigitData(col = 'blue')          #  hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 1, 10)
m$spp <- "wahoo"                 # specify the species
write.table(m, file=outnam, col.names=F, row.names=F, sep=",", append = T)

co <- DigitData(col = 'green')          #  hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 1, 10)
m$spp <- "dolphin"                 # specify the species
write.table(m, file=outnam, col.names=F, row.names=F, sep=",", append = T)

co <- DigitData(col = 'orange')          #  hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 1, 10)
m$spp <- "grouper"                 # specify the species
write.table(m, file=outnam, col.names=F, row.names=F, sep=",", append = T)

co <- DigitData(col = 'pink')          #  hit Esc to stop after clicking on points
m <- Calibrate(co, cal, 1, 12, 1, 10)
m$spp <- "cobia"                  # specify the species
write.table(m, file=outnam, col.names=F, row.names=F, sep=",", append = T)

