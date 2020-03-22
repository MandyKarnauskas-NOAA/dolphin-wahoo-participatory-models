#############################################################################
#                                                                           #
#  code for organizing and processing participatory fishery system models   #
#  M. Karnauskas - 22 Mar 2020                                              #
#                                                                           #
#############################################################################

#############################################################################
# Step 1. Put conceptual model into Mental Modeler and export as .csv
# http://www.mentalmodeler.org/#download
# create manually and "Export CSV" - save to "WorkshopLocation_matrix.csv"

#############################################################################
# Step 2. Read file into R and sort matrix into list of relationships

rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/model_processing")

#loc <- "VirginiaBeach"                                                        # select workshop location
#loc <- "Beaufort" 
loc <- "Wanchese" 

m <- read.table(paste0(loc, "_matrix.csv"), header=T, sep=",", row.names=1)   # specify matrix

d <- data.frame(matrix(, nrow=0, ncol=3))                                     # empty dataframe to be filled
for (i in 1: nrow(m)) {                                                       # parse out list of relationships
   b <- which(!is.na(m[i,]))
      d0 <- data.frame(cbind(rep(names(m[i]), length(b)), colnames(m)[b], as.numeric(m[i,b])))
      d <- rbind(d, d0)  } 

names(d) <- c("influences", "isInfluenced", "rel")
head(d)                                                                       # list of relationships

d$description <- ""                                                           # add other columns for next manual step
d$listOfTerms <- c(rownames(m), rep(NA, (nrow(d) - nrow(m))))
d$newTerm <- ""
d$comments <- ""

write.table(d, file=paste0(loc, "_relationships.csv"), append=F, quote=F, row.names=F, col.names=T, sep=",")

#############################################################################
# Step 3. Standardize names across matrices and combine





