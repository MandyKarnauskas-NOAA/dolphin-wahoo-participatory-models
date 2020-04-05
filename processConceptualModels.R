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

write.table(d, file=paste0(loc, "_relationships.csv"), append=F, quote=F, row.names=F, col.names=T, sep=",")

d1 <- data.frame(rownames(m))
names(d1) <- "listOfTerms"
d1$newTerm <- ""
d1$comments <- ""

#write.table(d1, file=paste0(loc, "_terms.csv"), append=T, quote=F, row.names=F, col.names=T, sep=",")
#############################################################################

#############################################################################
# Step 3. Fill out description column of the relationships_edited.csv
#  (manual process using notes and transcripts)
#  check conceptual models and update any linkages necessary
#############################################################################

#############################################################################
# Step 4. Merge edited mental models with relationships_edited.csv

rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/model_processing")

# select workshop location

#loc <- "Beaufort" 
#loc <- "Wanchese" 
loc <- "VirginiaBeach" 

m <- read.delim(paste0(loc, "_relationships.csv"), header=T, sep=",")   # specify matrix
me <- read.delim(paste0(loc, "_relationships_edited.csv"), header=T, sep=",")   # specify matrix

dim(m)
dim(me)

me$ref <- paste0(me$influences, me$isInfluenced)
m$ref <- paste0(m$influences, m$isInfluenced)

m1 <- merge(m, me, by="ref", all.x=TRUE)
dim(m1)

m1 <- m1[c(2, 3, 4, 9)]
names(m1) <- c("influences", "isInfluenced", "rel", "description")

write.table(m1, file=paste0(loc, "_relationships_edited.csv"), append=F, quote=F, row.names=F, col.names=T, sep=",")

#############################################################################

#############################################################################
# Step 5. Create list of terms

rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/model_processing")

w1 <- read.table("Beaufort_matrix.csv", header=T, sep=",", row.names=1)       # specify matrix
w2 <- read.table("Wanchese_matrix.csv", header=T, sep=",", row.names=1)       # specify matrix
w3 <- read.table("VirginiaBeach_matrix.csv", header=T, sep=",", row.names=1)       # specify matrix

d <- data.frame(rbind(cbind(rownames(w1), "BFT"), cbind(rownames(w2), "WAN"), cbind(rownames(w3), "VB")))
names(d) <- c("term", "wkshp")
d$newterm <- ""
d$notes <- ""

write.table(d, file="all_terms.csv", append=F, quote=F, row.names=F, col.names=T, sep=",")

##########################################################################

