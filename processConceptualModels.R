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

#############################################################################

#############################################################################
# Step 6. Standardize list of terms

# manual step - save "all_terms.csv" as "all_terms_edited.csv"
# make any necessary changes to address three issues:  

# (1) standardized wording for interchangeable terms 
#     (e.g. "catch" vs. "landings", "consumer" vs. "customer", "angler" vs. "fisherman", "CPUE" vs. "fishing efficiency")
#
# (2) remove directionality from any terms 
#     (e.g. "lack of regulations" --> "level of regulation", "no stock assessment" --> "ability to assess", "lack of data" --> "availability of data")
#
# (3) make vague terms quantifiable 
#     (e.g., "tournaments" --> "number of tournaments", "wind" --> "wind strength") 
# 
# save final as "all_terms_edited.csv" 
#
#############################################################################

#############################################################################
# Step 7. Replace terms in original matrices

rm(list=ls())
options( warn = 2 )
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/model_processing")

d <- read.table("all_terms_edited.csv", header=T, sep=",", stringsAsFactors = F)

sites <- c("Beaufort", "Wanchese", "VirginiaBeach")                                 # select workshop location

for (loc in sites) {    
  m <- read.table(paste0(loc, "_matrix.csv"), header=F, sep=",", stringsAsFactors = F)   # specify matrix
    
    for (i in 2:length(m$V1))  {   m$V1[i] <- d$allnewterms[min(which(m$V1[i] == d$term))]  }
    for (i in 2:length(m[1,]))  {   m[1,i] <- d$allnewterms[min(which(m[1,i] == d$term))]  }

  write.table(m, file=paste0(loc, "_matrix_newterms.csv"), append=F, quote=F, row.names=F, col.names=F, sep=",")
}




#######  code below doesn't work - Mental Modeler cannot read the .mmp  #########
#install.packages("stringr")
library("stringr")
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/model_processing")

m <- read.delim("Beaufort.mmp", header=F)
m[] <- lapply(m, as.character)

d <- read.table("all_terms_edited.csv", header=T, sep=",")
d1 <- d[which(d$wkshp == "BFT"),]

lis <- grep("<name ><![CDATA", m[,], fixed=T)
for (i in lis)  { 
    k <- which(str_detect(m[i,], as.character(d1$term), negate = FALSE) == TRUE)
    if (sum(str_detect(m[i,], as.character(d1$term), negate = FALSE)) == 1) {
    m[i,] <- str_replace(m[i,], as.character(d1$term[k]), as.character(d1$allnewterms[k]))   
                }
}

output.file <- file("Beaufort_newTerms.mmp", "wb")
write.table(m, row.names = FALSE, col.names = FALSE, file = output.file, 
            quote = FALSE, append = TRUE, sep = "")
close(output.file)
########################### end of code that doesn't work ####################



