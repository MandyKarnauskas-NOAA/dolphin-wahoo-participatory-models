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

sites <- c("Beaufort", "Wanchese", "VirginiaBeach") 

for (loc in sites) { 

  m <- read.table(paste0(loc, "_matrix.csv"), header=T, sep=",", row.names=1, check.names=FALSE)   # specify matrix

  d <- data.frame(matrix(, nrow=0, ncol=3))                                     # empty dataframe to be filled
  for (i in 1: nrow(m)) {                                                       # parse out list of relationships
    b <- which(!is.na(m[i,]))
        d0 <- data.frame(cbind(rep(names(m[i]), length(b)), colnames(m)[b], as.numeric(m[i,b])))
        d <- rbind(d, d0)  } 

  names(d) <- c("influences", "isInfluenced", "rel")
  head(d)                                                                       # list of relationships

  d$description <- ""                                                           # add other columns for next manual step

  write.table(d, file=paste0(loc, "_relationships.csv"), append=F, quote=F, row.names=F, col.names=T, sep=",")
}

#############################################################################

#############################################################################
# Step 3. Fill out description column of the relationships_edited.csv
#  (manual process using notes and transcripts)
#  check conceptual models and update any linkages necessary
#
#############################################################################

#############################################################################
# Step 4. Merge edited mental models with relationships_edited.csv

rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/model_processing")

sites <- c("Beaufort", "Wanchese", "VirginiaBeach")           # select workshop location

for (loc in sites) { 

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
}

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
###   Need to merge updated terms, if above steps are changed!

t0 <- read.table("all_terms.csv", header=T, sep=",", stringsAsFactors = F)
te <- read.table("all_terms_edited.csv", header=T, sep=",", stringsAsFactors = F)

dim(te)
dim(t0)

t1 <- merge(te, t0, by="term", all.y=TRUE)
dim(t1)

t1 <- t1[1:5]
names(t1) <- names(te)

write.table(t1, file="all_terms_edited.csv", append=F, quote=F, row.names=F, col.names=T, sep=",")

#############################################################################

#############################################################################
# Step 7. Update relationship flat file with new terminology

rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/model_processing")

d <- read.delim("all_terms_edited.csv", header=T, sep=",", stringsAsFactors = F)
d$allnewterms <- d$newterm
d$allnewterms[which(d$allnewterms=="")] <- d$term[which(d$allnewterms=="")]

sites <- c("Beaufort", "Wanchese", "VirginiaBeach") 

for (loc in sites)  { 
  m <- read.delim(paste0(loc, "_relationships_edited.csv"), header=T, sep=",", stringsAsFactors = F)   # specify matrix
  m1 <- m

    for (j in 1: nrow(m1))  {
      b1 <- which(d$term == m$influences[j])
      b2 <- which(d$term == m$isInfluenced[j])
      if (length(b1) >= 1) {  m1$influences[j]   <- d$allnewterms[min(b1)]  }  else { print(paste(loc, "j=", j, m$influences[j])) }
      if (length(b2) >= 1) {  m1$isInfluenced[j] <- d$allnewterms[min(b2)]  }  else { print(paste(loc, "j=", j, m$isInfluenced[j])) }
    }
  m1$rel[which(m1$rel==0)] <- NA
  write.table(m1, file=paste0(loc, "_relationships_newterm.csv"), append=F, quote=F, row.names=F, col.names=T, sep=",")
}

#############################################################################
# Step 8. Specify positive and negative relationships

# open "<workshopLocation>_relationships_newterm.csv" files and score "rel" column manually
# save scored files as "<workshopLocation>_relationships_newterm_scored.csv"

#############################################################################

d <- read.delim("Beaufort_relationships_edited.csv", header=T, sep=",", stringsAsFactors = F)

fin <- data.frame(cbind(d$influences, d$isInfluenced))
names(fin) <- c("From", "To")
fin$Group <- 0
fin$Type <- as.factor("U")
fin$Pair <- 1:nrow(fin)

levs <- unique(c(levels(fin$From), levels(fin$To)))
levels(fin$From) <- levs
levels(fin$To) <- levs
    
write.dia(fin, "test7.dia", width = 5, height = 1, self = F)




#############################################################################
# Step 9. Convert back to matrix format

rm(list=ls())

sites <- c("Beaufort", "Wanchese", "VirginiaBeach") 

for (loc in sites)  { 
  
  m <- read.delim(paste0(loc, "_relationships_newterm_scored.csv"), header=T, sep=",", stringsAsFactors = F)   # specify matrix

  lis <- unique(c(m$influences, m$isInfluenced))
  mat <- matrix(data="", nrow=length(lis), ncol=length(lis))

  for (i in 1:nrow(m)) { 
    mat[which(lis == m$influences[i]), which(lis == m$isInfluenced[i])] <- m$rel[i]
  }
  mat <- cbind(lis, mat)
  mat <- rbind(c("", lis), mat)
  
  write.table(mat, file=paste0(loc, "_matrix_newterms.csv"), append=F, quote=F, sep=",", row.names=F, col.names=F)
}

# can input this back into Mental Modeler

#############################################################################










