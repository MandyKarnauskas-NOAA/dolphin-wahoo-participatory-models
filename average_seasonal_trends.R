rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/stakeholder_plots")

# subset list of plots pertaining to present-day seasonal importance
# Beaufort
lis <- dir("Beaufort")[grep("seasonal", dir("Beaufort"))]
lis2 <- lis[grep(".csv", lis)]

alldat <- data.frame(matrix(ncol = 4, nrow = 0))

for (i in 1:length(lis2))  { 
    d <- read.table(paste0("Beaufort/", lis2[i]), header=F, sep=",", stringsAsFactors=F)
    splis <- as.character(unique(d$V3))
    d$V1 <- round(d$V1)
    for (j in splis)  {
        mismon <- which(1:12 %in% d$V1[which(d$V3==j)]==F)
        if (length(mismon > 0))  {
           nd <- data.frame(cbind(mismon, rep(0, length(mismon)), j))
           names(nd) <- names(d)
           d <- rbind(d, nd)    } 
        }
    d$V1 <- as.numeric(d$V1)
    d$V2 <- as.numeric(d$V2)
    d <- d[order(d$V3, d$V1),]
    d$V4 <- d$V2/max(d$V2)
    d$V5 <- i
    alldat <- rbind(alldat, d)    
    print(table(alldat$V1, alldat$V3))
    }
    
apply((table(alldat$V1, alldat$V3)), 2, mean)
apply((table(alldat$V1, alldat$V3)), 2, sd)

names(alldat) <- c("mon", "orig", "spp", "scal", "ID")
head(alldat)

colMeans(table(alldat$mon, alldat$spp))

alldat$spp <- as.character(alldat$spp)
unique(alldat$spp)

alldat$spp[which(alldat$spp == "KMK")] <- "mackerels"
alldat$spp[which(alldat$spp == "spanish mackerel; bluefish")] <- "mackerels" 
alldat$spp[which(alldat$spp == "grouper")] <- "snapper-grouper"
alldat$spp[which(alldat$spp == "BFT")] <- "tunas"

lwds <- colMeans(table(alldat$mon, alldat$spp))*2

avdep <- tapply(alldat$scal, list(alldat$mon, alldat$spp), mean)

matplot(avdep, type="l", lwd=lwds, lty=c(1:5, 1:5), col=rainbow(10), axes=F, ylim=c(0,1.12), ylab="dependence on species or group")
axis(1, at=1:12, month.abb); box()
axis(2, at=c(0, 0.1, 1), lab=c("none", "low", "high"), las=2)
legend("top", colnames(avdep), lwd=lwds, lty=c(1:5, 1:5), col=rainbow(10), ncol=5, bty="n")
