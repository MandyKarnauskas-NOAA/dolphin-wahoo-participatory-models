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
alldat$spp[which(alldat$spp == "inshore")] <- "inshore spp"

unique(alldat$spp)

alldat$spp <- factor(alldat$spp, 
   levels = c("dolphin", "wahoo", "snapper-grouper", "mackerels", "tunas", "billfish", "trigger", "cobia", "flounder", "inshore spp"))
unique(alldat$spp)

lwds <- colMeans(table(alldat$mon, alldat$spp)) * 1.5
lwds

avdep <- tapply(alldat$scal, list(alldat$mon, alldat$spp), mean)

avdepsm <- c()
for (i in 1:ncol(avdep))  {
    if (sum(avdep[,i]) > 1)  {
    ks <- ksmooth(1:12, avdep[,i], "normal", bandwidth=1.5, range.x=c(1,12), n.points=96) } else {
    ks <- ksmooth(1:12, avdep[,i], "normal", bandwidth=0.75, range.x=c(1,12), n.points=96)      }
    avdepsm <- cbind(avdepsm, ks$y) } 

matplot(avdepsm, type="l", lwd=lwds, lty=rep(1:2, 5), col=rainbow(10), axes=F, ylim=c(0,1.3), ylab="")
mtext(side=2, line=1.5, "dependence on species or group                   ", cex=1.2)
axis(1, at=seq(4, 96, 8), month.abb, cex.axis=1.2); box()
axis(2, at=c(0, 0.1, 1), lab=c("none", "low", "high"), las=2, cex.axis=1.2)
legend("topleft", colnames(avdep), lwd=lwds, lty=rep(1:2, 5), bty="n", col=rainbow(10), ncol=2, x.intersp=1, cex=1.1)
legend(80, 1.3, c("n = 1", "n = 3", "n = 6"), lwd=c(1.5, 4.5, 9), y.intersp = 1.1)
