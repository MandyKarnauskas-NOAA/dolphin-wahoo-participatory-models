
rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/stakeholder_plots")

# subset list of plots pertaining to present-day seasonal importance
# Beaufort
lis <- dir("Beaufort")[grep("Annual", dir("Beaufort"))]
lis2 <- lis[grep(".csv", lis)]

dc <- data.frame(matrix(ncol = 5, nrow = 0))

for (i in 1:length(lis2))  { 
  d <- read.table(paste0("Beaufort/", lis2[i]), header=F, sep=",", stringsAsFactors=F)
  d$V4 <- i
  d$V5 <- d$V2 / max(d$V2)
  dc <- rbind(dc, d)
}

names(dc) <- c("year", "raw", "spp", "id", "scal")

plot(dc$year, dc$scal, col=0, pch=20, xlab="year", ylab="", axes=F, ylim=c(0.1,1.2))
mtext(side=2, line=1, "relative local availability"); axis(1); box()
legend ("topleft", 0.88, c("dolphin", "wahoo"), lwd=3, col=c("#FF000090", "#0000FF90"), bty="n")
legend("topright", c("obs 1", "obs 2", "obs 3", "obs 4", "obs5"), lwd=1, lty=1:5, horiz=T, y.intersp=2, bty="n")

for (i in unique(dc$id))  {
  a <- dc[which(dc$id==i & dc$spp == "dolphin"),]
  b <- dc[which(dc$id==i & dc$spp == "wahoo"),]
  if (diff(range(a$year)) < 20) { bdw <- 3 } else { bdw <- 15 }
  
#  ks <- ksmooth(a$year, a$scal, "normal", bandwidth=bdw)
#  lines(ks$x, ks$y, lwd=3, col="#FF000060")
  lines(a$year, a$scal, lwd=2, col="#FF000090", lty=i)
  
#  ks2 <- ksmooth(b$year, b$scal, "normal", bandwidth=bdw)
#  lines(ks2$x, ks2$y, lwd=3,  col="#0000FF60") 
  lines(b$year, b$scal, lwd=2, col="#0000FF90", lty=i)
  }

######################################################################


avs <- tapply(dc$scal, dc$spp, mean)
sds <- tapply(dc$scal, dc$spp, sd)

dc$scaled <- NA

dc$scaled[which(dc$spp=="dolphin")] <- (dc$scal[which(dc$spp=="dolphin")] - avs[1]) - sds[1]
dc$scaled[which(dc$spp=="wahoo")] <- (dc$scal[which(dc$spp=="wahoo")] - avs[3]) - sds[3]

plot(dc$year, dc$scaled, col=0, pch=20, xlab="year", ylab="", axes=F)
mtext(side=2, line=1, "relative abundance"); axis(1); box()
legend (1970, 0.88, c("dolphin", "wahoo"), lwd=3, col=c("#FF000060", "#0000FF60"))

for (i in unique(dc$id))  {
  a <- dc[which(dc$id==i & dc$spp == "dolphin"),]
  b <- dc[which(dc$id==i & dc$spp == "wahoo"),]

  lines(a$year, a$scaled, lwd=3, col="#FF000060")
  lines(b$year, b$scaled, lwd=3, col="#0000FF60")
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

matplot(avdepsm, type="l", lwd=lwds, lty=rep(1:2, 5), col=rainbow(10), axes=F, ylim=c(0,1.12), ylab="")
mtext(side=2, line=1.5, "dependence on species or group", cex=1.2)
axis(1, at=seq(4, 96, 8), month.abb, cex.axis=1.2); box()
axis(2, at=c(0, 0.1, 1), lab=c("none", "low", "high"), las=2, cex.axis=1.2)
legend("top", colnames(avdep), lwd=lwds, lty=rep(1:2, 5), col=rainbow(10), ncol=5, bty="n", x.intersp=0.9, cex=1.1)

