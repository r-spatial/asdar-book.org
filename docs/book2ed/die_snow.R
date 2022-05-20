### R code from vignette source 'die.Rnw'
### Encoding: UTF-8
# data: sohoSG.tif buildings deaths nb_pump b_pump
# packages. rgdal RColorBrewer geoR
###################################################
### code chunk number 131: die.Rnw:2144-2145
###################################################
library(rgdal)
sohoSG <- readGDAL("sohoSG.tif")
names(sohoSG) <- c("snowcost_broad", "snowcost_not_broad")


###################################################
### code chunk number 133: die.Rnw:2149-2151
###################################################
buildings <- readOGR("buildings.shp", "buildings", integer64="allow.loss")
proj4string(sohoSG) <- CRS(proj4string(buildings))


###################################################
### code chunk number 134: die.Rnw:2168-2173
###################################################
deaths <- readOGR("deaths.shp", "deaths", integer64="allow.loss")
names(deaths) <- c("cat", "long", "lat", "Num_Cases", "snowcost_broad",
 "snowcost_not_broad", "b_nearer")
o <- over(deaths, sohoSG)
library(maptools)
deaths <- spCbind(deaths, o)
deaths$b_nearer <- deaths$snowcost_broad < deaths$snowcost_not_broad


###################################################
### code chunk number 135: die.Rnw:2175-2176
###################################################
by(deaths$Num_Cases, deaths$b_nearer, sum)


###################################################
### code chunk number 136: die.Rnw:2182-2194
###################################################
oopar <- par(mfrow=c(1,2), mar=c(5,3,1,1)+0.1)
b_wid <- table(deaths$b_nearer)
boxplot(snowcost_broad ~ b_nearer, deaths, width=b_wid, ylim=c(0,450),
 ylab="distance", xlab="Broad Street", col=grey.colors(1, 0.8, 0.8, 2.2))
boxplot(snowcost_not_broad ~ b_nearer, deaths, width=b_wid, ylim=c(0,450),
 xlab="Other pump", col=grey.colors(1, 0.8, 0.8, 2.2))
par(oopar)


###################################################
### code chunk number 137: die.Rnw:2203-2205
###################################################
nb_pump <- readOGR("nb_pump.shp", "nb_pump")
b_pump <- readOGR("b_pump.shp", "b_pump")


###################################################
### code chunk number 138: die.Rnw:2229-2277
###################################################
oopar <- par(mar=c(1,1,1,1)+0.1)
library(RColorBrewer)
image(sohoSG, "snowcost_broad", breaks=seq(0,750,50),
 col=colorRampPalette(brewer.pal(7, "Reds"))(15))
plot(buildings, col="white", add=TRUE)
plot(buildings, angle=45, density=10, col="grey70", add=TRUE)
symbols(coordinates(deaths), circles=4*sqrt(deaths$Num_Cases),
 inches=FALSE, add=TRUE, bg=c("brown2","grey40")[deaths$b_nearer+1])
rect(528900, 180550, 529040, 180990, border=NA, col="white")
text(528970, 180950, "metres from\nBroad Street\npump", cex=0.6)
#library(geoR)
#legend.krige(c(528930, 528960), c(180600, 180900),
# sohoSG$snowcost_broad, vertical=TRUE, breaks=seq(0,750,50),
# col=colorRampPalette(brewer.pal(7, "Reds"))(15))
plot(nb_pump, add=TRUE, pch=8, cex=1.3, lwd=2)
plot(b_pump, add=TRUE, pch=4, cex=1.5, lwd=8, col="white")
plot(b_pump, add=TRUE, pch=4, cex=1.5, lwd=6)
rect(528900, 181330, 529140, 181380, border=NA, col="white")
legend(c(528910, 529100), c(181350, 181380),
 legend=c("Broad Street pump","other pumps"), pch=c(4,8), bty="n",
 cex=0.6, y.inter=0.7)
rect(528900, 181270, 529180, 181335, border=NA, col="white")
legend(c(528910, 529100), c(181275, 181325),
 legend=c("nearer Broad Street pump","nearer other pump"),
 fill=c("grey40","brown2"), bty="n", cex=0.6, y.inter=0.7)
box()
par(oopar)


