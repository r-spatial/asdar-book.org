###################################################
# die_mod.R
# packages: sp, rgdal, maptools, spdep, RColorBrewer, gstat, gpclib
# datasets: 70042108.zip, snow_files.zip
# provided: legend_image.R


###################################################
### chunk number 1: 
###################################################
rm(list=ls())
if ((site <- Sys.getenv("ASDAR_DOWNLOAD")) != "") {
  download.file(paste(site, "70042108.zip", sep="/"), "70042108.zip")
  download.file(paste(site, "snow_files.zip", sep="/"), "snow_files.zip")
}


###################################################
### chunk number 8: 
###################################################
library(rgdal)


###################################################
### chunk number 10: 
###################################################
# From PROJ 6.0.0, EPSG data stored in an SQLite database without proj4 strings
EPSG <- try(make_EPSG())
if (class(EPSG) != "try-error") EPSG[grep("ED50$", EPSG$note),]


###################################################
### chunk number 11: 
###################################################
CRS("+init=epsg:4230")


###################################################
### chunk number 12: 
###################################################
ED50 <- CRS("+init=epsg:4230 +towgs84=-87,-96,-120,0,0,0,0")
ED50


###################################################
### chunk number 13: 
###################################################
IJ.east <- as(char2dms("4d31\'00\"E"), "numeric")
IJ.north <- as(char2dms("52d28\'00\"N"), "numeric")
IJ.ED50 <- SpatialPoints(cbind(x=IJ.east, y=IJ.north), ED50)
res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
x <- as(dd2dms(coordinates(res)[1]), "character")
y <- as(dd2dms(coordinates(res)[2], TRUE), "character")
cat(x, y, "\n")
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat=TRUE)*1000
library(maptools)
gzAzimuth(coordinates(IJ.ED50), coordinates(res))


###################################################
### chunk number 14: 
###################################################
if (class(EPSG) != "try-error") EPSG[grep("Atlas", EPSG$note), 1:2]


###################################################
### chunk number 15:  
###################################################
CRS("+init=epsg:2163")


###################################################
### chunk number 17: 
###################################################
proj <- projInfo("proj")
proj[proj$name == "laea",]
ellps <- projInfo("ellps")
ellps[grep("a=6370997", ellps$major),]


###################################################
### chunk number 18: 
###################################################
IJ.dms.E <- "4d31\'00\"E"
IJ.dms.N <- "52d28\'00\"N"


###################################################
### chunk number 19: 
###################################################
IJ_east <- char2dms(IJ.dms.E)
IJ_north <- char2dms(IJ.dms.N)
IJ_east
IJ_north
getSlots("DMS")


###################################################
### chunk number 20: 
###################################################
c(as(IJ_east, "numeric"), as(IJ_north, "numeric"))


###################################################
### chunk number 21: 
###################################################
download.file("http://web1.sph.emory.edu/users/lwaller/book/ch9/scot.shp", "scot.shp", mode="wb")
#download.file("http://www.sph.emory.edu/~lwaller/book/ch9/scot.shp", "scot.shp", mode="wb")
download.file("http://web1.sph.emory.edu/users/lwaller/book/ch9/scot.dbf", "scot.dbf", mode="wb")
download.file("http://web1.sph.emory.edu/users/lwaller/book/ch9/scot.shx", "scot.shx", mode="wb")
download.file("http://web1.sph.emory.edu/users/lwaller/book/ch2/scotland.dat", "scotland.dat", mode="w")


###################################################
### chunk number 22: 
###################################################
scot_LL <- readOGR(".", "scot")
proj4string(scot_LL) <- CRS("+proj=longlat +ellps=WGS84")
scot_LL$ID


###################################################
### chunk number 23: 
###################################################
scot_dat <- read.table("scotland.dat", skip=1)
names(scot_dat) <- c("District", "Observed", "Expected", "PcAFF", "Latitude", "Longitude")
scot_dat$District
library(maptools)
scot_dat1 <- scot_dat[match(scot_LL$ID, scot_dat$District),]
row.names(scot_dat1) <- sapply(slot(scot_LL, "polygons"), function(x) slot(x, "ID"))
scot_LLa <- spCbind(scot_LL, scot_dat1)
all.equal(scot_LLa$ID, scot_LLa$District)
names(scot_LLa)


###################################################
### chunk number 24: 
###################################################
library(spdep)
O <- scot_LLa$Observed
E <- scot_LLa$Expected
scot_LLa$SMR <- probmap(O, E)$relRisk/100
library(DCluster)
scot_LLa$smth <- empbaysmooth(O, E)$smthrr


###################################################
### chunk number 25: 
###################################################
scot_BNG <- spTransform(scot_LLa, CRS("+init=epsg:27700"))


###################################################
### chunk number 26: 
###################################################
library(RColorBrewer)


###################################################
### chunk number 27: 
###################################################
spplot(scot_BNG, c("SMR", "smth"),
 at=c(0, 0.5, 1, 1.5, 2.5, 7),
 col.regions=grey.colors(5, 0.95, 0.55, 2.2))


###################################################
### chunk number 28: 
###################################################
writeOGR(scot_LLa["ID"], dsn="scot_district.kml", layer="borders",
 driver="KML", overwrite_layer=TRUE)
llCRS <- CRS("+proj=longlat +ellps=WGS84")
scot_SP_LL <- SpatialPointsDataFrame(coordinates(scot_LLa),
 proj4string=llCRS, data=as(scot_LLa, "data.frame")[c("NAME",
 "Observed", "Expected", "SMR", "smth")])
writeOGR(scot_SP_LL, dsn="scot_rates.kml", layer="rates", driver="KML", overwrite_layer=TRUE)


###################################################
### chunk number 29: 
###################################################
drv <- "ESRI Shapefile"
writeOGR(scot_BNG, dsn=".", layer="scot_BNG", driver=drv, overwrite_layer=TRUE)


###################################################
### chunk number 30: 
###################################################
list.files(pattern="^scot_BNG")


###################################################
### chunk number 31: 
###################################################
getinfo.shape("scot_BNG.shp")


###################################################
### chunk number 32: 
###################################################
fname <- unzip(zipfile = "70042108.zip", files="70042108.tif")

###################################################
### chunk number 33: 
###################################################
auck_el1 <- readGDAL("70042108.tif")
summary(auck_el1)
is.na(auck_el1$band1) <- auck_el1$band1 <= 0 | auck_el1$band1 > 1e+4


###################################################
### chunk number 34: 
###################################################
x <- GDAL.open("70042108.tif")
xx <- getDriver(x)
xx
getDriverLongName(xx)
x
dim(x)
GDAL.close(x)


###################################################
### chunk number 35: 
###################################################
GDALinfo("70042108.tif")


###################################################
### chunk number 36: 
###################################################
library(maptools)
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
gridded(meuse.grid) = TRUE
proj4string(meuse.grid) <- CRS(paste("+init=epsg:28992",
 "+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812"))
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- slot(meuse.grid, "proj4string")


###################################################
### chunk number 37: 
###################################################
library(gstat)
log_zinc <- krige(log(zinc)~1, meuse, meuse.grid)["var1.pred"]
proj4string(log_zinc) <- slot(meuse.grid, "proj4string")
summary(log_zinc)


###################################################
### chunk number 38: 
###################################################
writeGDAL(log_zinc, fname="log_zinc.tif", driver="GTiff", type="Float32",
 options="INTERLEAVE=PIXEL")


###################################################
### chunk number 40: 
###################################################
GDALinfo("log_zinc.tif")


###################################################
### chunk number 41: 
###################################################
library(maptools)
grd <- as(meuse.grid, "SpatialPolygons")
proj4string(grd) <- slot(meuse, "proj4string")
gpclibPermit()
require(gpclib)
grd.union <- unionSpatialPolygons(grd, rep("x", length(slot(grd, "polygons"))))
ll <- CRS("+proj=longlat +datum=WGS84")
grd.union.ll <- spTransform(grd.union, ll)


###################################################
### chunk number 42: 
###################################################
llGRD <- GE_SpatialGrid(grd.union.ll)
if (packageVersion("sp") < "1.1.0") {
  llGRD_in <- overlay(llGRD$SG, grd.union.ll)
} else {
  llGRD_in <- over(llGRD$SG, grd.union.ll)
}
llSGDF <- SpatialGridDataFrame(grid=slot(llGRD$SG, "grid"),
 proj4string=slot(llGRD$SG, "proj4string"), data=data.frame(in0=llGRD_in))
llSPix <- as(llSGDF, "SpatialPixelsDataFrame")


###################################################
### chunk number 43: 
###################################################
meuse_ll <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
llSPix$pred <- idw(log(zinc)~1, meuse_ll, llSPix)$var1.pred


###################################################
### chunk number 44: 
###################################################
png(file="zinc_IDW.png", width=llGRD$width, height=llGRD$height,
 bg="transparent")
par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
image(llSPix, "pred", col=bpy.colors(20))
dev.off()
kmlOverlay(llGRD, "zinc_IDW.kml", "zinc_IDW.png")


###################################################
### chunk number 56: 
###################################################
fname <- unzip(zipfile = "snow_files.zip")

buildings <- readOGR(".", "buildings")
sohoSG <- readGDAL("sohoSG.tif")
names(sohoSG) <- c("snowcost_broad", "snowcost_not_broad")


###################################################
### chunk number 57: 
###################################################
deaths <- readOGR(".", "deaths")
proj4string(sohoSG) <- slot(deaths, "proj4string")
if (packageVersion("sp") < "1.1.0") {
  o <- overlay(sohoSG, deaths)
} else {
  slot(sohoSG, "proj4string") <- slot(deaths, "proj4string")
  o <- over(deaths, sohoSG)
}
deaths <- spCbind(deaths, as(o, "data.frame"))
deaths$b_nearer <- deaths$snowcost_broad < deaths$snowcost_not_broad


###################################################
### chunk number 58: 
###################################################
by(deaths$Num_Cases, deaths$b_nearer, sum)


###################################################
### chunk number 59: 
###################################################
oopar <- par(mfrow=c(1,2), mar=c(5,3,1,1)+0.1)
b_wid <- table(deaths$b_nearer)
boxplot(snowcost_broad ~ b_nearer, deaths, width=b_wid, ylim=c(0,450),
 ylab="distance", xlab="Broad Street", col=grey.colors(1, 0.8, 0.8, 2.2))
boxplot(snowcost_not_broad ~ b_nearer, deaths, width=b_wid, ylim=c(0,450),
 xlab="Other pump", col=grey.colors(1, 0.8, 0.8, 2.2))
par(oopar)


###################################################
### chunk number 60: 
###################################################
nb_pump <- readOGR(".", "nb_pump")
b_pump <- readOGR(".", "b_pump")


###################################################
### chunk number 61: 
###################################################
oopar <- par(mar=c(1,1,1,1)+0.1)
gcols <- grey.colors(15, 0.95, 0.55, 2.2)
image(sohoSG, "snowcost_broad", breaks=seq(0,750,50),
 col=gcols)
plot(buildings, col="white", add=TRUE)
plot(buildings, angle=45, density=10, col="grey70", add=TRUE)
symbols(coordinates(deaths), circles=4*sqrt(deaths$Num_Cases),
 inches=FALSE, add=TRUE, bg=c("grey75","grey50")[deaths$b_nearer+1])
source("legend_image.R") #from geoR
rect(528900, 180550, 529040, 180990, border=NA, col="white")
text(528970, 180950, "metres from\nBroad Street\npump", cex=0.6)
legend_image(c(528930, 528960), c(180600, 180900),
 sohoSG$snowcost_broad, vertical=TRUE, breaks=seq(0,750,50),
 col=gcols)
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
 fill=c("grey50","grey75"), bty="n", cex=0.6, y.inter=0.7)
box()
par(oopar)


###################################################
### chunk number 63: 
###################################################
sp2Mondrian(scot_BNG, "scot_BNG.txt")



