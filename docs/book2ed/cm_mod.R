### R code from vignette source 'cm.Rnw'
# data: CRAN051001a.txt seamap105_mod.csv auckland_mapgen.dat
# state.sat.data_mod.txt high.RData auck_el1.RData auck_gshhs.RData
# auck_el2.RData 70042108.tif
###################################################
### code chunk number 8: cm.Rnw:193-194
###################################################
pi * 10^2


###################################################
### code chunk number 9: cm.Rnw:206-207
###################################################
"*"(pi, "^"(10, 2))


###################################################
### code chunk number 10: cm.Rnw:217-218
###################################################
pi * (1:10)^2


###################################################
### code chunk number 11: cm.Rnw:236-240
###################################################
x <- pi * 10^2
x
print(x)
print(x, digits=12)


###################################################
### code chunk number 12: cm.Rnw:250-252
###################################################
class(x)
typeof(x)


###################################################
### code chunk number 13: cm.Rnw:287-291
###################################################
class(cars)
typeof(cars)
names(cars)
summary(cars)


###################################################
### code chunk number 14: cm.Rnw:306-307
###################################################
str(cars)


###################################################
### code chunk number 15: cm.Rnw:352-353
###################################################
class(dist ~ speed)


###################################################
### code chunk number 16: cm.Rnw:366-367
###################################################
lm(dist ~ speed, data=cars)


###################################################
### code chunk number 17: cm.Rnw:380-382
###################################################
cars$qspeed <- cut(cars$speed, breaks=quantile(cars$speed), include.lowest=TRUE)
is.factor(cars$qspeed)


###################################################
### code chunk number 19: cm.Rnw:392-402
###################################################
plot(dist ~ speed, data=cars, main="numerical: scatterplot")
plot(dist ~ qspeed, data=cars, main="factor: boxplots")


###################################################
### code chunk number 20: cm.Rnw:413-414
###################################################
lm(dist ~ qspeed, data=cars)


###################################################
### code chunk number 21: cm.Rnw:485-486
###################################################
library(sp)


###################################################
### code chunk number 22: cm.Rnw:488-489 
###################################################
getClass("Spatial")


###################################################
### code chunk number 24: cm.Rnw:510-511
###################################################
getClass("CRS")


###################################################
### code chunk number 25: cm.Rnw:528-533
###################################################
m <- matrix(c(0,0,1,1), ncol=2, dimnames=list(NULL, c("min", "max")))
crs <- CRS(projargs=as.character(NA))
crs
S <- Spatial(bbox=m, proj4string=crs)
S


###################################################
### code chunk number 27: cm.Rnw:555-558
###################################################
bb <- matrix(c(350, 85, 370, 95), ncol=2, dimnames=list(NULL, c("min", "max")))
res <- try(Spatial(bb, proj4string=CRS("+proj=longlat +ellps=WGS84")))


###################################################
### code chunk number 29: cm.Rnw:627-631
###################################################
CRAN_df <- read.table("CRAN051001a.txt", header=TRUE)
CRAN_mat <- cbind(CRAN_df$long, CRAN_df$lat)
row.names(CRAN_mat) <- 1:nrow(CRAN_mat)
str(CRAN_mat)


###################################################
### code chunk number 31: cm.Rnw:646-647 
###################################################
getClass("SpatialPoints")


###################################################
### code chunk number 33: cm.Rnw:671-674
###################################################
llCRS <- CRS("+proj=longlat +ellps=WGS84")
CRAN_sp <- SpatialPoints(CRAN_mat, proj4string=llCRS)
summary(CRAN_sp)


###################################################
### code chunk number 34: cm.Rnw:701-702
###################################################
bbox(CRAN_sp)


###################################################
### code chunk number 35: cm.Rnw:720-724
###################################################
proj4string(CRAN_sp)
proj4string(CRAN_sp) <- CRS(as.character(NA))
proj4string(CRAN_sp)
proj4string(CRAN_sp) <- llCRS


###################################################
### code chunk number 36: cm.Rnw:739-742
###################################################
brazil <- which(CRAN_df$loc == "Brazil")
brazil
coordinates(CRAN_sp)[brazil,]


###################################################
### code chunk number 37: cm.Rnw:757-758
###################################################
summary(CRAN_sp[brazil,])


###################################################
### code chunk number 38: cm.Rnw:767-769
###################################################
south_of_equator <- which(coordinates(CRAN_sp)[,2] < 0)
summary(CRAN_sp[-south_of_equator,])


###################################################
### code chunk number 39: cm.Rnw:812-813
###################################################
str(row.names(CRAN_df))


###################################################
### code chunk number 40: cm.Rnw:849-853
###################################################
CRAN_spdf1 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df, proj4string=llCRS, match.ID=TRUE)
CRAN_spdf1[4,]
str(CRAN_spdf1$loc)
str(CRAN_spdf1[["loc"]])


###################################################
### code chunk number 41: cm.Rnw:863-867
###################################################
s <- sample(nrow(CRAN_df))
CRAN_spdf2 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df[s,], proj4string=llCRS, match.ID=TRUE)
all.equal(CRAN_spdf2, CRAN_spdf1)
CRAN_spdf2[4,]


###################################################
### code chunk number 42: cm.Rnw:876-878
###################################################
CRAN_df1 <- CRAN_df
row.names(CRAN_df1) <- sample(c(outer(letters, letters, paste, sep="")), nrow(CRAN_df1))


###################################################
### code chunk number 44: cm.Rnw:883-885
###################################################
try(CRAN_spdf3 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df1, proj4string=llCRS, match.ID=TRUE))


###################################################
### code chunk number 45: cm.Rnw:907-908 
###################################################
getClass("SpatialPointsDataFrame")


###################################################
### code chunk number 47: cm.Rnw:925-927
###################################################
names(CRAN_spdf1)
str(model.frame(lat ~ long, data=CRAN_spdf1), give.attr=FALSE)


###################################################
### code chunk number 48: cm.Rnw:937-939
###################################################
CRAN_spdf4 <- SpatialPointsDataFrame(CRAN_sp, CRAN_df)
all.equal(CRAN_spdf4, CRAN_spdf2)


###################################################
### code chunk number 49: cm.Rnw:955-960
###################################################
CRAN_df0 <- CRAN_df
coordinates(CRAN_df0) <- CRAN_mat
proj4string(CRAN_df0) <- llCRS
all.equal(CRAN_df0, CRAN_spdf2)
str(CRAN_df0, max.level=2)


###################################################
### code chunk number 50: cm.Rnw:971-976
###################################################
CRAN_df1 <- CRAN_df
names(CRAN_df1)
coordinates(CRAN_df1) <- c("long", "lat")
proj4string(CRAN_df1) <- llCRS
str(CRAN_df1, max.level=2)


###################################################
### code chunk number 52: cm.Rnw:1002-1004
###################################################
turtle_df <- read.csv("seamap105_mod.csv")
summary(turtle_df)


###################################################
### code chunk number 54: cm.Rnw:1021-1027
###################################################
timestamp <- as.POSIXlt(strptime(as.character(turtle_df$obs_date), "%m/%d/%Y %H:%M:%S"), "GMT")
turtle_df1 <- data.frame(turtle_df, timestamp=timestamp)
turtle_df1$lon <- ifelse(turtle_df1$lon < 0, turtle_df1$lon+360, turtle_df1$lon)
turtle_sp <- turtle_df1[order(turtle_df1$timestamp),]
coordinates(turtle_sp) <- c("lon", "lat")
proj4string(turtle_sp) <- CRS("+proj=longlat +ellps=WGS84")


###################################################
### code chunk number 55: cm.Rnw:1031-1034
###################################################
#library(maptools)
#gshhs.c.b <- system.file("share/gshhs_c.b", package="maptools")
#pac <- Rgshhs(gshhs.c.b, level=1, xlim=c(130,250), ylim=c(15,60), verbose=FALSE)


###################################################
### code chunk number 56: cm.Rnw:1039-1051
###################################################
#plot(pac$SP, axes=TRUE, col="khaki2", xaxs="i", yaxs="i")
#plot(turtle_sp, add=TRUE)
plot(turtle_sp, axes=TRUE)
m_rle <- rle(months(turtle_sp$timestamp))
clen <- cumsum(m_rle$lengths[-length(m_rle$lengths)])-1
crds <- coordinates(turtle_sp)
text(crds[clen,], labels=m_rle$values[-1], pos=3, offset=1.5, srt=45)


###################################################
### code chunk number 57: cm.Rnw:1105-1107
###################################################
getClass("Line")
getClass("Lines")


###################################################
### code chunk number 58: cm.Rnw:1126-1127
###################################################
getClass("SpatialLines")


###################################################
### code chunk number 59: cm.Rnw:1140-1146
###################################################
library(maps)
japan <- map("world", "japan", plot=FALSE)
p4s <- CRS("+proj=longlat +ellps=WGS84")
library(maptools)
SLjapan <- map2SpatialLines(japan, proj4string=p4s)
str(SLjapan, max.level=2)


###################################################
### code chunk number 60: cm.Rnw:1174-1176
###################################################
Lines_len <- sapply(slot(SLjapan, "lines"), function(x) length(slot(x, "Lines")))
table(Lines_len)


###################################################
### code chunk number 61: cm.Rnw:1189-1191
###################################################
volcano_sl <- ContourLines2SLDF(contourLines(volcano))
t(slot(volcano_sl, "data"))


###################################################
### code chunk number 63: cm.Rnw:1217-1220
###################################################
llCRS <- CRS("+proj=longlat +ellps=WGS84")
auck_shore <- MapGen2SL("auckland_mapgen.dat", llCRS)
summary(auck_shore)


###################################################
### code chunk number 65: cm.Rnw:1227-1239
###################################################
lns <- slot(auck_shore, "lines")
islands_auck <- sapply(lns, function(x) {
  crds <- slot(slot(x, "Lines")[[1]], "coords")
  identical(crds[1,], crds[nrow(crds),])
})
islands_sl <- auck_shore[islands_auck]
list_of_Lines <- slot(islands_sl, "lines")
islands_sp <- SpatialPolygons(lapply(list_of_Lines, function(x) {
    Polygons(list(Polygon(slot(slot(x, "Lines")[[1]], "coords"))),
      ID=slot(x, "ID"))
  }),
  proj4string=CRS("+proj=longlat +ellps=WGS84"))


###################################################
### code chunk number 66: cm.Rnw:1263-1276
###################################################
plot(auck_shore)
legend("bottomleft", legend="a)", bty="n")
plot(auck_shore)
plot(islands_sp, add=TRUE, col="grey")
legend("bottomleft", legend="b)", bty="n")


###################################################
### code chunk number 67: cm.Rnw:1301-1308
###################################################
lns <- slot(auck_shore, "lines")
table(sapply(lns, function(x) length(slot(x, "Lines"))))
islands_auck <- sapply(lns, function(x) {
  crds <- slot(slot(x, "Lines")[[1]], "coords")
  identical(crds[1,], crds[nrow(crds),])
})
table(islands_auck)


###################################################
### code chunk number 68: cm.Rnw:1321-1322
###################################################
getClass("Polygon")


###################################################
### code chunk number 69: cm.Rnw:1353-1354
###################################################
getClass("Polygons")


###################################################
### code chunk number 70: cm.Rnw:1379-1380
###################################################
getClass("SpatialPolygons")


###################################################
### code chunk number 71: cm.Rnw:1401-1412
###################################################
islands_sl <- auck_shore[islands_auck]
list_of_Lines <- slot(islands_sl, "lines")
islands_sp <- SpatialPolygons(lapply(list_of_Lines, function(x) {
    Polygons(list(Polygon(slot(slot(x, "Lines")[[1]], "coords"))),
      ID=slot(x, "ID"))
  }),
  proj4string=CRS("+proj=longlat +ellps=WGS84"))
summary(islands_sp)
slot(islands_sp, "plotOrder")
order(sapply(slot(islands_sp, "polygons"),
  function(x) slot(x, "area")), decreasing=TRUE)


###################################################
### code chunk number 72: cm.Rnw:1469-1475
###################################################
library(maps)
state.map <- map("state", plot=FALSE, fill=TRUE)
IDs <- sapply(strsplit(state.map$names, ":"), function(x) x[1])
library(maptools)
state.sp <- map2SpatialPolygons(state.map, IDs=IDs,
  proj4string=CRS("+proj=longlat +ellps=WGS84"))


###################################################
### code chunk number 74: cm.Rnw:1491-1499
###################################################
sat <- read.table("state.sat.data_mod.txt", row.names=5, header=TRUE)
str(sat)
id <- match(row.names(sat), row.names(state.sp))
row.names(sat)[is.na(id)]
sat1 <- sat[!is.na(id),]
state.spdf <- SpatialPolygonsDataFrame(state.sp, sat1)
str(slot(state.spdf, "data"))
str(state.spdf, max.level=2)


###################################################
### code chunk number 77: cm.Rnw:1516-1519
###################################################
rownames(sat1)[2] <- "Arizona"
try(SpatialPolygonsDataFrame(state.sp, sat1))


###################################################
### code chunk number 78: cm.Rnw:1533-1538
###################################################
DC <- "district of columbia"
not_dc <- !(row.names(state.spdf) == DC)
state.spdf1 <- state.spdf[not_dc,]
dim(state.spdf1)
summary(state.spdf1)


###################################################
### code chunk number 79: cm.Rnw:1591-1594
###################################################
#library(maptools)
#high <- Rgshhs("/home/rsb/tmp/gshhs/GSHHS220/gshhs/gshhs_h.b", xlim=c(277,278), ylim=c(45.7,46.2))
#save(high, file="../Data/high.RData")


###################################################
### code chunk number 80: cm.Rnw:1597-1601
###################################################
load("high.RData")
manitoulin_sp <- high$SP


###################################################
### code chunk number 81: cm.Rnw:1603-1606
###################################################
length(slot(manitoulin_sp, "polygons"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "hole"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "ringDir"))


###################################################
### code chunk number 82: cm.Rnw:1643-1644
###################################################
library(rgeos)


###################################################
### code chunk number 83: cm.Rnw:1646-1648
###################################################
manitoulin_sp <- createSPComment(manitoulin_sp)
sapply(slot(manitoulin_sp, "polygons"), comment)


###################################################
### code chunk number 84: cm.Rnw:1674-1687
###################################################
plot(manitoulin_sp, pbg="lightsteelblue2", col="khaki2", usePolypath=FALSE)
text(t(sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "labpt")))[-c(1,2),], label=high$polydata$level[-c(1,2)], col="black", font=2)
cmt <- unlist(strsplit(sapply(slot(manitoulin_sp, "polygons"), comment), " "))
plot(manitoulin_sp, pbg="lightsteelblue2", col="khaki2", usePolypath=FALSE)
text(t(sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "labpt")))[-c(1,2),], label=cmt[-c(1,2)], col="black", font=2)


###################################################
### code chunk number 85: cm.Rnw:1719-1720
###################################################
getClass("GridTopology")


###################################################
### code chunk number 86: cm.Rnw:1732-1739
###################################################
bb <- bbox(manitoulin_sp)
bb
cs <- c(0.01, 0.01)
cc <- bb[,1]+(cs/2)
cd <- ceiling(diff(t(bb))/cs)
manitoulin_grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
manitoulin_grd


###################################################
### code chunk number 87: cm.Rnw:1754-1755 
###################################################
getClass("SpatialGrid")


###################################################
### code chunk number 89: cm.Rnw:1772-1775
###################################################
p4s <- CRS(proj4string(manitoulin_sp))
manitoulin_SG <- SpatialGrid(manitoulin_grd, proj4string=p4s)
summary(manitoulin_SG)


###################################################
### code chunk number 90: cm.Rnw:1781-1794
###################################################
#library(rgdal)
#auck_el1 <- readGDAL("../Data/70042108.tif")
#save(auck_el1, file="../Data/auck_el1.RData")
#library(maptools)
#auck2 <- Rgshhs("/home/rsb/tmp/gshhs/GSHHS220/gshhs/gshhs_f.b", xlim=c(174.2,175.3), ylim=c(-37.5,-36.5), level=1)
#auck_gshhs <- auck2$SP
#auck_gshhs <- createSPComment(auck_gshhs)
#save(auck_gshhs, file="../Data/auck_gshhs.RData")
load("auck_el1.RData")
is.na(auck_el1$band1) <- auck_el1$band1 <= 0
load("auck_gshhs.RData")


###################################################
### code chunk number 91: cm.Rnw:1818-1823
###################################################
class(auck_el1)
slot(auck_el1, "grid")
slot(auck_el1, "bbox")
object.size(auck_el1)
object.size(slot(auck_el1, "data"))


###################################################
### code chunk number 92: cm.Rnw:1833-1835
###################################################
is.na(auck_el1$band1) <- auck_el1$band1 <= 0
summary(auck_el1$band1)


###################################################
### code chunk number 93: cm.Rnw:1870-1875
###################################################
#auck_el2 <- as(auck_el1, "SpatialPixelsDataFrame")
#save(auck_el2, file="../Data/auck_el2.RData")
load("auck_el2.RData")


###################################################
### code chunk number 95: cm.Rnw:1880-1885
###################################################
object.size(auck_el2)
object.size(slot(auck_el2, "grid.index"))
object.size(slot(auck_el2, "coords"))
sum(is.na(auck_el1$band1)) + nrow(slot(auck_el2, "coords"))
prod(slot(slot(auck_el2, "grid"), "cells.dim"))


###################################################
### code chunk number 96: cm.Rnw:1909-1912
###################################################
auck_el_500 <- auck_el2[auck_el2$band1 > 500,]
summary(auck_el_500)
object.size(auck_el_500)


###################################################
### code chunk number 97: cm.Rnw:1932-1938
###################################################
data(meuse.grid)
mg_SP <- SpatialPoints(cbind(meuse.grid$x, meuse.grid$y))
summary(mg_SP)
mg_SPix0 <- SpatialPixels(mg_SP)
summary(mg_SPix0)
prod(slot(slot(mg_SPix0, "grid"), "cells.dim"))


###################################################
### code chunk number 98: cm.Rnw:1955-1957
###################################################
mg_SPix1 <- as(mg_SP, "SpatialPixels")
summary(mg_SPix1)


###################################################
### code chunk number 100: cm.Rnw:1983-1984
###################################################
library(raster)


###################################################
### code chunk number 101: cm.Rnw:1986-1987
###################################################
r <- raster("70042108.tif")


###################################################
### code chunk number 103: cm.Rnw:1992-1998
###################################################
class(r)
inMemory(r)
object.size(r)
cellStats(r, max)
cellStats(r, min)
inMemory(r)


###################################################
### code chunk number 104: cm.Rnw:2012-2024
###################################################
out <- raster(r)
bs <- blockSize(out)
out <- writeStart(out, filename=tempfile(), overwrite=TRUE)
for (i in 1:bs$n) {
    v <- getValues(r, row=bs$row[i], nrows=bs$nrows[i])
    v[v <= 0] <- NA
    writeValues(out, v, bs$row[i])
}
out <- writeStop(out)
cellStats(out, min)
cellStats(out, max)
inMemory(out)


###################################################
### code chunk number 106: cm.Rnw:2044-2052
###################################################
plot(out, col=terrain.colors(100))
plot(auck_gshhs, add=TRUE)


###################################################
### code chunk number 107: cm.Rnw:2069-2073
###################################################
r1 <- as(out, "SpatialGridDataFrame")
summary(r1)
r2 <- as(r1, "RasterLayer")
summary(r2)



