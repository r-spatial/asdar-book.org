### R code from vignette source 'die.Rnw'
### Encoding: UTF-8
# data: scotland.dat scot geohub.RData fires_120104 70042108.tif getosm.RData osm_bergen_120105.tif RgoogleMaps.RData osmar.RData 
###################################################
### code chunk number 9: die.Rnw:95-96
###################################################
#library(rgdal)


###################################################
### code chunk number 14: die.Rnw:264-266
###################################################
# From PROJ 6.0.0, EPSG data stored in an SQLite database without proj4 strings
#EPSG <- try(make_EPSG())
#if (class(EPSG) != "try-error") EPSG[grep("^# ED50$", EPSG$note),]

library(sf)
pths <- sf_proj_search_paths()
library(RSQLite)
db <- dbConnect(SQLite(), dbname=file.path(pths[length(pths)], "proj.db"))
GCRSi <- dbGetQuery(db, "SELECT auth_name, code, name FROM geodetic_crs WHERE typeof(code) = 'integer'")
GCRSi$code <- as.character(GCRSi$code)
GCRSt <- dbGetQuery(db, "SELECT auth_name, code, name FROM geodetic_crs WHERE typeof(code) = 'text'")
PCRSi <- dbGetQuery(db, "SELECT auth_name, code, name FROM projected_crs WHERE typeof(code) = 'integer'")
PCRSi$code <- as.character(PCRSi$code)
PCRSt <- dbGetQuery(db, "SELECT auth_name, code, name FROM projected_crs WHERE typeof(code) = 'text'")
dbDisconnect(db)
GPCRS <- rbind(GCRSi, GCRSt, PCRSi, PCRSt)
GPCRS[grep("^ED50$", GPCRS$name),]

###################################################
### code chunk number 15: die.Rnw:306-307
###################################################
#CRS("+init=epsg:4230")
library(sp)
CRS("EPSG:4230")

###################################################
### code chunk number 16: die.Rnw:330-332
###################################################
ED50 <- CRS("EPSG:4230")
ED50


###################################################
### code chunk number 17: die.Rnw:358-368
###################################################
IJ.east <- as(char2dms("4d31\'00\"E"), "numeric")
IJ.north <- as(char2dms("52d28\'00\"N"), "numeric")
IJ.ED50 <- SpatialPoints(cbind(x=IJ.east, y=IJ.north), proj4string=ED50)
res <- as(st_transform(st_as_sf(IJ.ED50), st_crs(CRS("EPSG:4326"))), "Spatial")
x <- as(dd2dms(coordinates(res)[1]), "character")
y <- as(dd2dms(coordinates(res)[2], TRUE), "character")
cat(x, y, "\n")
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat=TRUE)*1000
library(geosphere)
#gzAzimuth(coordinates(IJ.ED50), coordinates(res))
bearing(coordinates(IJ.ED50), coordinates(res), a=6370997, f=0)

###################################################
### code chunk number 19: die.Rnw:430-434
###################################################
proj4string(IJ.ED50) <- CRS("EPSG:4230")
res <- as(st_transform(st_as_sf(IJ.ED50), st_crs(CRS("EPSG:4326"))), "Spatial")
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat=TRUE)*1000
#gzAzimuth(coordinates(IJ.ED50), coordinates(res))
bearing(coordinates(IJ.ED50), coordinates(res), a=6370997, f=0)

###################################################
### code chunk number 21: die.Rnw:449-450
###################################################
#if (class(EPSG) != "try-error") EPSG[grep("Atlas", EPSG$note), 1:2]
GPCRS[grep("US National Atlas", GPCRS$name),]

###################################################
### code chunk number 22: die.Rnw:452-453 
###################################################
CRS("EPSG:2163")


###################################################
### code chunk number 24: die.Rnw:475-479
###################################################
#proj <- projInfo("proj")
#proj[proj$name == "laea",]
#ellps <- projInfo("ellps")
#ellps[grep("a=6370997", ellps$major),]


###################################################
### code chunk number 25: die.Rnw:521-523
###################################################
IJ.dms.E <- "4d31\'00\"E"
IJ.dms.N <- "52d28\'00\"N"


###################################################
### code chunk number 26: die.Rnw:533-538
###################################################
IJ_east <- char2dms(IJ.dms.E)
IJ_north <- char2dms(IJ.dms.N)
IJ_east
IJ_north
getSlots("DMS")


###################################################
### code chunk number 27: die.Rnw:552-553
###################################################
c(as(IJ_east, "numeric"), as(IJ_north, "numeric"))


###################################################
### code chunk number 28: die.Rnw:672-673
###################################################
head(st_drivers(), n=10)


###################################################
### code chunk number 31: die.Rnw:733-735
###################################################
scot_dat <- read.table("scotland.dat", skip=1)
names(scot_dat) <- c("District", "Observed", "Expected", "PcAFF", "Latitude", "Longitude")


###################################################
### code chunk number 34: die.Rnw:760-761
###################################################
#ogrInfo(".", "scot")


###################################################
### code chunk number 37: die.Rnw:791-794
###################################################
#scot_LL <- readOGR(dsn="scot.shp", layer="scot", integer64="allow.loss")
scot_LL <- as(st_read("scot.shp"), "Spatial")
proj4string(scot_LL)
proj4string(scot_LL) <- CRS("EPSG:4326")


###################################################
### code chunk number 39: die.Rnw:803-805
###################################################
sapply(slot(scot_LL, "data"), class)
scot_LL$ID


###################################################
### code chunk number 40: die.Rnw:817-825
###################################################
scot_dat$District
ID_D <- match(scot_LL$ID, scot_dat$District)
scot_dat1 <- scot_dat[ID_D,]
row.names(scot_dat1) <- row.names(scot_LL)
#library(maptools)
#scot_LLa <- spCbind(scot_LL, scot_dat1)
scot_LLa <- as(merge(st_as_sf(scot_LL), scot_dat1, by.x="ID", by.y="District"), "Spatial")
#all.equal(scot_LLa$ID, scot_LLa$District)
names(scot_LLa)


###################################################
### code chunk number 41: die.Rnw:846-852
###################################################
library(spdep) # drags in raster via spData
O <- scot_LLa$Observed
E <- scot_LLa$Expected
scot_LLa$SMR <- probmap(O, E)$relRisk/100
library(DCluster)
scot_LLa$smth <- empbaysmooth(O, E)$smthrr


###################################################
### code chunk number 42: die.Rnw:863-864
###################################################
scot_BNG <- as(st_transform(st_as_sf(scot_LLa), st_crs(CRS("EPSG:27700"))), "Spatial")


###################################################
### code chunk number 43: die.Rnw:868-869
###################################################
library(RColorBrewer)


###################################################
### code chunk number 44: die.Rnw:874-887
###################################################
spplot(scot_BNG, c("SMR", "smth"),
 at=c(0, 0.25, 0.5, 0.8, 1, 1.5, 2.5, 4.5, 7),
 col.regions=rev(brewer.pal(8, "RdBu")))


###################################################
### code chunk number 45: die.Rnw:907-909
###################################################
#drv <- "ESRI Shapefile"
#writeOGR(scot_BNG, dsn=".", layer="scot_BNG", driver=drv, overwrite_layer=TRUE)
st_write(st_as_sf(scot_BNG), "scot_BNG.shp", delete_layer=TRUE)


###################################################
### code chunk number 46: die.Rnw:911-912
###################################################
list.files(pattern="^scot_BNG")


###################################################
### code chunk number 47: die.Rnw:941-942
###################################################
load("geohub.RData")


###################################################
### code chunk number 48: die.Rnw:944-946 (eval = FALSE)
###################################################
## dsn <- "WFS:http://geohub.jrc.ec.europa.eu/effis/ows"
## ogrListLayers(dsn)


###################################################
### code chunk number 49: die.Rnw:949-950
###################################################
print(layers)


###################################################
### code chunk number 50: die.Rnw:952-953 (eval = FALSE)
###################################################
## Fires <- readOGR(dsn, "EFFIS:FiresAll")


###################################################
### code chunk number 51: die.Rnw:955-956
###################################################
geohub


###################################################
### code chunk number 53: die.Rnw:961-962
###################################################
#Fires <- readOGR("fires_120104.shp", "fires_120104")
Fires <- as(st_read("fires_120104.shp"), "Spatial")

###################################################
### code chunk number 55: die.Rnw:967-968
###################################################
names(Fires)


###################################################
### code chunk number 56: die.Rnw:986-996
###################################################
x <- c(-15, -15, 38, 38, -15)
y <- c(28, 62, 62, 28, 28)
crds <- cbind(x=x, y=y)
bb <- st_as_sf(SpatialPolygons(list(Polygons(list(Polygon(coords=crds)), "1"))))
#library(maptools)
#data(wrld_simpl)
data(world, package="spData") # spData drags in raster
st_crs(bb) <- st_crs(world)
#proj4string(bb) <- CRS(proj4string(wrld_simpl))
#library(rgeos)
s2_status <- sf_use_s2()
sf_use_s2(FALSE)
slbb <- as(st_intersection(bb, st_cast(world, "MULTILINESTRING")), "Spatial")
sf_use_s2(s2_status)
#slbb <- gIntersection(bb, as(wrld_simpl, "SpatialLines"))
spl <- list("sp.lines", slbb, lwd=0.7, col="khaki4")


###################################################
### code chunk number 57: die.Rnw:1013-1018
###################################################
Fires$dt <- as.Date(as.character(Fires$FireDate), format="%d-%m-%Y")
Fires0 <- Fires[-which(coordinates(Fires)[,2] < 0),]
Fires1 <- Fires0[order(Fires0$dt),]
library(spacetime)
Fires2 <- STIDF(as(Fires1, "SpatialPoints"), Fires1$dt, as(Fires1, "data.frame"))


###################################################
### code chunk number 58: die.Rnw:1020-1021 
###################################################
stplot(as(Fires2, "STI"), number=3, sp.layout=spl, cex=0.5)


###################################################
### code chunk number 60: die.Rnw:1057-1060
###################################################
names(Fires1)[1] <- "name"
GR_Fires <- Fires1[Fires1$Country == "GR",]
#writeOGR(GR_Fires, "EFFIS.gpx", "waypoints", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=YES", overwrite_layer=TRUE, delete_dsn=TRUE)
st_write(st_as_sf(GR_Fires), dsn="EFFIS.gpx", layer="waypoints", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=YES", delete_layer=TRUE, delete_dsn=TRUE)

###################################################
### code chunk number 61: die.Rnw:1071-1073
###################################################
#GR <- readOGR("EFFIS.gpx", "waypoints")
GR <- as(st_read("EFFIS.gpx", "waypoints"), "Spatial")
GR[1,c(5,24:28)]
base::print(GR[1,c(5,24:28)])

###################################################
### code chunk number 63: die.Rnw:1107-1108
###################################################
#getinfo.shape("scot_BNG.shp")


###################################################
### code chunk number 66: die.Rnw:1178-1181
###################################################
#auck_el1 <- readGDAL("70042108.tif")
library(stars)
auck_el1 <- as(read_stars("70042108.tif"), "Spatial")
names(auck_el1) <- "band1"
summary(auck_el1)
is.na(auck_el1$band1) <- auck_el1$band1 <= 0 | auck_el1$band1 > 1e+4


###################################################
### code chunk number 69: die.Rnw:1204-1211
###################################################
#x <- GDAL.open("70042108.tif")
#xx <- getDriver(x)
#xx #do not show pointer
#getDriverLongName(xx)
#x #do not show pointer
#dim(x)
#GDAL.close(x)


###################################################
### code chunk number 74: die.Rnw:1263-1272
###################################################
#brks <- c(0,10,20,50,100,150,200,300,400,500,600,700)
#pal <- terrain.colors(11)
#pal
#length(pal) == length(brks)-1
#auck_el1$band1 <- findInterval(auck_el1$band1, vec=brks, all.inside=TRUE)-1
#writeGDAL(auck_el1, "demIndex.tif", drivername="GTiff", type="Byte", colorTable=list(pal), mvFlag=length(brks)-1)
#Gi <- GDALinfo("demIndex.tif", returnColorTable=TRUE)
#CT <- attr(Gi, "ColorTable")[[1]]
#CT[CT > "#000000"]


###################################################
### code chunk number 76: die.Rnw:1296-1303
###################################################
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
gridded(meuse.grid) <- TRUE
slot(meuse.grid, "proj4string") <- CRS("EPSG:28992")
data(meuse)
coordinates(meuse) <- c("x", "y")
slot(meuse, "proj4string") <- slot(meuse.grid, "proj4string")


###################################################
### code chunk number 77: die.Rnw:1308-1310
###################################################
library(gstat)
log_zinc <- gstat::idw(log(zinc)~1, meuse, meuse.grid)["var1.pred"]


###################################################
### code chunk number 78: die.Rnw:1312-1314
###################################################
summary(log_zinc)
#writeGDAL(log_zinc, fname="log_zinc.tif", drivername="GTiff", type="Float32", options="INTERLEAVE=PIXEL")
write_stars(st_as_stars(log_zinc), dsn="log_zinc.tif", type="Float32", options="INTERLEAVE=PIXEL")

###################################################
### code chunk number 79: die.Rnw:1316-1317 
###################################################
#GDALinfo("log_zinc.tif")


###################################################
### code chunk number 82: die.Rnw:1346-1354
###################################################
Soil <- meuse.grid["soil"]
table(Soil$soil)
#Soil$soil <- as.integer(Soil$soil)-1
Cn <- c("Rd10A", "Rd90C/VII", "Bkd26/VII")
Soil$soil <- factor(Soil$soil, levels=1:3, labels=Cn)
#writeGDAL(Soil, "Soil.tif", drivername="GTiff", type="Byte", catNames=list(Cn), mvFlag=length(Cn))
write_stars(st_as_stars(Soil), "Soil.tif", type="Byte")
#Gi <- GDALinfo("Soil.tif", returnCategoryNames=TRUE)
#attr(Gi, "CATlist")[[1]]
#summary(readGDAL("Soil.tif"))


###################################################
### code chunk number 84: die.Rnw:1368-1369
###################################################
head(st_drivers("raster"), n=10)


###################################################
### code chunk number 90: die.Rnw:1410-1413 (eval = FALSE)
###################################################
## service_xml <- "frmt_wms_openstreetmap_tms.xml"
## offset <- c(19339000, 34546000)
## osm <- readGDAL(service_xml, offset=offset, region.dim=c(2000, 2000), output.dim=c(1000, 1000))


###################################################
### code chunk number 92: die.Rnw:1418-1420
###################################################
load("getosm.RData")
cat(paste(strwrap(getosm, exdent=5), collapse="\n"), "\n")


###################################################
### code chunk number 94: die.Rnw:1425-1426
###################################################
#osm <- readGDAL("osm_bergen_120105.tif")
osm123 <- read_stars("osm_bergen_120105.tif")
osm <- cbind(as(osm123[,,,1, drop=TRUE], "Spatial"), as(osm123[,,,2, drop=TRUE], "Spatial"), as(osm123[,,,3, drop=TRUE], "Spatial"))

###################################################
### code chunk number 96: die.Rnw:1431-1432
###################################################
summary(osm)


###################################################
### code chunk number 97: die.Rnw:1441-1450
###################################################
image(osm, red=1, green=2, blue=2)


###################################################
### code chunk number 98: die.Rnw:1497-1498
###################################################
load("RgoogleMaps.RData")


###################################################
### code chunk number 99: die.Rnw:1501-1503 (eval = FALSE)
###################################################
## library(RgoogleMaps)
## myMap <- GetMap(center=c(60.395, 5.322), zoom =16, destfile = "MyTile2.png", maptype = "mobile")


###################################################
### code chunk number 100: die.Rnw:1505-1513
###################################################
BB <- do.call("rbind", myMap$BBOX)
dBB <- rev(diff(BB))
DIM12 <- dim(myMap$myTile)[1:2]
cs <- dBB/DIM12
cc <- c(BB[1,2] + cs[1]/2, BB[1,1] + cs[2]/2)
GT <- GridTopology(cc, cs, DIM12)
p4s <- CRS("EPSG:4326")
SG_myMap <- SpatialGridDataFrame(GT, proj4string=p4s, data=data.frame(r=c(t(myMap$myTile[,,1]))*255, g=c(t(myMap$myTile[,,2]))*255, b=c(t(myMap$myTile[,,3]))*255))


###################################################
### code chunk number 101: die.Rnw:1515-1516 (eval = FALSE)
###################################################
## myMap1 <- GetMap.OSM(lonR = c(5.3190, 5.3280), latR = c(60.392, 60.398), scale=4000, destfile = "MyTile.png")


###################################################
### code chunk number 102: die.Rnw:1535-1537
###################################################
library(osmar)
load("osmar.RData")


###################################################
### code chunk number 103: die.Rnw:1540-1544 (eval = FALSE)
###################################################
## library(osmar)
## api <- osmsource_api()
## box <- corner_bbox(5.3190, 60.392, 5.3280, 60.398)
## torget <- get_osm(box, source = api)


###################################################
### code chunk number 104: die.Rnw:1546-1547
###################################################
torget1 <- as_sp(torget, "lines")


###################################################
### code chunk number 105: die.Rnw:1548-1549 
###################################################
sort(table(torget1$user), decreasing=TRUE)[1:3]


###################################################
### code chunk number 107: die.Rnw:1572-1576
###################################################
bybane <- find(torget, way(tags(k == "light_rail")))
bybane <- find_down(torget, way(bybane))
bybane <- subset(torget, ids=bybane)
bybane <- as_sp(bybane, "lines")


###################################################
### code chunk number 108: die.Rnw:1583-1596
###################################################
image(SG_myMap, red=1, green=2, blue=3)
plot(torget1, add=TRUE)
plot(bybane, add=TRUE, lwd=5, col="orange2")
plot(0:1, 0:1, type = "n", axes = FALSE, asp=1)
rasterImage(myMap1[[4]], 0, 0, 1, 1)


###################################################
### code chunk number 110: die.Rnw:1614-1615
###################################################
#writeOGR(Fires[,c("gml_id", "FireDate", "Area_HA")], dsn="fires.kml", layer="fires", driver="KML", overwrite_layer=TRUE)
st_write(st_as_sf(Fires[,c("gml_id", "FireDate", "Area_HA")]), dsn="fires.kml", layer="fires", driver="KML", delete_layer=TRUE)

###################################################
### code chunk number 111: die.Rnw:1673-1679
###################################################
#library(maptools)
grd <- as(meuse.grid, "SpatialPolygons")
proj4string(grd) <- CRS(proj4string(meuse))
#grd.union <- unionSpatialPolygons(grd, rep("x", length(slot(grd, "polygons"))))
grd.union <- st_union(st_as_sf(grd))
ll <- CRS("EPSG:4326")
grd.union.ll <- st_transform(grd.union, st_crs(ll))


###################################################
### code chunk number 112: die.Rnw:1703-1707
###################################################
#llGRD <- maptools::GE_SpatialGrid(grd.union.ll) # move from maptools to sp
library(stars)
llGRD <- st_as_stars(st_bbox(grd.union.ll), n=600*600)
#llGRD_in <- over(llGRD$SG, grd.union.ll)
#llGRD_in <- over(llGRD, grd.union.ll)
#llSGDF <- SpatialGridDataFrame(grid=slot(llGRD$SG, "grid"), proj4string=CRS(proj4string(llGRD$SG)), data=data.frame(in0=llGRD_in))
#llSGDF <- SpatialGridDataFrame(grid=slot(llGRD, "grid"), proj4string=slot(llGRD, "proj4string"), data=data.frame(in0=llGRD_in))
llSGDF <- as(st_crop(llGRD, grd.union.ll), "Spatial")
llSPix <- as(llSGDF, "SpatialPixelsDataFrame")


###################################################
### code chunk number 113: die.Rnw:1722-1724
###################################################
meuse_ll <- as(st_transform(st_as_sf(meuse), st_crs(CRS("EPSG:4326"))), "Spatial")
llSPix$pred <- gstat::idw(log(zinc)~1, meuse_ll, llSPix)$var1.pred


###################################################
### code chunk number 114: die.Rnw:1744-1749
###################################################
#png(file="zinc_IDW.png", width=llGRD$width, height=llGRD$height, bg="transparent")
#par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
image(llSPix, "pred", col=bpy.colors(20))
#dev.off()
#maptools::kmlOverlay(llGRD, "zinc_IDW.kml", "zinc_IDW.png") # move from maptools to sp

###################################################
source("die_snow.R", echo=TRUE)

(sI <- sessionInfo()) # check: no sp?

"rgdal" %in% c(names(sI$otherPkgs), names(sI$loadedOnly))
"rgeos" %in% c(names(sI$otherPkgs), names(sI$loadedOnly))
"maptools" %in% c(names(sI$otherPkgs), names(sI$loadedOnly))

