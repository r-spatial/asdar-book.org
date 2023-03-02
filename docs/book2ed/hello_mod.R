###################################################
# hello_mod.R
# packages: sp, maps, maptools, rgdal
# datasets: data1964al.xy


###################################################
### chunk number 1: 
###################################################
rm(list=ls())
if ((site <- Sys.getenv("ASDAR_DOWNLOAD")) != "")
  download.file(paste(site, "data1964al.xy", sep="/"), "data1964al.xy")


###################################################
### chunk number 11: 
###################################################
library(sp)
library(maps)
library(maptools)
library(rgdal)
sessionInfo()


###################################################
### chunk number 12: 
###################################################
volc.tab = read.table("data1964al.xy")
volc = SpatialPoints(volc.tab[c(2,1)])
llCRS <- CRS("+proj=longlat +ellps=WGS84")
proj4string(volc) <- llCRS
prj_new = CRS("+proj=moll +ellps=WGS84")
volc_proj = spTransform(volc, prj_new)
wrld <- map("world", interior=FALSE, xlim=c(-179,179), ylim=c(-89,89),
 plot=FALSE)
wrld_p <- pruneMap(wrld, xlim=c(-179,179))
wrld_sp <- map2SpatialLines(wrld_p, proj4string=llCRS)
wrld_proj <- spTransform(wrld_sp, prj_new)
wrld_grd <- gridlines(wrld_sp, easts=c(-179,seq(-150,150,50),179.5),
 norths=seq(-75,75,15), ndiscr=100)
wrld_grd_proj <- spTransform(wrld_grd, prj_new)
at_sp <- gridat(wrld_sp, easts=0, norths=seq(-75,75,15), offset=0.3)
at_proj <- spTransform(at_sp, prj_new)
opar = par(no.readonly = TRUE)
par(mar=c(1,1,1,1)+0.1, xpd=NA)
plot(wrld_proj, col="grey50")
plot(wrld_grd_proj, add=TRUE, lty=3, col="grey50")
points(volc_proj, cex = .8, pch = 3)
text(coordinates(at_proj), pos=at_proj$pos, offset=at_proj$offset,
 labels=parse(text=as.character(at_proj$labels)), cex=0.6)


###################################################
### chunk number 13: 
###################################################
data(volcano)
opar = par(no.readonly = TRUE)
par(mar = rep(1,4))
grys <- grey.colors(8, 0.55, 0.95)

layout(matrix(c(1,2,1,3,1,4),3,2,byrow=TRUE), c(3,1))

image(volcano, axes=FALSE, col=grys, asp=1, main="a")
contour(volcano, add=TRUE)
box()

image(volcano, axes=FALSE, col='white', asp=1, main="b")
x2 = ContourLines2SLDF(contourLines(volcano))
plot(x2, add=TRUE)
box()

image(volcano, axes=FALSE, col='white', asp=1, main="c")
pl <- list(Polygon(coordinates(x2[x2$level == 140,])))
plot(SpatialPolygons(list(Polygons(pl, ID="x"))), add = TRUE)
box()

image(volcano, axes=FALSE, col=grys, asp=1, main="d")
x3l1 = coordinates(x2[x2$level == 160,])[[1]][[1]]
x3l2 = coordinates(x2[x2$level == 160,])[[1]][[2]]
pl <- list(Polygon(x3l1,hole=F), Polygon(x3l2,hole=TRUE))
x3 = SpatialPolygons(list(Polygons(pl, ID=c("x"))))

SP2TRI = function(x, debug = TRUE){
    p = x@polygons[[1]] # object of class Polygons
    p1 = p@Polygons[[1]] # outer Polygon
    p2 = p@Polygons[[2]] # inner Polygon
    stopifnot(!p1@hole)
    stopifnot(p2@hole)
    # find nearest point
    allcoords = rbind(p1@coords, p2@coords)
    n1 = nrow(p1@coords)
    n2 = nrow(p2@coords)
    dists = as.matrix(dist(allcoords))[((n1+1):(n1+n2)),1:n1]
    wm = which.min(dists)[1]
    ind1 = (wm %/% n2) + 1
    ind2 = wm %% n2
    if (debug)
        print(c(ind1,ind2))
    #plot polygon points:
    p1c = p1@coords
    p2c = p2@coords
    #plot shortest distance:
    if (debug)
        lines(rbind(p1c[ind1,], p2c[ind2,]))
    if (debug)
        points(rbind(p1c, p2c))
    p = rbind(p1c[c(ind1:n1,1:ind1),], p2c[c(ind2:n2,1:ind2),], p1c[ind1,])
    #polygon(p, col = 'red', border = NULL)
    polygon(p, angle=45, border = NA, density = 12)
}
plot(x3, col = 'transparent', add = TRUE)
SP2TRI(x3, FALSE)
box()

par(opar)


