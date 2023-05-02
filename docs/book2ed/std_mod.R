### R code from vignette source 'std.Rnw'
# data: ECDovelatlon.dat ECDoveBBS1986_2003.dat 
###################################################
### code chunk number 11: std.Rnw:408-412
###################################################
ecd.ll <- as.matrix(read.table("ECDovelatlon.dat", header = FALSE))
library(sp)
ecd.ll <- SpatialPoints(ecd.ll[,c(2,1)])
proj4string(ecd.ll) <- CRS("+proj=longlat +datum=WGS84")


###################################################
### code chunk number 12: std.Rnw:421-425
###################################################
library(xts)
library(spacetime)
ecd.years <- 1986:2003
ecd.y <- as.Date(paste(ecd.years, "-01-01", sep=""), "%Y-%m-%d")


###################################################
### code chunk number 13: std.Rnw:434-438
###################################################
ecd <- read.table("ECDoveBBS1986_2003.dat", header=FALSE)
ecd[ecd == -1] <- NA
ecd.st <- STFDF(ecd.ll, ecd.y, data.frame(counts = as.vector(as.matrix(ecd))))
dim(ecd.st)


###################################################
### code chunk number 14: std.Rnw:453-456
###################################################
ecd.st2 <- stConstruct(ecd, ecd.ll, list(counts = names(ecd)),
    TimeObj = ecd.y, interval = TRUE)
all.equal(ecd.st2, ecd.st)


###################################################
### code chunk number 15: std.Rnw:503-508
###################################################
library(maps)
m <- map("state", "florida", fill = TRUE, plot = FALSE)
library(maptools)
FL <- map2SpatialPolygons(m, "FL")
proj4string(FL) <- proj4string(ecd.st)


###################################################
### code chunk number 16: std.Rnw:518-522
###################################################
dim(ecd.st[FL,])
dim(ecd.st[, "1998::2003"])
dim(ecd.st[,,"counts"])
dim(ecd.st[FL, "1998::2003", "counts"])


###################################################
### code chunk number 17: std.Rnw:531-535
###################################################
mode(ecd.st[[1]])
length(ecd.st[[1]])
length(ecd.st[["counts"]])
length(ecd.st$counts)


###################################################
### code chunk number 18: std.Rnw:543-544
###################################################
ecd.st$sqrtcounts <- sqrt(ecd.st$counts)


###################################################
### code chunk number 19: std.Rnw:562-563 (eval = FALSE)
###################################################
## over(x, y)


###################################################
### code chunk number 20: std.Rnw:578-579
###################################################
bb <- STF(FL, ecd.y[c(4,6,8,10,12)])


###################################################
### code chunk number 22: std.Rnw:600-601
###################################################
over(bb, ecd.st, fn=sum, na.rm=TRUE)


###################################################
### code chunk number 23: std.Rnw:609-610
###################################################
bb.counts <- new("STFDF", bb, data = over(bb, ecd.st, fn=sum, na.rm=TRUE))


###################################################
### code chunk number 24: std.Rnw:619-620
###################################################
aggregate(ecd.st, bb, sum, na.rm = TRUE)


###################################################
### code chunk number 25: std.Rnw:655-656
###################################################
ecd.5y <- aggregate(ecd.st, "5 years", mean, na.rm = TRUE)


###################################################
### code chunk number 27: std.Rnw:713-725
###################################################
library(RColorBrewer)
print(stplot(ecd.5y[FL,], c("1986-1990", "1991-1995", "1996-2000", "2001-2003"),
	col.regions = brewer.pal(6, "Reds"),
	cex=1, 
	cuts=c(0,5,10,20,40,80,131), sp.layout = list("sp.polygons", FL, col = "grey"),
	ylim = bbox(FL)[2,], scales = list(draw=FALSE),colorkey=TRUE))


###################################################
### code chunk number 28: std.Rnw:775-779
###################################################
ecd.FL <- ecd.st[FL, , "sqrtcounts"]
x <- as(ecd.FL, "xts")
x[is.na(x)] <- 0
o <- order(as.vector(1:18 %*% x) / apply(x,2,sum))


###################################################
### code chunk number 30: std.Rnw:802-819
###################################################
ecd.FL <- ecd.st[FL,,"sqrtcounts"]
x <- as(ecd.FL, "xts")
x[is.na(x)] <- 0
o <- order(as.vector(1:18 %*% x) / apply(x,2,sum))
library(RColorBrewer)
pal <- brewer.pal(6, "Reds")
cuts <- c(0,2,4,6,8,10,12)
ck <- list(at = cuts, labels = as.character(cuts^2))
stplot(ecd.FL[o,], mode = "xt", col.regions = pal, cuts = 6, asp = .5, 
	xlab = "Sites, ordered by time", 
	scales=list(x=list(rot = 90, cex=.5)),
	colorkey = ck)


###################################################
### code chunk number 31: std.Rnw:829-848
###################################################
library(maps)
states.m = map("state", plot=FALSE, fill=TRUE)
IDs <- sapply(strsplit(states.m$names, ":"), function(x) x[1])
library(maptools)
states = map2SpatialPolygons(states.m, IDs=IDs)
yrs = 1970:1986
time = as.POSIXct(paste(yrs, "-01-01", sep=""), tz = "GMT")
library(plm)
data("Produc")
Produc.st = STFDF(states[-8], time, Produc[order(Produc[,2], Produc[,1]),])
print(stplot(Produc.st[1:2,,5:8], mode = "tp", key.space = "bottom"),
	more = TRUE, split = c(1,1,2,1))
print(stplot(Produc.st[c(1:3,5),,5:6], mode = "ts", key.space = "bottom"),
	more = FALSE, split = c(2,1,2,1))


