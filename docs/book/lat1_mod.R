###################################################
# lat1_mod.R
# packages: sp, rgdal, spdep, pgirmess
# datasets: NY_data.zip
# provided: Sy_GeoDa1.GAL, Sy_GeoDa2.GAL, Sy_GeoDa4.GWT


###################################################
### chunk number 1: 
###################################################
rm(list=ls())
if ((site <- Sys.getenv("ASDAR_DOWNLOAD")) != "") {
  download.file(paste(site, "NY_data.zip", sep="/"), "NY_data.zip")
}

###################################################
### chunk number 8: 
###################################################
#fname <- zip.file.extract(file="NY8_utm18.dbf", zipname = "NY_data.zip")
#file.copy(fname, "./NY8_utm18.dbf", overwrite=TRUE)
#fname <- zip.file.extract(file="NY8_utm18.prj", zipname = "NY_data.zip")
#file.copy(fname, "./NY8_utm18.prj", overwrite=TRUE)
#fname <- zip.file.extract(file="NY8_utm18.shp", zipname = "NY_data.zip")
#file.copy(fname, "./NY8_utm18.shp", overwrite=TRUE)
#fname <- zip.file.extract(file="NY8_utm18.shx", zipname = "NY_data.zip")
#file.copy(fname, "./NY8_utm18.shx", overwrite=TRUE)

#fname <- zip.file.extract(file="TCE.dbf", zipname = "NY_data.zip")
#file.copy(fname, "./TCE.dbf", overwrite=TRUE)
#fname <- zip.file.extract(file="TCE.prj", zipname = "NY_data.zip")
#file.copy(fname, "./TCE.prj", overwrite=TRUE)
#fname <- zip.file.extract(file="TCE.shp", zipname = "NY_data.zip")
#file.copy(fname, "./TCE.shp", overwrite=TRUE)
#fname <- zip.file.extract(file="TCE.shx", zipname = "NY_data.zip")
#file.copy(fname, "./TCE.shx", overwrite=TRUE)

#fname <- zip.file.extract(file="NY8cities.dbf", zipname = "NY_data.zip")
#file.copy(fname, "./NY8cities.dbf", overwrite=TRUE)
#fname <- zip.file.extract(file="NY8cities.prj", zipname = "NY_data.zip")
#file.copy(fname, "./NY8cities.prj", overwrite=TRUE)
#fname <- zip.file.extract(file="NY8cities.shp", zipname = "NY_data.zip")
#file.copy(fname, "./NY8cities.shp", overwrite=TRUE)
#fname <- zip.file.extract(file="NY8cities.shx", zipname = "NY_data.zip")
#file.copy(fname, "./NY8cities.shx", overwrite=TRUE)

#fname <- zip.file.extract(file="NY_nb.gal", zipname = "NY_data.zip")
#file.copy(fname, "./NY_nb.gal", overwrite=TRUE)

unzip(zipfile="NY_data.zip")

library(sp)
library(rgdal)
NY8 <- readOGR(".", "NY8_utm18")
TCE <- readOGR(".", "TCE")
cities <- readOGR(".", "NY8cities")
library(spdep)
NY_nb <- read.gal("NY_nb.gal", region.id=row.names(as(NY8, "data.frame")))


###################################################
### chunk number 9: 
###################################################
oopar <- par(mfrow=c(1,2), mar=c(3,3,1,1)+0.1)
plot(NY8, border="grey60", axes=TRUE)
text(coordinates(cities), labels=as.character(cities$names), font=2, cex=0.9)
text(bbox(NY8)[1,1], bbox(NY8)[2,2], labels="a)", cex=0.8)
plot(NY8, border="grey60", axes=TRUE)
points(TCE, pch=1, cex=0.7)
points(TCE, pch=3, cex=0.7)
text(coordinates(TCE), labels=as.character(TCE$name), cex=0.7,
 font=1, pos=c(4,1,4,1,4,4,4,2,3,4,2), offset=0.3)
text(bbox(NY8)[1,1], bbox(NY8)[2,2], labels="b)", cex=0.8)
par(oopar)


###################################################
### chunk number 12: 
###################################################
summary(NY_nb)
isTRUE(all.equal(attr(NY_nb, "region.id"), row.names(as(NY8, "data.frame"))))


###################################################
### chunk number 14: 
###################################################
oopar <- par(mar=c(3,3,1,1)+0.1)
plot(NY8, border="grey60", axes=TRUE)
plot(NY_nb, coordinates(NY8), pch=19, cex=0.6, add=TRUE)
par(oopar)


###################################################
### chunk number 15: 
###################################################
Syracuse <- NY8[NY8$AREANAME == "Syracuse city",]
Sy0_nb <- subset(NY_nb, NY8$AREANAME == "Syracuse city")
isTRUE(all.equal(attr(Sy0_nb, "region.id"),
  row.names(as(Syracuse, "data.frame"))))
summary(Sy0_nb)


###################################################
### chunk number 16: 
###################################################
class(Syracuse)
Sy1_nb <- poly2nb(Syracuse)
isTRUE(all.equal(Sy0_nb, Sy1_nb, check.attributes=FALSE))


###################################################
### chunk number 17: 
###################################################
Sy2_nb <- poly2nb(Syracuse, queen=FALSE)
isTRUE(all.equal(Sy0_nb, Sy2_nb, check.attributes=FALSE))


###################################################
### chunk number 18: 
###################################################
oopar <- par(mfrow=c(1,2), mar=c(3,3,1,1)+0.1)
plot(Syracuse, border="grey60")
plot(Sy0_nb, coordinates(Syracuse), add=TRUE, pch=19, cex=0.6)
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="a)", cex=0.8)
plot(Syracuse, border="grey60")
plot(Sy0_nb, coordinates(Syracuse), add=TRUE, pch=19, cex=0.6)
plot(diffnb(Sy0_nb, Sy2_nb, verbose=FALSE), coordinates(Syracuse),
  add=TRUE, pch=".", cex=0.6, lwd=2)
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="b)", cex=0.8)
par(oopar)


###################################################
### chunk number 22: 
###################################################
coords <- coordinates(Syracuse)
IDs <- row.names(as(Syracuse, "data.frame"))
library(deldir)
Sy4_nb <- tri2nb(coords, row.names=IDs)
Sy5_nb <- graph2nb(soi.graph(Sy4_nb, coords), row.names=IDs)
Sy6_nb <- graph2nb(gabrielneigh(coords), row.names=IDs)
Sy7_nb <- graph2nb(relativeneigh(coords), row.names=IDs)


###################################################
### chunk number 23: 
###################################################
oopar <- par(mfrow=c(2,2), mar=c(1,1,1,1)+0.1)
plot(Syracuse, border="grey60")
plot(Sy4_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="a)", cex=0.8)
plot(Syracuse, border="grey60")
plot(Sy5_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="b)", cex=0.8)
plot(Syracuse, border="grey60")
plot(Sy6_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="c)", cex=0.8)
plot(Syracuse, border="grey60")
plot(Sy7_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="d)", cex=0.8)
par(oopar)


###################################################
### chunk number 24: 
###################################################
nb_l <- list(Triangulation=Sy4_nb, SOI=Sy5_nb, Gabriel=Sy6_nb,
  Relative=Sy7_nb)
sapply(nb_l, function(x) is.symmetric.nb(x, verbose=FALSE, force=TRUE))
sapply(nb_l, function(x) n.comp.nb(x)$nc)


###################################################
### chunk number 25: 
###################################################
Sy8_nb <- knn2nb(knearneigh(coords, k=1), row.names=IDs)
Sy9_nb <- knn2nb(knearneigh(coords, k=2), row.names=IDs)
Sy10_nb <- knn2nb(knearneigh(coords, k=4), row.names=IDs)
nb_l <- list(k1=Sy8_nb, k2=Sy9_nb, k4=Sy10_nb)
sapply(nb_l, function(x) is.symmetric.nb(x, verbose=FALSE, force=TRUE))
sapply(nb_l, function(x) n.comp.nb(x)$nc)


###################################################
### chunk number 26: 
###################################################
oopar <- par(mfrow=c(1,3), mar=c(1,1,1,1)+0.1)
plot(Syracuse, border="grey60")
plot(Sy8_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="a)", cex=0.8)
plot(Syracuse, border="grey60")
plot(Sy9_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="b)", cex=0.8)
plot(Syracuse, border="grey60")
plot(Sy10_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="c)", cex=0.8)
par(oopar)


###################################################
### chunk number 27: 
###################################################
dsts <- unlist(nbdists(Sy8_nb, coords))
summary(dsts)
max_1nn <- max(dsts)
max_1nn
Sy11_nb <- dnearneigh(coords, d1=0, d2=0.75*max_1nn, row.names=IDs)
Sy12_nb <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=IDs)
Sy13_nb <- dnearneigh(coords, d1=0, d2=1.5*max_1nn, row.names=IDs)
nb_l <- list(d1=Sy11_nb, d2=Sy12_nb, d3=Sy13_nb)
sapply(nb_l, function(x) is.symmetric.nb(x, verbose=FALSE, force=TRUE))
sapply(nb_l, function(x) n.comp.nb(x)$nc)


###################################################
### chunk number 28: 
###################################################
oopar <- par(mfrow=c(1,3), mar=c(1,1,1,1)+0.1)
plot(Syracuse, border="grey60")
plot(Sy11_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="a)", cex=0.8)
plot(Syracuse, border="grey60")
plot(Sy12_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="b)", cex=0.8)
plot(Syracuse, border="grey60")
plot(Sy13_nb, coords, add=TRUE, pch=".")
text(bbox(Syracuse)[1,1], bbox(Syracuse)[2,2], labels="c)", cex=0.8)
par(oopar)


###################################################
### chunk number 29: 
###################################################
dS <- c(0.75, 1, 1.5)*max_1nn
res <- sapply(nb_l, function(x) table(card(x)))
mx <- max(card(Sy13_nb))
res1 <- matrix(0, ncol=(mx+1), nrow=3)
rownames(res1) <- names(res)
colnames(res1) <- as.character(0:mx)
res1[1, names(res$d1)] <- res$d1
res1[2, names(res$d2)] <- res$d2
res1[3, names(res$d3)] <- res$d3
library(RColorBrewer)
pal <- grey.colors(3, 0.95, 0.55, 2.2)
barplot(res1, col=pal, beside=TRUE, legend.text=FALSE, xlab="numbers of neighbours", ylab="tracts")
legend("topright", legend=format(dS, digits=1), fill=pal, bty="n", cex=0.8, title="max. distance")


###################################################
### chunk number 30: 
###################################################
dsts0 <- unlist(nbdists(NY_nb, coordinates(NY8)))
summary(dsts0)


###################################################
### chunk number 31: 
###################################################
Sy0_nb_lags <- nblag(Sy0_nb, maxlag=9)


###################################################
### chunk number 33: 
###################################################
cell2nb(7, 7, type="rook", torus=TRUE)
cell2nb(7, 7, type="rook", torus=FALSE)


###################################################
### chunk number 34: 
###################################################
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
gridded(meuse.grid) <- TRUE
dst <- max(slot(slot(meuse.grid, "grid"), "cellsize"))
mg_nb <- dnearneigh(coordinates(meuse.grid), 0, dst)
mg_nb
table(card(mg_nb))


###################################################
### chunk number 35: 
###################################################
Sy0_lw_W <- nb2listw(Sy0_nb)
Sy0_lw_W
names(Sy0_lw_W)
names(attributes(Sy0_lw_W))


###################################################
### chunk number 36: 
###################################################
1/rev(range(card(Sy0_lw_W$neighbours)))
summary(unlist(Sy0_lw_W$weights))
summary(sapply(Sy0_lw_W$weights, sum))


###################################################
### chunk number 37: 
###################################################
Sy0_lw_B <- nb2listw(Sy0_nb, style="B")
summary(unlist(Sy0_lw_B$weights))
summary(sapply(Sy0_lw_B$weights, sum))


###################################################
### chunk number 38: 
###################################################
Sy0_lw_C <- nb2listw(Sy0_nb, style="C")
length(Sy0_lw_C$neighbours)/length(unlist(Sy0_lw_C$neighbours))
summary(unlist(Sy0_lw_C$weights))
summary(sapply(Sy0_lw_C$weights, sum))


###################################################
### chunk number 39: 
###################################################
Sy0_lw_S <- nb2listw(Sy0_nb, style="S")
summary(unlist(Sy0_lw_S$weights))
summary(sapply(Sy0_lw_S$weights, sum))


###################################################
### chunk number 40: 
###################################################
dsts <- nbdists(Sy0_nb, coordinates(Syracuse))
idw <- lapply(dsts, function(x) 1/(x/1000))
Sy0_lw_idwB <- nb2listw(Sy0_nb, glist=idw, style="B")
summary(unlist(Sy0_lw_idwB$weights))
summary(sapply(Sy0_lw_idwB$weights, sum))


###################################################
### chunk number 41: 
###################################################
pal <- grey.colors(9, 1, 0.5, 2.2)
oopar <- par(mfrow=c(1,3), mar=c(1,1,3,1)+0.1)
z <- t(listw2mat(Sy0_lw_W))
brks <- c(0,0.1,0.143,0.167,0.2,0.5,1)
nbr3 <- length(brks)-3
image(1:63, 1:63, z[,ncol(z):1], breaks=brks, col=pal[c(1,(9-nbr3):9)],
 main="W style", axes=FALSE)
box()
z <- t(listw2mat(Sy0_lw_B))
image(1:63, 1:63, z[,ncol(z):1], col=pal[c(1,9)], main="B style", axes=FALSE)
box()
z <- t(listw2mat(Sy0_lw_idwB))
brks <- c(0,0.35,0.73,0.93,1.2,2.6)
nbr3 <- length(brks)-3
image(1:63, 1:63, z[,ncol(z):1], breaks=brks, col=pal[c(1,(9-nbr3):9)],
 main="IDW B style", axes=FALSE)
box()
par(oopar)


###################################################
### chunk number 43: 
###################################################
try(Sy0_lw_D1 <- nb2listw(Sy11_nb, style="B"))


###################################################
### chunk number 44: 
###################################################
Sy0_lw_D1 <- nb2listw(Sy11_nb, style="B", zero.policy=TRUE)
print(Sy0_lw_D1, zero.policy=TRUE)


###################################################
### chunk number 45: 
###################################################
Sy14_nb <- read.gal("Sy_GeoDa1.GAL")
isTRUE(all.equal(Sy0_nb, Sy14_nb, check.attributes=FALSE))
Sy15_nb <- read.gal("Sy_GeoDa2.GAL")
isTRUE(all.equal(Sy2_nb, Sy15_nb, check.attributes=FALSE))


###################################################
### chunk number 46: 
###################################################
Sy16_nb <- read.gwt2nb("Sy_GeoDa4.GWT")
isTRUE(all.equal(Sy10_nb, Sy16_nb, check.attributes=FALSE))


###################################################
### chunk number 48: 
###################################################
set.seed(987654)
n <- length(Sy0_nb)
uncorr_x <- rnorm(n)
rho <- 0.5
autocorr_x <- spatialreg::invIrW(Sy0_lw_W, rho) %*% uncorr_x


###################################################
### chunk number 49: 
###################################################
oopar <- par(mfrow=c(1,2), mar=c(4,4,3,2)+0.1)
plot(uncorr_x, lag(Sy0_lw_W, uncorr_x), xlab="random variable", cex.lab=0.8,
 ylab="spatial lag", main="Uncorrelated random variable", cex.main=0.8)
lines(lowess(uncorr_x, lag(Sy0_lw_W, uncorr_x)), lty=2, lwd=2)
plot(autocorr_x, lag(Sy0_lw_W, autocorr_x),
 xlab="autocorrelated random variable", ylab="spatial lag",
 main="Autocorrelated random variable", cex.main=0.8, cex.lab=0.8)
lines(lowess(autocorr_x, lag(Sy0_lw_W, autocorr_x)), lty=2, lwd=2)
par(oopar)


###################################################
### chunk number 50: 
###################################################
moran.test(uncorr_x, listw=Sy0_lw_W)
moran.test(autocorr_x, listw=Sy0_lw_W)
moran.test(autocorr_x, listw=nb2listw(Sy9_nb, style="W"))


###################################################
### chunk number 51: 
###################################################
et <- coords[,1] - min(coords[,1])
trend_x <- uncorr_x + 0.00025 * et
moran.test(trend_x, listw=Sy0_lw_W)
lm.morantest(lm(trend_x ~ et), listw=Sy0_lw_W)


###################################################
### chunk number 53: 
###################################################
moran(NY8$Cases, listw=nb2listw(NY_nb, style="B"), n=length(NY8$Cases), S0=Szero(nb2listw(NY_nb, style="B")))


###################################################
### chunk number 54: 
###################################################
moran.test(NY8$Cases, listw=nb2listw(NY_nb))


###################################################
### chunk number 55: 
###################################################
lw_B <- nb2listw(NY_nb, style="B")
moran.test(NY8$Cases, listw=lw_B)


###################################################
### chunk number 56: 
###################################################
moran.test(NY8$Cases, listw=lw_B, randomisation=FALSE)


###################################################
### chunk number 57: 
###################################################
lm.morantest(lm(Cases ~ 1, NY8), listw=lw_B)


###################################################
### chunk number 59: 
###################################################
lm.morantest.sad(lm(Cases ~ 1, NY8), listw=lw_B)


###################################################
### chunk number 61: 
###################################################
lm.morantest.exact(lm(Cases ~ 1, NY8), listw=lw_B)


###################################################
### chunk number 62: 
###################################################
set.seed(1234)
bperm <- moran.mc(NY8$Cases, listw=lw_B, nsim=999)
bperm


###################################################
### chunk number 63: 
###################################################
r <- sum(NY8$Cases)/sum(NY8$POP8)
rni <- r*NY8$POP8
CR <- function(var, mle) rpois(length(var), lambda=mle)
MoranI.pboot <- function(var, i, listw, n, S0, ...) {
  return(moran(x=var, listw=listw, n=n, S0=S0)$I)
}
set.seed(1234)


###################################################
### chunk number 65: 
###################################################
library(boot)
boot2 <- boot(NY8$Cases, statistic=MoranI.pboot, R=999, sim="parametric",
  ran.gen=CR, listw=lw_B, n=length(NY8$Cases), S0=Szero(lw_B), mle=rni)


###################################################
### chunk number 67: 
###################################################
pnorm((boot2$t0 - mean(boot2$t[,1]))/sd(boot2$t[,1]), lower.tail=FALSE)


###################################################
### chunk number 68: 
###################################################
oopar <- par(mfrow=c(1,2))
xlim <- range(c(bperm$res, boot2$t[,1]))
hist(bperm$res[-length(bperm$res)], main="Permutation bootstrap", xlab=expression(I[std]), xlim=xlim, density=15, angle=45, ylim=c(0,260))
abline(v=bperm$statistic, lty=2)
hist(boot2$t, col=rgb(0.4,0.4,0.4), main="Parametric bootstrap", xlab=expression(I[CR]), xlim=xlim, ylim=c(0,260))
hist(bperm$res[-length(bperm$res)], density=15, angle=45, add=TRUE)
abline(v=boot2$t0, lty=2)
par(oopar)


###################################################
### chunk number 70: 
###################################################
set.seed(1234)
EBImoran.mc(n=NY8$Cases, x=NY8$POP8, listw=nb2listw(NY_nb, style="B"), nsim=999)


###################################################
### chunk number 71: 
###################################################
cor8 <- sp.correlogram(neighbours=NY_nb, var=NY8$Cases, order=8, method="I", style="C")
library(pgirmess)
corD <- correlog(coordinates(NY8), NY8$Cases, method="Moran")


###################################################
### chunk number 72: 
###################################################
oopar <- par(mfrow=c(1,2))
plot(cor8, main="Contiguity lag orders")
plot(corD, main="Distance bands")
par(oopar)


###################################################
### chunk number 75: 
###################################################
print(cor8, p.adj.method="holm") 


###################################################
### chunk number 78: 
###################################################
corD


###################################################
### chunk number 79: 
###################################################
oopar <- par(mfrow=c(1,2))
msp <- moran.plot(NY8$Cases, listw=nb2listw(NY_nb, style="C"), quiet=TRUE)
title("Moran scatterplot")
infl <- ifelse(packageVersion("spdep") > "1.1.4", msp$is_inf, apply(msp$is.inf, 1, any))
x <- NY8$Cases
lhx <- cut(x, breaks=c(min(x), mean(x), max(x)), labels=c("L", "H"), include.lowest=TRUE)
wx <- lag(nb2listw(NY_nb, style="C"), NY8$Cases)
lhwx <- cut(wx, breaks=c(min(wx), mean(wx), max(wx)), labels=c("L", "H"), include.lowest=TRUE)
lhlh <- interaction(lhx, lhwx, infl, drop=TRUE)
cols <- rep(1, length(lhlh))
cols[lhlh == "H.L.TRUE"] <- 2
cols[lhlh == "L.H.TRUE"] <- 3
cols[lhlh == "H.H.TRUE"] <- 4
plot(NY8, col=grey.colors(4, 0.95, 0.55, 2.2)[cols])
legend("topright", legend=c("None", "HL", "LH", "HH"), fill=grey.colors(4, 0.95, 0.55, 2.2), bty="n", cex=0.8, y.intersp=0.8)
title("Tracts with influence")
par(oopar)


###################################################
### chunk number 81: 
###################################################
lm1 <- localmoran(NY8$Cases, listw=nb2listw(NY_nb, style="C"))
lm2 <- as.data.frame(localmoran.sad(lm(Cases ~ 1, NY8), nb=NY_nb, style="C"))
lm3 <- as.data.frame(localmoran.exact(lm(Cases ~ 1, NY8), nb=NY_nb, style="C"))


###################################################
### chunk number 82: 
###################################################
r <- sum(NY8$Cases)/sum(NY8$POP8)
rni <- r*NY8$POP8
lw <- nb2listw(NY_nb, style="C")
sdCR <- (NY8$Cases - rni)/sqrt(rni)
wsdCR <- lag(lw, sdCR)
I_CR <- sdCR * wsdCR


###################################################
### chunk number 83: 
###################################################
NY8$Standard <- lm1[,1]
NY8$"Constant_risk" <- I_CR
nms <- match(c("Standard", "Constant_risk"), names(NY8))
spplot(NY8, nms, at=c(-2.5,-1.4,-0.6,-0.2,0,0.2,0.6,4,7), col.regions=grey.colors(8, 0.95, 0.55, 2.2))


###################################################
### chunk number 84: 
###################################################
set.seed(1234)
nsim <- 999
N <- length(rni)
sims <- matrix(0, ncol=nsim, nrow=N)
for (i in 1:nsim) {
  y <- rpois(N, lambda=rni)
  sdCRi <- (y - rni)/sqrt(rni)
  wsdCRi <- lag(lw, sdCRi)
  sims[,i] <- sdCRi * wsdCRi 
}
xrank <- apply(cbind(I_CR, sims), 1, function(x) rank(x)[1])
diff <- nsim - xrank
diff <- ifelse(diff > 0, diff, 0)
pval <- punif((diff + 1)/(nsim + 1))


###################################################
### chunk number 85: 
###################################################
NY8$Normal <- lm2[,3]
NY8$Randomisation <- lm1[,5]
NY8$Saddlepoint <- lm2[,5]
NY8$Exact <- lm3[,5]
NY8$Constant_risk <- pval
spplot(NY8, c("Normal", "Randomisation", "Saddlepoint", "Exact", "Constant_risk"), at=c(0,0.01,0.05,0.1,0.9,0.95,0.99,1), col.regions=grey.colors(7, 0.95, 0.55, 2.2))


###################################################
### chunk number 86: 
###################################################
spplot(NY8, c("Normal", "Exact", "Constant_risk"), xlim=c(405200, 432200), ylim=c(4652700, 4672000), at=c(0,0.01,0.05,0.1,0.9,0.95,0.99,1), col.regions=grey.colors(7, 0.95, 0.55, 2.2))

