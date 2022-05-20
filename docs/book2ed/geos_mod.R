### R code from vignette source 'geos.Rnw'
### Encoding: UTF-8
# data: m1m2.R
###################################################
### code chunk number 1: geos.Rnw:8-19
###################################################
require(RColorBrewer)
require(lattice)
pal = function(n = 9) brewer.pal(n, "Reds")


###################################################
### code chunk number 11: geos.Rnw:169-175
###################################################
#trellis.par.set(canonical.theme(color = FALSE))
library(sp)
data(meuse)
coordinates(meuse) <- c("x", "y")


###################################################
### code chunk number 13: geos.Rnw:207-220
###################################################
print(xyplot(log(zinc)~sqrt(dist), as.data.frame(meuse), asp = .8), split = 
c(1, 1,2,1), more = TRUE)
zn.lm <- lm(log(zinc)~sqrt(dist), meuse)
meuse$fitted.s <- predict(zn.lm, meuse) - mean(predict(zn.lm, meuse))
meuse$residuals <- residuals(zn.lm)
print(spplot(meuse, c("fitted.s", "residuals"), col.regions = 
  pal(), cuts = 8, colorkey=TRUE), split = c(2,1,2,1))


###################################################
### code chunk number 14: geos.Rnw:250-253
###################################################
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")


###################################################
### code chunk number 15: geos.Rnw:290-293
###################################################
library(gstat)
idw.out <- gstat::idw(zinc~1, meuse, meuse.grid, idp = 2.5)
as.data.frame(idw.out)[1:5,]


###################################################
### code chunk number 16: geos.Rnw:320-323
###################################################
zn.lm <- lm(log(zinc)~sqrt(dist), meuse)
meuse.grid$pred <- predict(zn.lm, meuse.grid)
meuse.grid$se.fit <- predict(zn.lm, meuse.grid, se.fit=TRUE)$se.fit


###################################################
### code chunk number 17: geos.Rnw:337-338
###################################################
meuse.lm <- krige(log(zinc)~sqrt(dist), meuse, meuse.grid)


###################################################
### code chunk number 18: geos.Rnw:364-365
###################################################
meuse.tr2 <- krige(log(zinc)~1, meuse, meuse.grid, degree = 2)


###################################################
### code chunk number 19: geos.Rnw:384-385 
###################################################
lm(log(zinc)~I(x^2)+I(y^2)+I(x*y) + x + y, meuse)


###################################################
### code chunk number 20: geos.Rnw:393-394 
###################################################
lm(log(zinc) ~ poly(x, y, degree = 2), meuse)


###################################################
### code chunk number 22: geos.Rnw:515-521
###################################################
hscat(log(zinc)~1,meuse,(0:9)*100, pch=3, cex=.6, col = 'grey')


###################################################
### code chunk number 24: geos.Rnw:560-585
###################################################
library(gstat)
cld <- variogram(log(zinc) ~ 1, meuse, cloud = TRUE)
svgm <- variogram(log(zinc) ~ 1, meuse)
d <- data.frame(gamma = c(cld$gamma, svgm$gamma),
    dist = c(cld$dist, svgm$dist),
    id = c(rep("cloud", nrow(cld)), rep("sample variogram", nrow(svgm)))
    )
xyplot(gamma ~ dist | id, d,
    scales = list(y = list(relation = "free", 
	  #ylim = list(NULL, c(-.005,0.7)))),
	  limits = list(NULL, c(-.005,0.7)))),
    layout = c(1, 2), as.table = TRUE,
    panel = function(x,y, ...) {
        if (panel.number() == 2)
            ltext(x+10, y, svgm$np, adj = c(0,0.5)) #$
        panel.xyplot(x,y,...)
    },
    xlim = c(0, 1590),
    cex = .5, pch = 3
)


###################################################
### code chunk number 25: geos.Rnw:597-599 (eval = FALSE)
###################################################
## library(gstat)
## variogram(log(zinc) ~ 1, meuse, cloud = TRUE)


###################################################
### code chunk number 26: geos.Rnw:626-628 (eval = FALSE)
###################################################
## sel <- plot(variogram(zinc ~ 1, meuse, cloud = TRUE), digitize = TRUE)
## plot(sel, meuse)


###################################################
### code chunk number 27: geos.Rnw:664-694
###################################################
sel <-
structure(list(x = c(145.291968730077, 266.107479142605, 320.156523274526,
339.232656497557, 323.335878811698, 212.058435010685, 135.753902118561,
46.7319470777507, 78.5255024494688, 142.112613192905), y = c(574649.690841889,
581256.265954825, 627502.29174538, 822396.257577002, 1053626.38652977,
1278249.94036961, 1255126.92747433, 792666.669568789, 634108.866858316,
577952.978398357)), .Names = c("x", "y"))
v <- variogram(zinc ~ 1, meuse, cloud = TRUE)
v$gamma <- v$gamma/1e6
sel$y <- sel$y/1e6
p1 <- xyplot(gamma~dist, v,
    panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        llines(sel$x, sel$y, col = 'red')
    },
    pch=3, cex = .5, asp = 1, ylab = "gamma (x 1e6)")
x <-
structure(list(head = c(40, 40, 40, 54, 55, 54, 47, 80, 55, 55,
54, 53, 54, 55, 59, 59), tail = c(41, 42, 43, 57, 57, 58, 59,
99, 121, 122, 123, 125, 125, 125, 125, 132)), .Names = c("head",
"tail"), row.names = as.integer(c(NA, 16)), class = c("pointPairs",
"data.frame"))
p2 = plot(x, meuse, scales=list(draw=F), col.line = 'red')
print(p1, split = c(1,1,2,1), more = TRUE)
print(p2, split = c(2,1,2,1))


###################################################
### code chunk number 28: geos.Rnw:726-758
###################################################
v <- variogram(log(zinc) ~ 1, meuse)
# INTERACTIVE mode out-commented:
#plot(v, type = 'b', pch = 3)
#fn = function(n = 100) {
#        for (i in 1:n) {
#           meuse$random = sample(meuse$zinc)
#           v = variogram(log(random) ~ 1, meuse)
#           trellis.focus("panel", 1, 1, highlight = FALSE)
#           llines(v$dist, v$gamma, col = 'grey')
#           trellis.unfocus()
#        }
#}
#fn()
#trellis.focus("panel", 1, 1, highlight = FALSE)
#lpoints(v$dist, v$gamma, col = 'black', type = 'b', lwd = 2, pch=3)
#trellis.unfocus()
print(xyplot(gamma ~ dist, v, pch = 3, type = 'b', lwd = 2, col = 'darkblue',
    panel = function(x, y, ...) {
        for (i in 1:100) {
            meuse$random = sample(meuse$zinc)
            v = variogram(log(random) ~ 1, meuse)
            llines(v$dist, v$gamma, col = 'grey')
        }
        panel.xyplot(x, y, ...)
    },
    ylim = c(0, 0.75), xlab = 'distance', ylab = 'semivariance'
))


###################################################
### code chunk number 29: geos.Rnw:770-771 
###################################################
plot(variogram(log(zinc) ~ 1, meuse))


###################################################
### code chunk number 30: geos.Rnw:789-790 
###################################################
plot(variogram(log(zinc) ~ 1, meuse, alpha = c(0, 45, 90, 135)))


###################################################
### code chunk number 31: geos.Rnw:850-851 
###################################################
plot(variogram(log(zinc) ~ 1, meuse, cutoff = 1000, width = 50))


###################################################
### code chunk number 32: geos.Rnw:866-867 
###################################################
variogram(log(zinc) ~ 1, meuse, boundaries = c(0,50,100,seq(250,1500,250)))


###################################################
### code chunk number 33: geos.Rnw:903-905 
###################################################
show.vgms()
show.vgms(model = "Mat", kappa.range = c(.1, .2, .5, 1, 2, 5, 10), max = 10)


###################################################
### code chunk number 34: geos.Rnw:931-937
###################################################
vgm(1, "Sph", 300)
vgm(1, "Sph", 300, 0.5)
v1 <- vgm(1, "Sph", 300, 0.5)
v2 <- vgm(0.8, "Sph", 800, add.to = v1)
v2
vgm(0.5, "Nug", 0)


###################################################
### code chunk number 35: geos.Rnw:962-963
###################################################
vgm()


###################################################
### code chunk number 36: geos.Rnw:984-986 
###################################################
v <- variogram(log(zinc) ~ 1, meuse)
plot(v)


###################################################
### code chunk number 37: geos.Rnw:1002-1003
###################################################
fit.variogram(v, vgm(1, "Sph", 800, 1))


###################################################
### code chunk number 38: geos.Rnw:1017-1018
###################################################
fit.variogram(v, vgm(1, "Sph", 10, 1))


###################################################
### code chunk number 39: geos.Rnw:1032-1035 (eval = FALSE)
###################################################
## v.fit <- fit.variogram(v, vgm(1, "Sph", 10, 1))
## if (attr(v.fit, "singular"))
##     stop("singular fit")


###################################################
### code chunk number 40: geos.Rnw:1050-1081
###################################################
ccol = 'darkblue' #grey(.5)
v <- variogram(log(zinc) ~ 1, meuse)
v.fit <- fit.variogram(v, vgm(1, "Sph", 800, 1))
plot(v, v.fit, pch = 3, panel = function(x,y,subscripts,...) {
		larrows(0,v.fit$psill[1], v.fit$range[2], v.fit$psill[1], 
			col=ccol, ends = 'both', length=.1, angle=15)
		larrows(v.fit$range[2],0, v.fit$range[2], v.fit$psill[1], 
			col=ccol, ends = 'both', length=.1, angle=15)
		larrows(v.fit$range[2],v.fit$psill[1], v.fit$range[2], 
			sum(v.fit$psill), 
			col=ccol, ends = 'both', length=.1, angle=15)
		ltext(v.fit$rang[2]/2, 1.2*v.fit$psill[1], "range", col=ccol,
			adj = c(.5, 0), cex=.9)
		ltext(1.02 * v.fit$rang[2], 0.5 *v.fit$psill[1], "nugget", col=ccol,
			adj = c(0, 0.5), cex=.9)
		ltext(1.02 * v.fit$rang[2], v.fit$psill[1] + 0.5 * v.fit$psill[2], 
			"partial sill", col=ccol, adj = c(0, 0.5), cex=.9)
		vgm.panel.xyplot(x,y,subscripts,...)
	}
)



###################################################
### code chunk number 41: geos.Rnw:1106-1107
###################################################
attr(v.fit, "SSErr")


###################################################
### code chunk number 42: geos.Rnw:1150-1153 (eval = FALSE)
###################################################
## library(geoR)
## v.eye <- eyefit(variog(as.geodata(meuse["zinc"]),max.dist=1500))
## ve.fit <- as.vgm.variomodel(v.eye[[1]])


###################################################
### code chunk number 43: geos.Rnw:1185-1186
###################################################
fit.variogram(v, vgm(1, "Sph", 800, 0.06), fit.sills = c(FALSE, TRUE))


###################################################
### code chunk number 44: geos.Rnw:1207-1215
###################################################
v.dir <- variogram(log(zinc)~1,meuse,alpha=(0:3)*45)
v.anis <- vgm(.6, "Sph", 1600, .05, anis=c(45,.3))
print(plot(v.dir, v.anis, pch=3))


###################################################
### code chunk number 45: geos.Rnw:1224-1225
###################################################
fit.variogram.reml(log(zinc)~1, meuse, model=vgm(0.6, "Sph", 800, 0.06))


###################################################
### code chunk number 46: geos.Rnw:1262-1264
###################################################
v.dir <- variogram(log(zinc)~1,meuse,alpha=(0:3)*45)
v.anis <- vgm(.6, "Sph", 1600, .05, anis=c(45, 0.3))


###################################################
### code chunk number 47: geos.Rnw:1266-1267 
###################################################
plot(v.dir, v.anis)


###################################################
### code chunk number 48: geos.Rnw:1285-1286 
###################################################
plot(variogram(log(zinc)~1,meuse, map=TRUE, cutoff=1000, width=100))


###################################################
### code chunk number 49: geos.Rnw:1343-1350
###################################################
g <- gstat(NULL, "logCd", log(cadmium)~1, meuse)
g <- gstat(g, "logCu", log(copper)~1, meuse)
g <- gstat(g, "logPb", log(lead)~1, meuse)
g <- gstat(g, "logZn", log(zinc)~1, meuse)
g
vm <- variogram(g)
vm.fit <- fit.lmc(vm, g, vgm(1, "Sph", 800, 1))


###################################################
### code chunk number 51: geos.Rnw:1378-1384
###################################################
print(plot(vm, vm.fit))


###################################################
### code chunk number 52: geos.Rnw:1397-1398
###################################################
cor(as.data.frame(meuse)[c("cadmium", "copper", "lead", "zinc")])


###################################################
### code chunk number 54: geos.Rnw:1468-1474
###################################################
f <- log(zinc) ~ sqrt(dist)
vt <- variogram(f, meuse)
vt.fit <- fit.variogram(vt, vgm(1, "Exp", 300, 1))
vt.fit
g.wls <- gstat(NULL, "log-zinc", f, meuse, model=vt.fit, set = list(gls=1))
(variogram(g.wls)$gamma - vt$gamma)/ mean(vt$gamma)


###################################################
### code chunk number 55: geos.Rnw:1571-1574
###################################################
lz.sk <- krige(log(zinc)~1, meuse, meuse.grid, v.fit, beta = 5.9)
lz.ok <- krige(log(zinc)~1, meuse, meuse.grid, v.fit)
lz.uk <- krige(log(zinc)~sqrt(dist), meuse, meuse.grid, vt.fit)


###################################################
### code chunk number 56: geos.Rnw:1617-1619
###################################################
cok.maps <- predict(vm.fit, meuse.grid)
names(cok.maps)


###################################################
### code chunk number 58: geos.Rnw:1644-1652
###################################################
print(spplot.vcov(cok.maps, cuts=6, col.regions=pal(7)))


###################################################
### code chunk number 60: geos.Rnw:1676-1678
###################################################
vm2.fit <- vm.fit
vm2.fit$model[[3]]$range  = c(0, 900)


###################################################
### code chunk number 61: geos.Rnw:1680-1684
###################################################
vm2.fit$set <- list(nocheck=1) 
x <- predict(vm2.fit, meuse.grid)
names(as.data.frame(x))
any(as.data.frame(x)[c(2,4,6,8)] < 0)


###################################################
### code chunk number 62: geos.Rnw:1711-1724
###################################################
g.cc <- gstat(NULL, "log.zinc", log(zinc)~1, meuse, model = v.fit)
meuse.grid$distn <- meuse.grid$dist - mean(meuse.grid$dist) +
   mean(log(meuse$zinc))
vd.fit <- v.fit
vov <- var(meuse.grid$distn) / var(log(meuse$zinc))
vd.fit$psill <- v.fit$psill * vov
g.cc <- gstat(g.cc, "distn", distn ~ 1, meuse.grid, nmax = 1, model=vd.fit,
    merge = c("log.zinc","distn"))
vx.fit <- v.fit
vx.fit$psill <- sqrt(v.fit$psill * vd.fit$psill) *
   cor(meuse$dist, log(meuse$zinc)) #$
g.cc <- gstat(g.cc, c("log.zinc", "distn"), model = vx.fit)
x <- predict(g.cc, meuse.grid)


###################################################
### code chunk number 63: geos.Rnw:1740-1752
###################################################
x$lz.uk <- lz.uk$var1.pred
x$lz.ok <- lz.ok$var1.pred
print(spplot(x, c("log.zinc.pred", "lz.ok", "lz.uk"),
    names.attr = c("collocated", "ordinary", "universal"),
    cuts=7, col.regions=pal(8)
))


###################################################
### code chunk number 65: geos.Rnw:1888-1889
###################################################
lz.ok <- krige(log(zinc)~1, meuse, meuse.grid, v.fit, block = c(40, 40))


###################################################
### code chunk number 66: geos.Rnw:1905-1908
###################################################
xy <- expand.grid(x = seq(-20, 20, 4), y = seq(-20, 20, 4))
xy <- xy[(xy$x^2 + xy$y^2) <= 20^2, ]
lz.ok <- krige(log(zinc)~1, meuse, meuse.grid, v.fit, block = xy)


###################################################
### code chunk number 67: geos.Rnw:1921-1922 
###################################################
## lz.pols <- krige(log(zinc)~1, meuse, meuse.polygons, v.fit)


###################################################
### code chunk number 68: geos.Rnw:1936-1937 
###################################################
## spsample(polygon, n = 500, type = "regular", offset = c(.5, .5))


###################################################
### code chunk number 70: geos.Rnw:1988-1989
###################################################
meuse$part.a <- gstat::idw(part.a~1, meuse.grid, meuse, nmax=1)$var1.pred


###################################################
### code chunk number 72: geos.Rnw:2011-2012
###################################################
meuse$part.a <- over(meuse, meuse.grid["part.a"])[[1]]


###################################################
### code chunk number 73: geos.Rnw:2020-2021
###################################################
meuse$part.a <- meuse.grid$part.a[over(meuse, geometry(meuse.grid))]


###################################################
### code chunk number 74: geos.Rnw:2039-2048
###################################################
x1 <- krige(log(zinc)~1, meuse[meuse$part.a == 0,],
meuse.grid[meuse.grid$part.a == 0,], model = vgm(.548, "Sph", 900, .0654),
nmin = 20, nmax = 40, maxdist = 1000)
x2 <- krige(log(zinc)~1, meuse[meuse$part.a == 1,],
meuse.grid[meuse.grid$part.a == 1,], model = vgm(.716, "Sph", 900),
nmin = 20, nmax = 40, maxdist = 1000)
lz.stk <- rbind(as.data.frame(x1), as.data.frame(x2))
coordinates(lz.stk) <- c("x", "y")
lz.stk <- as(x, "SpatialPixelsDataFrame")


###################################################
### code chunk number 75: geos.Rnw:2050-2051 (eval = FALSE)
###################################################
## spplot(lz.stk["var1.pred"], main = "stratified kriging predictions")


###################################################
### code chunk number 76: geos.Rnw:2067-2070
###################################################
g.tr <- gstat(formula = log(zinc) ~ sqrt(dist), data = meuse, model = v.fit)
predict(g.tr, meuse[1,])
predict(g.tr, meuse[1,], BLUE = TRUE)


###################################################
### code chunk number 77: geos.Rnw:2090-2091 (eval = FALSE)
###################################################
## predict(g, meuse[1,], BLUE = TRUE, debug = 32)


###################################################
### code chunk number 78: geos.Rnw:2104-2112
###################################################
meuse$Int <- rep(1, 155) 
g.tr <- gstat(formula = log(zinc) ~ -1+Int+sqrt(dist), data = meuse, 
   model = v.fit)
rn <- c("Intercept", "beta1")
df <- data.frame(Int = c(1,0), dist = c(0,1), row.names=rn)
spdf <- SpatialPointsDataFrame(SpatialPoints(matrix(0, 2, 2)), df)
spdf
predict(g.tr, spdf, BLUE = TRUE)


###################################################
### code chunk number 79: geos.Rnw:2159-2161 (eval = FALSE)
###################################################
## library(MASS)
## boxcox(zinc~sqrt(dist), data=as.data.frame(meuse))


###################################################
### code chunk number 80: geos.Rnw:2179-2180
###################################################
meuse$zinc.ns <- qqnorm(meuse$zinc, plot.it = FALSE)$x


###################################################
### code chunk number 81: geos.Rnw:2207-2212
###################################################
ind.f <- I(zinc < 500) ~ 1
ind.fit <- fit.variogram(variogram(ind.f, meuse), vgm(1, "Sph", 800, 1))
ind.kr <- krige(ind.f, meuse, meuse.grid, ind.fit)
summary(ind.kr$var1.pred)



###################################################
### code chunk number 82: geos.Rnw:2257-2260 (eval = FALSE)
###################################################
## meuse.dup <- rbind(as.data.frame(meuse)[1,], as.data.frame(meuse))
## coordinates(meuse.dup)=~x+y
## krige(log(zinc)~1, meuse.dup, meuse[1,], v.fit)


###################################################
### code chunk number 83: geos.Rnw:2287-2289
###################################################
meuse.dup <- rbind(as.data.frame(meuse)[1,], as.data.frame(meuse))
coordinates(meuse.dup)=~x+y


###################################################
### code chunk number 84: geos.Rnw:2291-2295
###################################################
zd <- zerodist(meuse.dup)
zd
meuse0 <- meuse.dup[-zd[,1],]
krige(log(zinc)~1, meuse0, meuse[1,], v.fit)


###################################################
### code chunk number 85: geos.Rnw:2333-2335
###################################################
if (packageDescription("gstat")$Version < "1.1-1") {
    setL <- list(cn_max=1e10)
} else {
    setL <- list(choleski = 0)
}
krige(log(zinc)~1, meuse.dup, meuse[1,], v.fit, set = setL)


###################################################
### code chunk number 86: geos.Rnw:2372-2377
###################################################
v <- variogram(log(zinc) ~ 1, meuse)
v.fit <- fit.variogram(v, vgm(1, "Sph", 800, 1))
v.fit
log(meuse$zinc[1])
krige(log(zinc)~1, meuse, meuse[1,], v.fit)


###################################################
### code chunk number 87: geos.Rnw:2379-2381
###################################################
meuse_shift = meuse
meuse_shift@coords[1,] = meuse@coords[1,] + 1


###################################################
### code chunk number 88: geos.Rnw:2389-2390
###################################################
krige(log(zinc)~1, meuse, meuse_shift[1,], v.fit)


###################################################
### code chunk number 89: geos.Rnw:2416-2419
###################################################
err.fit <- fit.variogram(v, vgm(1, "Sph", 800, Err=1))
err.fit
krige(log(zinc)~1, meuse, meuse[1,], err.fit)


###################################################
### code chunk number 90: geos.Rnw:2435-2439
###################################################
v = fit.variogram(v, vgm(1, "Sph", 800, Err = .01, nugget = 1),
   fit.sill = c(FALSE,TRUE,TRUE))
v
krige(log(zinc)~1, meuse[1,], meuse[1,], v)


###################################################
### code chunk number 91: geos.Rnw:2490-2491
###################################################
set.seed(1357531)


###################################################
### code chunk number 92: geos.Rnw:2493-2503
###################################################
sel100 <- sample(1:155, 100)
m.model <- meuse[sel100,]
m.valid <- meuse[-sel100,]
v100.fit <- fit.variogram(variogram(log(zinc)~1, m.model), vgm(1, "Sph", 800,   1))
m.valid.pr <- krige(log(zinc)~1, m.model, m.valid, v100.fit)
resid.kr <- log(m.valid$zinc) - m.valid.pr$var1.pred
summary(resid.kr)
resid.mean <- log(m.valid$zinc) - mean(log(m.valid$zinc))
R2 <- 1 - sum(resid.kr^2)/sum(resid.mean^2)
R2


###################################################
### code chunk number 93: geos.Rnw:2520-2521
###################################################
m.valid.pr$res <- resid.kr 


###################################################
### code chunk number 94: geos.Rnw:2523-2524 (eval = FALSE)
###################################################
## bubble(m.valid.pr, "res")


###################################################
### code chunk number 95: geos.Rnw:2539-2544
###################################################
nfold <- 3
part <- sample(1:nfold, 155, replace = TRUE)
sel <- (part != 1)
m.model <- meuse[sel,]
m.valid <- meuse[-sel,]


###################################################
### code chunk number 96: geos.Rnw:2563-2565
###################################################
v.fit <- vgm(.59, "Sph", 874, .04)
cv155 <- krige.cv(log(zinc)~1, meuse, v.fit, nfold=5, verbose=FALSE)


###################################################
### code chunk number 97: geos.Rnw:2567-2568 (eval = FALSE)
###################################################
## bubble(cv155, "residual", main = "log(zinc): 5-fold CV residuals")


###################################################
### code chunk number 98: geos.Rnw:2587-2595
###################################################
print(bubble(cv155, "residual", main = "log(zinc): 5-fold CV residuals",
    maxsize = 1.5, col = c("green", "red")))


###################################################
### code chunk number 99: geos.Rnw:2608-2609
###################################################
summary(cv155)


###################################################
### code chunk number 100: geos.Rnw:2631-2632 
###################################################
g.cv <- gstat.cv(g, nmax=40)


###################################################
### code chunk number 101: geos.Rnw:2673-2675
###################################################
v1.fit <- vgm(0.591, "Sph", 897, .0507)
v2.fit <- vgm(0.591, "Sph", 897, add.to = vgm(0.0507, "Sph", 40))


###################################################
### code chunk number 102: geos.Rnw:2689-2694
###################################################
set.seed(13331)
cv155.1 <- krige.cv(log(zinc)~1, meuse, v1.fit, nfold=5, verbose=FALSE)
set.seed(13331)
cv155.2 <- krige.cv(log(zinc)~1, meuse, v2.fit, nfold=5, verbose=FALSE)
summary(cv155.1$residual-cv155.2$residual)


###################################################
### code chunk number 103: geos.Rnw:2710-2713
###################################################
b1 <- krige(log(zinc)~1, meuse, meuse.grid, v1.fit, block = c(40,40))$var1.var
b2 <- krige(log(zinc)~1, meuse, meuse.grid, v2.fit, block = c(40,40))$var1.var
summary((b1-b2)/b1)


###################################################
### code chunk number 104: geos.Rnw:2820-2821
###################################################
lzn.sim <- krige(log(zinc)~1, meuse, meuse.grid, v.fit, nsim = 6, nmax=40)


###################################################
### code chunk number 106: geos.Rnw:2841-2850
###################################################
print(spplot(lzn.sim, col.regions=pal(7), cuts=6))


###################################################
### code chunk number 107: geos.Rnw:2884-2896
###################################################
area <-
structure(c(181130.059662363, 180917.131281033, 180769.007189672,
180676.429632572, 180611.625342602, 180463.501251241, 180389.439205561,
180556.078808342, 180694.945143992, 180796.780456802, 180824.553723932,
180898.615769613, 181000.451082423, 181130.059662363, 181241.152730884,
181268.925998014, 181194.863952334, 181130.059662363, 333718.312421053,
333491.307789474, 333245.386105263, 332990.005894737, 332781.918315789,
332554.913684211, 332498.162526316, 332413.035789474, 332592.747789474,
332810.293894737, 333008.922947368, 333188.634947368, 333358.888421053,
333491.307789474, 333566.976, 333614.268631579, 333793.980631579,
333718.312421053), .Dim = as.integer(c(18, 2)))
area.sp = SpatialPolygons(list(Polygons(list(Polygon(area)), "area")))


###################################################
### code chunk number 108: geos.Rnw:2898-2905
###################################################
# set eval=TRUE if change; this section is repeated in the FIG code
nsim <- 1000
cutoff <- 500
sel.grid <- meuse.grid[area.sp, ]
lzn.sim <- krige(log(zinc)~1, meuse, sel.grid, v.fit, nsim = nsim, nmax=40)
res <- apply(as.data.frame(lzn.sim)[1:nsim], 2, function(x) mean(x >            
log(cutoff)))


###################################################
### code chunk number 109: geos.Rnw:2907-2908 (eval = FALSE)
###################################################
## hist(res, main = paste("fraction above", cutoff), xlab = NULL, ylab = NULL)


###################################################
### code chunk number 110: geos.Rnw:2922-2924
###################################################
bkr <- krige(log(zinc)~1, meuse, area.sp, v.fit)
1 - pnorm(log(cutoff), bkr$var1.pred, sqrt(bkr$var1.var))


###################################################
### code chunk number 111: geos.Rnw:2930-2943
###################################################
layout(matrix(1:2, 1, 2))
omar <- par("mar")
par(mar = rep(0,4))
image(meuse.grid["part.a"], col = 'gray') #$
lines(area, col = 'red')
par(mar = c(2,2,0.5,0)+.1)
hist(res, main = NULL, xlab = NULL, ylab = NULL)
par(mar = omar)


###################################################
### code chunk number 112: geos.Rnw:2982-2986
###################################################
table(meuse$soil) #$
s1.fit <- fit.variogram(variogram(I(soil==1)~1,meuse), vgm(1, "Sph", 800, 1))
s1.sim <- krige(I(soil==1)~1, meuse, meuse.grid, s1.fit, nsim = 6, 
   indicators = TRUE, nmax = 40)


###################################################
### code chunk number 114: geos.Rnw:3006-3014
###################################################
print(spplot(s1.sim, cuts=1, col.regions=pal(3)[c(1,3)]))


###################################################
### code chunk number 115: geos.Rnw:3064-3067 (eval = FALSE)
###################################################
## m1 <- sapply(1:155, function(x) mean(krige(log(zinc)~1, meuse[-x,],
##     meuse.grid, v.fit)$var1.var))
## which(m1 == min(m1)) #$


###################################################
### code chunk number 116: geos.Rnw:3081-3082 (eval = FALSE)
###################################################
## plot(sort(m1))


###################################################
### code chunk number 117: geos.Rnw:3098-3105 (eval = FALSE)
###################################################
## cutoff <- 1000
## f <- function(x) {
##     kr = krige(log(zinc)~1, meuse[-x,], meuse.grid, v.fit)
##     mean(abs(pnorm((kr$var1.pred - log(cutoff))/sqrt(kr$var1.var)) - 0.5))
## }
## m2 <- sapply(1:155, f)
## which(m2 == max(m2))


###################################################
### code chunk number 118: geos.Rnw:3111-3130
###################################################
source("m1m2.R")
layout(matrix(1:2, 1, 2))
omar <- par("mar")
par(mar = rep(0,4))
image(meuse.grid, col = grey(.8))
points(meuse)
points(meuse[m1 < quantile(m1,.1),], pch=1, col = 'red')
points(meuse[m1 > quantile(m1,.9),], pch=16, col = 'darkgreen')
image(meuse.grid, col = grey(.8))
points(meuse)
points(meuse[m2 < quantile(m2,.1),], pch=16, col = 'darkgreen')
points(meuse[m2 > quantile(m2,.9),], pch=1, col = 'red')
par(mar = omar)


###################################################
### code chunk number 119: geos.Rnw:3221-3223
###################################################
## library(RandomFields)
## ver <- packageDescription("RandomFields")$Version


###################################################
### code chunk number 120: geos.Rnw:3237-3238 
###################################################
## PrintModelList()


###################################################
### code chunk number 121: geos.Rnw:3270-3272 
###################################################
#library(geoR)
#plot(variog(as.geodata(meuse["zinc"]), max.dist=1500))


###################################################
### code chunk number 122: geos.Rnw:3319-3320 (eval = FALSE)
###################################################
## vignette("st", package = "gstat")



