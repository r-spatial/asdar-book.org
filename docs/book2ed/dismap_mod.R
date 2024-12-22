### R code from vignette source 'dismap.Rnw'
### Encoding: UTF-8
# data: brainNM.RData
###################################################
### code chunk number 7: dismap.Rnw:183-191
###################################################
#library(maptools)
library(spdep)
library(sp)

#nc_file <- system.file("shapes/sids.shp", package="spData")[1]
nc_file <- system.file("shapes/sids.gpkg", package="spData")[1]
#llCRS <- CRS("+proj=longlat +datum=NAD27")
#nc <- readShapePoly(nc_file, ID="FIPSNO", proj4string=llCRS)
nc <- as(sf::st_read(nc_file), "Spatial")
#rn <- sapply(slot(nc, "polygons"), function(x) slot(x, "ID"))
rn <- as.character(nc$FIPSNO)
gal_file <- system.file("weights/ncCR85.gal", package="spData")[1]
ncCR85 <- read.gal(gal_file, region.id = rn)


###################################################
### code chunk number 9: dismap.Rnw:243-244
###################################################
nc$Observed <- nc$SID74


###################################################
### code chunk number 10: dismap.Rnw:252-255
###################################################
nc$Population <- nc$BIR74
r <- sum(nc$Observed)/sum(nc$Population)
nc$Expected <- nc$Population*r


###################################################
### code chunk number 11: dismap.Rnw:263-264
###################################################
nc$SMR <- nc$Observed/nc$Expected


###################################################
### code chunk number 12: dismap.Rnw:270-291
###################################################
library(RColorBrewer)

#Used method proposed by Nicky Best
logSMR <- log(nc$SMR[nc$SMR>0])
nsteps <- 7
step <- (max(logSMR)-min(logSMR))/nsteps
brks <- exp(min(logSMR)+(0:nsteps)*step)
brks[1] <- 0
cols <- c(rev(brewer.pal(3, "Blues")), brewer.pal(4, "Reds"))
grps <- as.ordered(cut(nc$SMR, brks, include.lowest=TRUE))
plot(nc, col=cols[unclass(grps)], axes = FALSE)
box()
degAxis(1)
degAxis(2, at=c(34,35,36,37)) 
legend("bottomleft",legend=levels(grps), fill=cols, bty="n",cex=0.8,y.intersp=0.8) 


###################################################
### code chunk number 13: dismap.Rnw:318-346
###################################################
library(epitools)
CISMR <- pois.exact(nc$Observed, nc$Expected)
plot(1,1, type="n", xlim=c(1,100), ylim=c(0,9),
  main= "Confidence intervals of the SMR",
  xlab="County", ylab="Relative Risk", xaxt="n")
abline(h=1, lty=2)

for(i in 1:100) {
        if(CISMR$lower[i]>1 ) {
            #sig.col <- 'red'
            sig.col <- brewer.pal(4, "Reds")[4]
            col <- sig.col
            lty <- 2
            text(i, CISMR$upper[i]+.31, nc$NAME[i],
                srt=90, col=sig.col, cex=.85)
        } else {
            col <- "black"
            lty <- 1
        }
        lines(c(i,i), c(CISMR$lower[i],CISMR$upper[i]), col=col, lty=lty)
        points(i, nc$SMR[i], pch=18, col=col)
}


###################################################
### code chunk number 14: dismap.Rnw:425-430
###################################################
library(DCluster)
eb <- empbaysmooth(nc$Observed, nc$Expected)
nc$EBPG <- eb$smthrr
eb$nu
eb$alpha


###################################################
### code chunk number 15: dismap.Rnw:432-434
###################################################
ebnu <- eb$nu
ebalpha <- eb$alpha


###################################################
### code chunk number 16: dismap.Rnw:463-503
###################################################
nc$pvalpois <- ppois(nc$Observed, nc$Expected, lower.tail=FALSE)

nbparam <- calculate.mle(as(nc, "data.frame"), model="negbin")
nc$pvalnegbin <- pnbinom(nc$Observed, size=nbparam$size, prob=nbparam$prob,
  lower.tail=FALSE)

colorkeypval <- list(labels=as.character(c(0, 0.01, 0.05, 0.1, .5, 1)), 
  at=(0:5)/5, height=.5)

pvalcols <- brewer.pal(5, "Reds")

print(spplot(nc, c("pvalpois","pvalnegbin"), col.regions=rev(pvalcols), 
  at=c(0, 0.01, 0.05, 0.1, .5, 1), axes=TRUE))


###################################################
### code chunk number 17: dismap.Rnw:558-560
###################################################
ebln <- lognormalEB(nc$Observed, nc$Expected)
nc$EBLN <- exp(ebln$smthrr)


###################################################
### code chunk number 18: dismap.Rnw:600-603
###################################################
library(spdep)
EBMarshall <- EBest(nc$Observed, nc$Expected)
nc$EBMarshall <- EBMarshall[,2]


###################################################
### code chunk number 19: dismap.Rnw:610-631
###################################################
#Display all the risk estimates
atcol <- (0:5)*max(nc$SMR)/5
colorkey <- list(labels=as.character(c(formatC(brks, format="f", dig=2))),
  at=atcol,  height=.5)

print(spplot(nc, c("SMR","EBPG", "EBLN", "EBMarshall"), col.regions=cols, 
  at=brks, axes = TRUE))


###################################################
### code chunk number 20: dismap.Rnw:719-720
###################################################
nc$EBMrshloc <- EBlocal(nc$Observed, nc$Expected, ncCR85)$est


###################################################
### code chunk number 21: dismap.Rnw:728-739
###################################################
print(spplot(nc, c("EBMarshall", "EBMrshloc"), col.regions=cols, at=brks))


###################################################
### code chunk number 22: dismap.Rnw:784-795
###################################################
boxplot(as(nc, "data.frame")[,c("SMR", "EBPG", "EBLN", "EBMarshall", "EBMrshloc")], cex.lab=.5, las=1, horizontal=TRUE)


###################################################
### code chunk number 30: dismap.Rnw:1219-1221
###################################################
library(R2BayesX)
nc$AREAID <- 1:nrow(nc)


###################################################
### code chunk number 31: dismap.Rnw:1223-1226 
###################################################
pgbayesx <- bayesx(Observed ~ sx(AREAID, bs="re"), 
   offset= log(nc$Expected),
   family = "poisson", data = as(nc, "data.frame"))


###################################################
### code chunk number 37: dismap.Rnw:1276-1277
###################################################
library(CARBayes)


###################################################
### code chunk number 38: dismap.Rnw:1279-1284 
###################################################
set.seed(1)
#ncdf <- as(nc, "data.frame")
#attach(ncdf)
#suppressOutput <- capture.output(pgcarbayes <- poisson.independent(formula = Observed ~ offset(log(Expected)),
#   burnin=5000, n.sample=10000))
#detach(ncdf)
# CARBayes_3.0
#pgcarbayes <- independent.re(formula = Observed ~ offset(log(Expected)), data=nc, family="poisson", burnin=5000, n.sample=10000, verbose=FALSE)
# CARBayes 4.0
# CARBayes 4.4 S.independent withdrawn
#pgcarbayes <- S.independent(formula = Observed ~ offset(log(Expected)), data=nc, family="poisson", burnin=5000, n.sample=10000, verbose=FALSE)


###################################################
### code chunk number 40: dismap.Rnw:1298-1301
###################################################
nc$PGBAYESX <- pgbayesx$fitted.values[order(pgbayesx$bayesx.setup$order),2]/nc$Expected
#nc$PGINLA <- pginla$summary.fitted.values$mean/nc$Expected
#nc$PGCARBAYES <- pgcarbayes$fitted.values[,1]/nc$Expected
#nc$PGCARBAYES <- pgcarbayes$fitted.values/nc$Expected


###################################################
### code chunk number 41: dismap.Rnw:1313-1322
###################################################
print(spplot(nc, c("PGBAYESX"#, "PGCARBAYES"
),
 col.regions=cols,  at=brks, axes = TRUE))


###################################################
### code chunk number 42: dismap.Rnw:1456-1470
###################################################
nc$RATIO <- nc$NWBIR74/nc$BIR74
print(spplot(nc, "RATIO", col.regions=brewer.pal(4, "Reds"), at=.8*0:4/4))


###################################################
### code chunk number 46: dismap.Rnw:1530-1541
###################################################
nc$nwprop <- nc$NWBIR74/nc$BIR74


###################################################
### code chunk number 60: dismap.Rnw:1894-1895 
###################################################
ncgra <- nb2gra(ncCR85)


###################################################
### code chunk number 61: dismap.Rnw:1912-1916 
###################################################
bymbayesx <- bayesx(Observed~nwprop+sx(AREAID, bs="re") +
   sx(FIPSNO,bs="spatial", map=ncgra),
   offset= log(nc$Expected),
   family="poisson", data=as(nc, "data.frame"))


###################################################
### code chunk number 63: dismap.Rnw:1934-1940 
###################################################
set.seed(1)
#ncdf <- as(nc, "data.frame")
#attach(ncdf)
#suppressOutput <- capture.output(obj <- poisson.bymCAR(Observed ~ nwprop + offset(log(Expected)), 
#   W=nb2mat(ncCR85, style="B"), 
#   n.sample=30000, burnin=20000, thin=10))
# CARBayes_3.0
#obj <- bymCAR.re(Observed ~ nwprop + offset(log(Expected)), data=nc, family="poisson", W=nb2mat(ncCR85, style="B"), n.sample=30000, burnin=20000, thin=10, verbose=FALSE)
# CARBayes 4.0
obj <- S.CARbym(Observed ~ nwprop + offset(log(Expected)), data=nc, family="poisson", W=nb2mat(ncCR85, style="B"), n.sample=30000, burnin=20000, thin=10, verbose=FALSE)
print(obj) # added for CARBayes >= 1.6.0 
#detach(ncdf)


###################################################
### code chunk number 65: dismap.Rnw:1954-1957
###################################################
nc$BAYESX <- bymbayesx$fitted.values[order(bymbayesx$bayesx.setup$order),2]/nc$Expected
#nc$INLA <- INLA_BYM$summary.fitted.values[,1]/nc$Expected
#nc$CARBayes <- obj$fitted.values[,1]/nc$Expected
nc$CARBayes <- obj$fitted.values/nc$Expected


###################################################
### code chunk number 66: dismap.Rnw:1972-1982
###################################################
print(spplot(nc, c("BAYESX", "CARBayes"), 
   at=brks, axes=TRUE, col.regions=cols))


###################################################
### code chunk number 67: dismap.Rnw:2001-2045
###################################################
oldpar <- par(mfrow=c(2,2))
#Intercept
d1 <- density(attr(bymbayesx$fixed.effects, "sample")[,1])
plot(as.data.frame(d1[1:2]), main="Intercept", type="l")
#lines(INLA_BYM$marginals.fixed[[1]], lty=2)
if (packageVersion("CARBayes") >= "1.6") {lines(density( obj$samples$beta[,1], bw=d1$bw), lty=2) 
} else lines(density( obj$samples.beta[,1], bw=d1$bw), lty=2)
legend("topleft", legend=c("BayesX", "CARBayes"), bty="n", 
   lty=1:2, cex=.65)

#Nwprop
d2 <- density(attr(bymbayesx$fixed.effects, "sample")[,2])
plot(as.data.frame(d2[1:2]), main="nwprop", type="l")
#lines(INLA_BYM$marginals.fixed[[2]], lty=2)
if (packageVersion("CARBayes") >= "1.6") {lines(density( obj$samples$beta[,2], bw=d2$bw), lty=2) 
} else lines(density( obj$samples.beta[,2], bw=d2$bw), lty=2)
legend("topleft", legend=c("BayesX", "CARBayes"), bty="n",
   lty=1:2, cex=.65)

#FIXME: Add variances here
#Density of posterio means of non-spatial random effects
# fix bug from updating R2BayesX to 0.3-1
sxname <- names(bymbayesx$effects)[grep("sx\\(AREAID\\)", names(bymbayesx$effects))]
d3 <- density(bymbayesx$effects[[sxname]]$Mean)
#d3 <- density(bymbayesx$effects[["sx(AREAID)"]]$Mean)
plot(as.data.frame(d3[1:2]), main="Non-spatial r.eff.", type="l")
#lines(density(INLA_BYM$summary.random$FIPS$mean, bw=d3$bw), lty=2)
# from CARBayes 4.0 only single re
#if (packageVersion("CARBayes") >= "1.6") {lines(density( apply( obj$samples$phi,2,mean), bw=d3$bw), lty=2) 
#} else lines(density(apply( obj$samples.phi,,mean), bw=d3$bw), lty=2)
#lines(density(MCMCres$mean$u, bw=d3$bw), lty=4)
legend("topleft", legend=c("BayesX", "CARBayes"),  bty="n",
   lty=1:2, cex=.65)


#Variance of spatial random effects
#FIXME: check the prior that INLA uses for this. Seems too smoothed
# fix bug from updating R2BayesX to 0.3-1
sxname <- names(bymbayesx$effects)[grep("sx\\(FIPSNO\\)", names(bymbayesx$effects))]
d4 <- density(bymbayesx$effects[[sxname]]$Mean)
#d4 <- density(bymbayesx$effects[["sx(FIPSNO)"]]$Mean)
plot(as.data.frame(d4[1:2]), main="Spatial r.eff.", type="l")
#lines(density(INLA_BYM$summary.random$AREAID$mean, bw=d4$bw), lty=2)
# from CARBayes 4.0 only single re
#if (packageVersion("CARBayes") >= "1.6") {lines(density( apply( obj$samples$theta,2,mean), bw=d4$bw), lty=2) 
#} else lines(density(apply( obj$samples.theta,2,mean), bw=d4$bw), lty=2)
#lines(density(MCMCres$mean$v, bw=d4$bw), lty=4)
legend("topleft", legend=c("BayesX", "CARBayes"), bty="n",
   lty=1:2, cex=.65)

par(oldpar)

###################################################
### code chunk number 68: dismap.Rnw:2075-2078 (eval = FALSE)
###################################################
bayesxps <- bayesx(Observed~sx(nwprop, bs="ps", knots=10),
   offset= log(nc$Expected),
   family="poisson", data=as(nc, "data.frame"))


###################################################
### code chunk number 69: dismap.Rnw:2090-2095 (eval = FALSE)
###################################################
nc$long <- coordinates(nc)[,1]
nc$lat <- coordinates(nc)[,2]
bayesxte <- bayesx(Observed~sx(long,lat, bs="te"),
   offset= log(nc$Expected),
   family="poisson", data=as(nc, "data.frame"))


###################################################
### code chunk number 71: dismap.Rnw:2114-2121
###################################################
plot(bayesxps)


###################################################
### code chunk number 72: dismap.Rnw:2131-2142
###################################################
#plot(bayesxte, image=TRUE)


###################################################
### code chunk number 73: dismap.Rnw:2236-2241 (eval = FALSE)
###################################################
## #
## #I do not know why but if these objects are not removed I 
## #get 10 pages of warnings!!
## #
## rm(list=c("EBMarshall","Observed","Expected","SMR"))


###################################################
### code chunk number 74: dismap.Rnw:2245-2246
###################################################
set.seed(1)


###################################################
### code chunk number 75: dismap.Rnw:2251-2254
###################################################
chtest <- achisq.test(Observed~offset(log(Expected)), 
   as(nc, "data.frame"), "multinom", 999)
chtest


###################################################
### code chunk number 76: dismap.Rnw:2269-2270
###################################################
1- pchisq(chtest$t0, 100-1)


###################################################
### code chunk number 77: dismap.Rnw:2307-2308
###################################################
set.seed(1)


###################################################
### code chunk number 78: dismap.Rnw:2312-2314
###################################################
pwtest <- pottwhitt.test(Observed~offset(log(Expected)), 
   as(nc, "data.frame"), "multinom", 999)


###################################################
### code chunk number 79: dismap.Rnw:2323-2325
###################################################
Oplus<- sum(nc$Observed)
1- pnorm(pwtest$t0, Oplus*(Oplus-1), sqrt(2*100*Oplus*(Oplus-1)))


###################################################
### code chunk number 80: dismap.Rnw:2395-2398
###################################################
col.W <- nb2listw(ncCR85, zero.policy=TRUE)
moranI.test(Observed~offset(log(Expected)), as(nc, "data.frame"), 
   "negbin", 999, listw=col.W, n=length(ncCR85), S0=Szero(col.W))


###################################################
### code chunk number 81: dismap.Rnw:2450-2461
###################################################
data(nc.sids)
idx <- match(nc$NAME, rownames(nc.sids))
nc$x <- nc.sids$x[idx]
nc$y <- nc.sids$y[idx]
coords <- cbind(nc$x, nc$y)
dlist <- dnearneigh(coords, 0, Inf)
dlist <- include.self(dlist)
dlist.d <- nbdists(dlist, coords)
phi <- 100
col.W.tango <- nb2listw(dlist, glist=lapply(dlist.d, 
  function(x, phi) {exp(-x/phi)}, phi=phi), style="C")


###################################################
### code chunk number 82: dismap.Rnw:2475-2476
###################################################
set.seed(1)


###################################################
### code chunk number 83: dismap.Rnw:2478-2480
###################################################
tango.test(Observed~offset(log(Expected)), as(nc, "data.frame"), "negbin", 999, 
   listw=col.W.tango, zero.policy=TRUE)


###################################################
### code chunk number 84: dismap.Rnw:2533-2536
###################################################
sidsgam <- opgam(data=as(nc, "data.frame"),  radius=30, step=10, alpha=.002)
gampoints <- SpatialPoints(sidsgam[,c("x", "y")]*1000, 
   CRS("+proj=utm +zone=18 +datum=NAD27"))


###################################################
### code chunk number 85: dismap.Rnw:2539-2543
###################################################
library(rgdal)
ll <- CRS("+proj=longlat +datum=NAD27")
gampoints <- spTransform(gampoints, ll)
gam.layout <- list("sp.points", gampoints)


###################################################
### code chunk number 86: dismap.Rnw:2627-2628
###################################################
set.seed(1234)


###################################################
### code chunk number 88: dismap.Rnw:2639-2643
###################################################
mle <- calculate.mle(as(nc, "data.frame"), model="negbin")
thegrid <- as(nc, "data.frame")[,c("x","y")]
knresults <- opgam(data=as(nc, "data.frame"), thegrid=thegrid, alpha=.05,
   iscluster=kn.iscluster, fractpop=0.15, R=99, model="negbin", mle=mle)


###################################################
### code chunk number 90: dismap.Rnw:2653-2682
###################################################

clusters <- get.knclusters(as(nc, "data.frame"), knresults)
i <- which.max(knresults$statistic)

nc$KNcluster <- "county"
nc$KNcluster[clusters[[i]]] <- "cluster"
nc$KNcluster[clusters[[i]][1]] <- "centre"
nc$KNcluster <- as.factor(nc$KNcluster)
bp0 <- brewer.pal(4, "Reds")

print(spplot(nc, "KNcluster",# main="Kulldorff's method",
  col.regions=c(bp0[4], bp0[3], bp0[1]), sp.layout=list("sp.points",
  gampoints, col=gray(.4), pch=4)))


###################################################
### code chunk number 92: dismap.Rnw:2764-2768
###################################################
stone.stat(as(nc, "data.frame"), region=which(nc$NAME=="Anson"))
st <- stone.test(Observed~offset(log(Expected)), as(nc, "data.frame"), 
   model="negbin", 99, region=which(nc$NAME=="Anson"))
st


###################################################
### code chunk number 93: dismap.Rnw:2846-2861
###################################################
#Temporal trend
library(spacetime)
library(xts)
#Load STFDF
load("brainNM.RData")
#nmf <- slot(brainst, "sp")

obs <- xts(brainst@data$Observed,
   as.Date(paste(as.character(brainst@data$Year), "-01-01", sep="")))
expt <- xts(brainst@data$Expected,
   as.Date(paste(as.character(brainst@data$Year), "-01-01", sep="")))

obsy <- apply.yearly(obs, sum)
expty <- apply.yearly(expt, sum)
smry <- obsy/expty


###################################################
### code chunk number 94: dismap.Rnw:2866-2874
###################################################
print(stplot(brainst[,,"SMR"], 1973:1991, at=c(0,.75, .9, 1, 1.1,1.25, 2, 3, 8),
   col.regions=brewer.pal(8, "Reds")) )


###################################################
### code chunk number 95: dismap.Rnw:2885-2892
###################################################
plot(smry, main="Standardised Mortality Ratio by Year")


###################################################
### code chunk number 96: dismap.Rnw:2938-2940
###################################################
library(spdep)
neib <- poly2nb(nmf, row.names=1:length(nmf))


###################################################
### code chunk number 98: dismap.Rnw:2983-2985 
###################################################
nmgra <- nb2gra(neib)
nmbayesx <- bayesx(Observed~IDLANLre+sx(Year, bs="rw1")+sx(ID, bs="spatial", map=nmgra),offset= log(brainst$Expected), family="poisson", data=as(brainst, "data.frame"))


###################################################
### code chunk number 101: dismap.Rnw:3044-3046
###################################################
# fix bug from updating R2BayesX to 0.3-1
sxname <- names(nmbayesx$effects)[grep("sx\\(ID\\)", names(nmbayesx$effects))]
nmf$SPBAYESX <- nmbayesx$effects[[sxname]]$Mean
#nmf$SPBAYESX <- nmbayesx$effects[["sx(ID)"]]$Mean


###################################################
### code chunk number 102: dismap.Rnw:3068-3089
###################################################
#Brain cancer in New Mexico: Temporal trend

plot(1973:1991, nmbayesx$effects[["sx(Year)"]]$Mean, type="l", lwd=1.5,
main="Temporal trend (BayesX))", ylim=c(-.25, .25), xlab="Year", ylab="")
lines(1973:1991, nmbayesx$effects[["sx(Year)"]]$"2.5%", lty=2)
lines(1973:1991, nmbayesx$effects[["sx(Year)"]]$"97.5%", lty=2)
abline(h=0, lty=3)



###################################################
### code chunk number 103: dismap.Rnw:3099-3113
###################################################
spl <- list(list("sp.points", losalamos, pch=19, col="red"),
   list("sp.text", coordinates(losalamos), "Los Alamos National Laboratory", pos=1, col="black", cex=0.7))
cols <- colorRampPalette(c(rev(brewer.pal(4, "Reds")), brewer.pal(4, "Blues")))
ats <- quantile(nmf$SPBAYESX, seq(0, 1, 1/9))
print(spplot(nmf, c("SPBAYESX"), at=ats, col.regions=cols(length(ats)-1), 
   col="grey", sp.layout=spl, main="Spatial effects"))


