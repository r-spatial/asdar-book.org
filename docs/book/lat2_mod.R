###################################################
# lat2_mod.R
# packages: sp, rgdal, spdep, nlme, lmtest, sandwich, mgcv, MASS, spgwr
# datasets: NY_data.zip


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

#fname <- zip.file.extract(file="NY_nb.gal", zipname = "NY_data.zip")
#file.copy(fname, "./NY_nb.gal", overwrite=TRUE)

unzip(zipfile="NY_data.zip")

library(sp)
library(rgdal)
NY8 <- readOGR(".", "NY8_utm18")
TCE <- readOGR(".", "TCE")
library(spdep)
NY_nb <- read.gal("NY_nb.gal", region.id=row.names(as(NY8, "data.frame")))


###################################################
### chunk number 10: 
###################################################
nylm <- lm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8)
summary(nylm)
NY8$lmresid <- residuals(nylm)


###################################################
### chunk number 11: 
###################################################
gry <- grey.colors(9, 0.95, 0.55, 2.2)
TCEpts <- list("sp.points", TCE, pch=16, col="grey5")
spplot(NY8, "lmresid", sp.layout=list(TCEpts), col.regions=colorRampPalette(gry)(7), at=seq(-2.5,5,length.out=6))


###################################################
### chunk number 12: 
###################################################
NYlistw<-nb2listw(NY_nb, style = "B")
lm.morantest(nylm, NYlistw)


###################################################
### chunk number 13: 
###################################################
library(spatialreg)
nysar<-spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME , data=NY8, listw=NYlistw)
summary(nysar)


###################################################
### chunk number 15: 
###################################################
nylmw <- lm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, weights=POP8)
summary(nylmw)
NY8$lmwresid <- residuals(nylmw)


###################################################
### chunk number 16: 
###################################################
TCEpts <- list("sp.points", TCE, pch=16, col="grey5")
spplot(NY8, "lmwresid", sp.layout=list(TCEpts), col.regions=colorRampPalette(gry)(7), at=seq(-2.5,5,length.out=6))


###################################################
### chunk number 17: 
###################################################
lm.morantest(nylmw, NYlistw)


###################################################
### chunk number 18: 
###################################################
nysarw<-spatialreg::spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME , data=NY8, listw=NYlistw, weights=POP8)
summary(nysarw)


###################################################
### chunk number 19: 
###################################################
nycar<-spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME , data=NY8, family="CAR",
   listw=NYlistw)
summary(nycar)


###################################################
### chunk number 21: 
###################################################
nycarw<-spatialreg::spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, family="CAR",
   listw=NYlistw, weights=POP8)
summary(nycarw)


###################################################
### chunk number 22: 
###################################################
nysarwM<-spatialreg::spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, family="SAR",
   listw=NYlistw, weights=POP8, method="Matrix")


###################################################
### chunk number 23: 
###################################################
summary(nysarwM)


###################################################
### chunk number 24: 
###################################################
1/range(eigenw(NYlistw))
nysar_ll<-spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, family="SAR",
   listw=NYlistw, llprof=100)
nysarw_ll<-spatialreg::spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, family="SAR",
   listw=NYlistw, weights=POP8, llprof=100)


###################################################
### chunk number 25: 
###################################################
ylim <- range(c(nysarw_ll$llprof$ll, nysar_ll$llprof$ll), na.rm=TRUE)
plot(nysarw_ll$llprof$lambda, nysarw_ll$llprof$ll, type="l", xlab=expression(lambda), ylab="log likelihood", ylim=ylim, lwd=2)
abline(v=nysarw_ll$lambda)
abline(h=nysarw_ll$LL)
lines(nysar_ll$llprof$lambda, nysar_ll$llprof$ll, lty=2, lwd=2)
abline(v=nysar_ll$lambda, lty=2)
abline(h=nysar_ll$LL, lty=2)
legend("bottom", legend=c("weighted SAR", "SAR"), lty=c(1,2), lwd=2, bty="n")


###################################################
### chunk number 26: 
###################################################
nysmaw<-spatialreg::spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, family="SMA",
   listw=NYlistw, weights=POP8)
summary(nysmaw)


###################################################
### chunk number 27: 
###################################################
library(nlme)

NY8$x<-coordinates(NY8)[,1]/1000
NY8$y<-coordinates(NY8)[,2]/1000

sp1 <- corSpatial(1, form = ~ x + y, type = "gaussian") 
scor<-Initialize(sp1, as(NY8, "data.frame")[,c("x", "y")], nugget=FALSE)


###################################################
### chunk number 29:  
###################################################
spmodel<-lme(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, random=~1|AREAKEY, data=as(NY8, "data.frame"), correlation=scor, method="ML")


###################################################
### chunk number 30: 
###################################################
summary(spmodel)


###################################################
### chunk number 31: 
###################################################
library(lmtest)
bptest(nylm)


###################################################
### chunk number 32: 
###################################################
library(sandwich)
coeftest(nylm)
coeftest(nylm, vcov=vcovHC(nylm, type="HC4"))


###################################################
### chunk number 33: 
###################################################
NYlistwW <- nb2listw(NY_nb, style = "W")
res <- lm.LMtests(nylm, listw=NYlistwW, test="all")
tres <- t(sapply(res, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tres) <- c("Statistic", "df", "p-value")
printCoefmat(tres)


###################################################
### chunk number 34: 
###################################################
nylag <- lagsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW)
summary(nylag)
bptest.Sarlm(nylag)


###################################################
### chunk number 35: 
###################################################
nymix <- lagsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW, type="mixed")
nymix
#anova(nymix, nylag)
LR.Sarlm(nymix, nylag)

###################################################
### chunk number 36: 
###################################################
nyerr <- errorsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW)
summary(nyerr)


###################################################
### chunk number 37: 
###################################################
nystsls <- stsls(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW)
summary(nystsls)


###################################################
### chunk number 38: 
###################################################
nystslsR <- stsls(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW, robust=TRUE)
summary(nystslsR)


###################################################
### chunk number 39: 
###################################################
nyGMerr <- GMerrorsar(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW)
summary(nyGMerr)


###################################################
### chunk number 40: 
###################################################
vv <- nyGMerr$vv
lambda <- nyGMerr$lambda
s2 <- nyGMerr$s2
lseq <- seq(0, 0.4, 0.01)
s2seq <- seq(0.3, 0.55, 0.01)
lsseq <- as.matrix(expand.grid(lseq, s2seq))
res <- numeric(nrow(lsseq))
for (i in seq(along=res)) res[i] <- spatialreg:::.kpgm(lsseq[i,,drop=TRUE], v=vv, verbose=TRUE)
SGDF <- SpatialPixelsDataFrame(lsseq, data=data.frame(fn=res))
fullgrid(SGDF) <- TRUE


###################################################
### chunk number 41: 
###################################################
image(SGDF, "fn", col=grey.colors(10, 1, 0.55, 2.2), axes=TRUE)
title(xlab=expression(lambda), ylab=expression(sigma^2))
contour(SGDF, "fn", add=TRUE)
points(c(lambda, nyerr$lambda), c(s2, nyerr$s2), pch=c(4, 3), lwd=2)
text(c(lambda, nyerr$lambda), c(s2, nyerr$s2), labels=c("GM", "ML"), pos=c(2, 4), offset=0.5)


###################################################
### chunk number 42: 
###################################################
library(mgcv)
### change for 1.5-2
if (packageDescription("mgcv")$Version != "1.5-2") {
  nyGAM1 <- gam(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME+s(x,y), weights=POP8, data=NY8)
} else {
  nyGAM1 <- gam(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME+s(x,y), weights=POP8, data=as(NY8, "data.frame"))
}
anova(nylmw, nyGAM1, test="Chisq")


###################################################
### chunk number 44: 
###################################################
nyGLMp <- glm(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(POP8)), data=NY8, family="poisson")


###################################################
### chunk number 45:  
###################################################
summary(nyGLMp)


###################################################
### chunk number 47: 
###################################################
NY8$lmpresid <- residuals(nyGLMp, type="deviance")
lm.morantest(nyGLMp, listw=NYlistwW)


###################################################
### chunk number 48: 
###################################################
TCEpts <- list("sp.points", TCE, pch=16, col="grey5")
spplot(NY8, "lmpresid", sp.layout=list(TCEpts), col.regions=grey.colors(9, 0.95, 0.55, 2.2), at=seq(-3,5,length.out=9))


###################################################
### chunk number 49: 
###################################################
if (packageDescription("mgcv")$Version != "1.5-2") {
  nyGAMp <- gam(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(POP8))+s(x,y), data=NY8, family="poisson")
} else {
  NY8df <- as(NY8, "data.frame")
  nyGAMp <- gam(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(POP8))+s(x,y), data=NY8df, family="poisson")
}
summary(nyGAMp)


###################################################
### chunk number 50:  
###################################################
anova(nyGLMp, nyGAMp, test="Chisq")


###################################################
### chunk number 52: 
###################################################
nylam1 <- c(summary(nyGAMp)$edf)


###################################################
### chunk number 54:  
###################################################
#library(MASS)
#attach(as(NY8, "data.frame"))
#nyGLMMp <- glmmPQL(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(POP8)), data=NY8, family=poisson, random=~1|AREAKEY, correlation=scor)
# Error in lme.formula(fixed = zz ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME +  : 
#  offset() terms are not supported (nlme 3.1-157 https://svn.r-project.org/R-packages/trunk/nlme/ChangeLog)
#detach("as(NY8, \"data.frame\")")


###################################################
### chunk number 55:  
###################################################
#summary(nyGLMMp)


###################################################
### chunk number 57: 
###################################################
nySFE <- SpatialFiltering(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, nb=NY_nb, style="W", verbose=FALSE)
nylmSFE <- lm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME+fitted(nySFE), data=NY8)
summary(nylmSFE)
anova(nylm, nylmSFE)


###################################################
### chunk number 59:  !!NOTE!! Time-consuming ...
###################################################
# The seed and alpha= values here are adjusted to match the book
# but with a different seed, the stopping rule may cut in at
# a different place, though the order of the first chosen eigenvectos
# stays the same
set.seed(111)
nyME <- spatialreg::ME(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, offset=log(POP8), family="poisson", listw=NYlistwW, alpha=0.44)


###################################################
### chunk number 60: 
###################################################
nyME


###################################################
### chunk number 61: 
###################################################
NY8$eigen_24 <- fitted(nyME)[,1]
NY8$eigen_223 <- fitted(nyME)[,2]


###################################################
### chunk number 62: 
###################################################
spplot(NY8, c("eigen_24", "eigen_223"), col.regions=grey.colors(6, 0.95, 0.55, 2.2), cuts=5)


###################################################
### chunk number 63: 
###################################################
nyglmME <- glm(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(POP8))+fitted(nyME), data=NY8, family="poisson")


###################################################
### chunk number 64:  
###################################################
summary(nyglmME)


###################################################
### chunk number 66:  
###################################################
anova(nyGLMp, nyglmME, test="Chisq")


###################################################
### chunk number 69: 
###################################################
library(spgwr)


###################################################
### chunk number 70:  !!NOTE!! Time-consuming ...
###################################################
bwG <- gwr.sel(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, gweight=gwr.Gauss)
gwrG <- gwr(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, bandwidth=bwG, gweight=gwr.Gauss, hatmatrix=TRUE)


###################################################
### chunk number 71: 
###################################################
gwrG


###################################################
### chunk number 72:  !!NOTE!! Time-consuming ...
###################################################
gbwG <- ggwr.sel(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(POP8)), data=NY8, family="poisson", gweight=gwr.Gauss)
ggwrG <- ggwr(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(POP8)), data=NY8, family="poisson", bandwidth=gbwG, gweight=gwr.Gauss)


###################################################
### chunk number 73:  
###################################################
ggwrG


###################################################
### chunk number 75: 
###################################################
TCEpts <- list("sp.points", TCE, pch=16, col="grey5")
spplot(ggwrG$SDF, "PEXPOSURE", sp.layout=list(TCEpts), col.regions=grey.colors(7, 0.95, 0.55, 2.2), cuts=6)


###################################################
### chunk number 76: 
###################################################
pairs(as(ggwrG$SDF, "data.frame")[,2:5])


