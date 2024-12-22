
###################################################
# dismap_mod.R
# packages: sp, maptools, spdep, epitools, DCluster, rgdal
# datasets: 


###################################################
### chunk number 1: 
###################################################
rm(list=ls())


###################################################
### chunk number 7: 
###################################################
#library(maptools)
library(spdep)
library(sp)

#Load data
#nc_file <- system.file("shapes/sids.shp", package="spData")[1]
nc_file <- system.file("shapes/sids.gpkg", package="spData")[1]
#llCRS <- CRS("+proj=longlat +datum=NAD27")
#nc <- readShapePoly(nc_file, ID="FIPSNO", proj4string=llCRS)
nc <- as(sf::st_read(nc_file), "Spatial")
#rn <- sapply(slot(nc, "polygons"), function(x) slot(x, "ID"))
rn <- as.character(nc$FIPSNO)
gal_file <- system.file("weights/ncCR85.gal", package="spData")[1]
ncCR85 <- read.gal(gal_file, region.id=rn)


###################################################
### chunk number 9: 
###################################################
nc$Observed<-nc$SID74
nc$Population<-nc$BIR74#Population at risk; number of births
r<-sum(nc$Observed)/sum(nc$Population)
nc$Expected<-nc$Population*r

#Computed Standardised Mortality Ratio
nc$SMR<-nc$Observed/nc$Expected


###################################################
### chunk number 10: 
###################################################
#Display SMR 
logSMR<-log(nc$SMR[nc$SMR>0])
nsteps <- 5
step<-(max(logSMR)-min(logSMR))/nsteps
brks<-exp(min(logSMR)+(0:nsteps)*step)
brks[1]<-0
cols <- grey.colors(nsteps, 0.95, 0.55, 2.2)
grps<-as.ordered(cut(nc$SMR, brks, include.lowest=TRUE))
plot(nc, col=cols[unclass(grps)], axes = FALSE)
box()
degAxis(1)
degAxis(2, at=c(34,35,36,37)) 
legend("bottomleft",legend=levels(grps), fill=cols, bty="n",cex=0.8,y.intersp=0.8) 


###################################################
### chunk number 11: 
###################################################
library(epitools)

CISMR<-pois.exact(nc$Observed, nc$Expected)

plot(1,1, type="n", xlim=c(1,100), ylim=c(0,9),
  main= "Confidence intervals of the SMR",
  xlab="County", ylab="Relative Risk", xaxt="n")
abline(h=1, lty=2)

for(i in 1:100)
{
        if(CISMR$lower[i]>1 )
        {
                col<-gray(.4)
		lty<-2
                text(i, CISMR$upper[i]+.31, nc$NAME[i],
                srt=90, col=gray(.4), cex=.85)
        }
        else
        {
                col<-"black"
		lty<-1
        }

        lines(c(i,i), c(CISMR$lower[i],CISMR$upper[i]), col=col, lty=lty)
        points(i, nc$SMR[i], pch=18, col=col)
}


###################################################
### chunk number 12: 
###################################################
library(DCluster)

eb<-empbaysmooth(nc$Observed, nc$Expected)

nc$EBPG<-eb$smthrr


###################################################
### chunk number 14: 
###################################################
nc$pvalpois<-ppois(nc$Observed, nc$Expected, lower.tail=FALSE)

nbparam<-calculate.mle(as(nc, "data.frame"), model="negbin")
nc$pvalnegbin<-pnbinom(nc$Observed, size=nbparam$size, prob=nbparam$prob,
  lower.tail=FALSE)

colorkeypval<-list(labels=as.character(c(0, 0.01, 0.05, 0.1, .5, 1)), 
  at=(0:5)/5, height=.5)

pvalcols <- grey.colors(5, 0.95, 0.55, 2.2)

print(spplot(nc, c("pvalpois","pvalnegbin"), col.regions=rev(pvalcols), 
  at=c(0, 0.01, 0.05, 0.1, .5, 1), axes=TRUE, colorkey=colorkeypval ))



###################################################
### chunk number 15: 
###################################################
#Log-normal model
ebln<-lognormalEB(nc$Observed, nc$Expected)
nc$EBLN<-exp(ebln$smthrr)


###################################################
### chunk number 16: 
###################################################
#Compute Marshall risk estimator
EBMarshall<-EBest(nc$Observed, nc$Expected)
nc$EBMarshall<-EBMarshall[,2]


###################################################
### chunk number 17: 
###################################################
#Display all the risk estimates
atcol<-(0:5)*max(nc$SMR)/5
colorkey<-list(labels=as.character(c(formatC(brks, format="f", dig=2))),
  at=atcol,  height=.5)

cols <- grey.colors(5, 0.95, 0.45, 2.2)

print(spplot(nc, c("SMR","EBPG", "EBLN", "EBMarshall"), col.regions=cols, 
  at=brks, axes = TRUE, colorkey=colorkey))


###################################################
### chunk number 18: 
###################################################
nc$EBMrshloc<-EBlocal(nc$Observed, nc$Expected, ncCR85)$est


###################################################
### chunk number 19: 
###################################################
print(spplot(nc, c("EBMarshall", "EBMrshloc"), col.regions=cols, at=brks, colorkey=colorkey))


###################################################
### chunk number 20: 
###################################################
oopar <- par(mar=c(3,7,2,1)+0.1)
boxplot(as(nc, "data.frame")[,c("SMR", "EBPG", "EBLN", "EBMarshall", "EBMrshloc")], cex.lab=.5, las=1, horizontal=TRUE)
par(oopar)


###################################################
### chunk number 46: 
###################################################
set.seed(1)


###################################################
### chunk number 47: 
###################################################
chtest<-achisq.test(Observed~offset(log(Expected)), as(nc, "data.frame"), "multinom", 999)
chtest


###################################################
### chunk number 48: 
###################################################
1- pchisq(chtest$t0, 100-1)


###################################################
### chunk number 49: 
###################################################
set.seed(1)


###################################################
### chunk number 50: 
###################################################
pwtest<-pottwhitt.test(Observed~offset(log(Expected)), as(nc, "data.frame"), "multinom", 999)


###################################################
### chunk number 51: 
###################################################
Oplus<- sum(nc$Observed)
1- pnorm(pwtest$t0, Oplus*(Oplus-1), sqrt(2*100*Oplus*(Oplus-1)))


###################################################
### chunk number 52: 
###################################################
col.W <- nb2listw(ncCR85, zero.policy=TRUE)


moranI.test(Observed~offset(log(Expected)), as(nc, "data.frame"), "negbin", 999, 
   listw=col.W, n=length(ncCR85), S0=Szero(col.W) )


###################################################
### chunk number 53: 
###################################################
data(nc.sids)
#Centroids
idx<-match(nc$NAME, rownames(nc.sids))

nc$x<-nc.sids$x[idx]
nc$y<-nc.sids$y[idx]
#Calculate neighbours based on distance
coords<-cbind(nc$x, nc$y)

dlist<-dnearneigh(coords, 0, Inf)
dlist<-include.self(dlist)
dlist.d<-nbdists(dlist, coords)

#Calculate weights. They are globally standardised but it doesn't
#change significance.
phi<-100
col.W.tango<-nb2listw(dlist, glist=lapply(dlist.d, function(x, phi) {exp(-x/phi)}, 
  phi=phi), style="C")


###################################################
### chunk number 54: 
###################################################
set.seed(1)


###################################################
### chunk number 55: 
###################################################
tango.test(Observed~offset(log(Expected)), as(nc, "data.frame"), "negbin", 999, 
   listw=col.W.tango, zero.policy=TRUE)


###################################################
### chunk number 56: 
###################################################
sidsgam<-opgam(data=as(nc, "data.frame"),  radius=30, step=10, alpha=.002)
gampoints<-SpatialPoints(sidsgam[,c("x", "y")]*1000, 
   CRS("+proj=utm +zone=18 +datum=NAD27"))


###################################################
### chunk number 57: 
###################################################
library(rgdal)
ll <- CRS("+proj=longlat +datum=NAD27")
gampoints<-spTransform(gampoints, ll)
gam.layout<-list("sp.points", gampoints)


###################################################
### chunk number 58: 
###################################################
#Plot centroids
plot(nc, xlab="Easting", ylab="Northing")
#Plot points marked as clusters
points(gampoints, sidsgam$y, col=gray(.4), pch=4)



###################################################
### chunk number 59: 
###################################################
set.seed(1234)


###################################################
### chunk number 61: 
###################################################
mle <- calculate.mle(as(nc, "data.frame"), model="negbin")
thegrid <- as(nc, "data.frame")[,c("x","y")]
knresults<-opgam(data=as(nc, "data.frame"), thegrid=thegrid, alpha=.05,
   iscluster=kn.iscluster, fractpop=0.15, R=99, model="negbin", mle=mle)


###################################################
### chunk number 63: 
###################################################
clusters<-get.knclusters(as(nc, "data.frame"), knresults)
i<-which.max(knresults$statistic)

nc$KNcluster<-""
nc$KNcluster[clusters[[i]]]<-"cluster"
nc$KNcluster[clusters[[i]][1]]<-"centre"
nc$KNcluster<-as.factor(nc$KNcluster)

print(spplot(nc, "KNcluster", main="Kulldorff's method",
  xlab="Easting", ylab="Northing", col.regions=c(gray(1), gray(.5), gray(.8))))


###################################################
### chunk number 65: 
###################################################
stone.stat(as(nc, "data.frame"), region=which(nc$NAME=="Anson"))
st<-stone.test(Observed~offset(log(Expected)), as(nc, "data.frame"), model="negbin", 99, 
   region=which(nc$NAME=="Anson"))
st


