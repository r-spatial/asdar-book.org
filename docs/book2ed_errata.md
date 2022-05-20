# Errata 2nd edition
## hello
Page 7: PROJ.4 string requires +ellps from PROJ 4.9.

## cm
Page 34: For sp version 1.0-13 only, the construction of the matched sorted
SpatialPointsDataFrame failed, see sp SVN revision 1470, rspatial project on
R-forge.
Page 39: From maps version 3.0.0-1, the count of lines making up Japan changes from 51 to 34, and the bounding box changes.

## vis
Page 66: PROJ.4 string requires +ellps from PROJ 4.9.

## die
Page 88-89: From sp 1.2-4, a bug in the the calculation of great circle distances has been corrected, and the printed distance of 124.0994 should be 124.1372 and equivalently 125.8692 should be 125.9068.

Page 93: footnote 16: http://www.sph.emory.edu/~lwaller/WGindex.htm changed to http://web1.sph.emory.edu/users/lwaller/WGindex.htm. 

Page 94: From rgdal 1.2.\* and GDAL 2, integer values read from ESRI Shapefiles and specified with wide fields are treated as 64-bit integers. This breaks earlier assumptions, so that from rgdal 1.2.\*, the readOGR() integer64= argument is changed from default "allow.loss" to "no.loss", returning a string value rather than an integer (or in some settings a double value). In this case, "allow.loss" returning an R 32-bit signed integer is the intended behaviour.

Page 97: Access to the EFFIS WFS server has been terminated (WMS appears to continue at http://forest.jrc.ec.europa.eu/effis/applications/data-and-services/). The data are provided as an R object (layer names) and a shapefile in the Chapter bundle.

Page 98, chunk 58: after updating to spacetime 1.0-6, the stplot command has changed its arguments, and the dispatching object should be cast to STI from STIDF: stplot(as(Fires2, "STI"), number=3, sp.layout=spl, cex=0.5).

Page 108, chunks 98-100: If you run the commented-out code in chunk 99 (bug
reported by Manfred Jensen), you need to re-import the image in banded rather 
than native form, following changes in the RgoogleMaps package, adding:
```
myMap$myTile <- readPNG("MyTile2.png", native=FALSE)
```

Page 120, chunk 139: To correct errors in brackets, the lines in the middle of 
the code chunk at the foot of the page should read:
```
> crs <- CRS(proj4string(vsnow4buf))
> SGDF <- SpatialGridDataFrame(GRD, proj4string=crs, data=data.frame(o=o))
```

Page 121: drop legend as geoR unavailable (May 2022).

## cm2
Page 135: From GEOS 3.9.0 with Overlay-NG, the same slivers are found, but their order differs.

Page 147: R >= 3.6 uses a different default sampler giving for the same seed 991 instead of 979 and 999 for 1003 output counts. The tabulation on page 148 is also changed, with max. grid_regular becoming 77.

Section 5.3.3, foot of page 147, code chunk 72, tabulation of five number summaries. Because over() now returns a data frame, the coordinates method fails for extracting the number of data objects returned; corrected in cm2_mod.R.

Page 143, Chunk 57: maptools should be loaded explicitly.

Page 145-146: From version 2.3-0 of raster (released 6 September 2014), the argument small=FALSE should be added to match the over method in sp; before 2.3-0, the default was FALSE, but was changed to TRUE at this release.

## std

## sppa

Page 180, chunk 19: Changes in spatstat 1.45-0 enforce a tighter spacing of the r vector, changed in code from by = 0.005 to by = 0.001

Page 186: Changes in spatstat::bw.diggle() between versions 1.41-1 and 1.42-1 appear to have made a small change to the value returned.

## geos

Page 228: Minor changes in fitted variogram values following the change to BLAS/LAPACK from Meschach linear algebra functions in gstat 1.1-0.

Page 236: Following the change to BLAS/LAPACK from Meschach linear algebra functions in gstat 1.1-0, the poor matrix condition check example in chunk 61 should be omitted. The Choleski decomposition used from gstat 1.1-0 on indicates that the resulting covariance matrix is not positive definite, and generates missing values.

Page 241: Minor change in prediction variance values following the change to BLAS/LAPACK from Meschach linear algebra functions in gstat 1.1-0.

Pages 241-242: df constructed wrongly; "it would appear as though the estimate of the intercept is -2.47 and the estimate of beta1 is 6.95.  I wonder if these labels are switched, as a simple lm of log(zinc) on sqrt(dist) has intercept 6.99 and slope -2.54 - there is a negative relationship of log zinc with sqrt(dist)"; thanks to Susan Service. Script updated 30 August 2017.

Page 244: Minor change in prediction variance values following the change to BLAS/LAPACK from Meschach linear algebra functions in gstat 1.1-0.

Pages 244-245, section 8.5.10, chunk 85: Starting with gstat 1.1-0, the option to set cn\_max is no longer needed (nor present from 1.1-1), due to the replacement of the meschach matrix library with the Lapack/BLAS library. The new behaviour in case of non-positive definiteness is printing of the warning (Covariance matrix singular at location [x,y,z]: skipping...) and continuing with the next location. Singularity is now by default detected by the Lapack routine dpotrs, which uses Choleski decomposition and breaks on non-positive definiteness. Gstat versions prior to 1.1-0 used the LDL' decomposition instead of the Choleski decomposition. Some non-positive definite matrices can be decomposed by LDL', but not by Choleski. Compatibility with the pre-gstat 1.1-0 behaviour is obtained when the variable choleski is set to the value 0, in the same way cn\_max was defined.

Page 245: Minor change in prediction variance values following the change to BLAS/LAPACK from Meschach linear algebra functions in gstat 1.1-0.

Page 247-248, sections 8.7.1/2: changes in sample() from R >= 3.6.0 change the model training and validation sets and R2 measure; the cross-validation score summary is also changed.

Page 260: drop as geoR unavailable (May 2022).

## lat

From **spdep** 1.1-1, functions for fitting models are moved to the new package **spatialreg**, and changes are made to adapt. Present changes pass most deprecated **spdep** functions through to **spatialreg**.

Page 281-2: Monte Carlo test output affected by change in sample() for R >= 3.6.

Page 282, Chunk 45: boot should be loaded explicitly.

Page 283: Monte Carlo test output affected by change in sample() for R >= 3.6, and by changes to the calculation of the measure in February 2016.

Page 285, The sentence introducing local Moran's I is potentially misleading; the formula is given after Lloyd (2007) p. 67, eq. 4.16, originally from Anselin (1995) p. 99, eq. 12 [Anselin, L. 1995. Local indicators of spatial association, Geographical Analysis, 27, 93-115], and Getis and Ord (1996) p. 267-268, eq. 14.4 [Getis, A. and Ord, J. K. 1996 Local spatial statistics: an overview. In P. Longley and M. Batty (eds) Spatial analysis: modelling in a GIS environment (Cambridge: Geoinformation International), 261-277]. The "sum" referred to is the sum of the calculated values of local Moran's divided by the sum of the weights [for example using Szero() on the listw object].

Page 303, Chunk 81: After updating Matrix from 1.0-14 to 1.1-0, the value of the numerical Hessian standard error of lambda changes slightly.

Page 309, Chunk 90: coda should be loaded explicitly.

Page 313: Bootstrap output affected by change in sample() for R >= 3.6.

## dismap

Page 336: chunk 38: From CARBayes 3.0, the function name changes to independent.re; dismap_mod.R has been modified to work with 3.0. Further changes in CARBayes 4.0, function name to S.independent; dismap_mod.R has been modified to work with 4.0. In CARBayes 4.4, the S.independent has been withdrawn.

Page 344: chunk 63: From CARBayes 3.0, the function name changes to bymCAR.re; dismap_mod.R has been modified to work with 3.0. Further changes in CARBayes 4.0, function name to S.CARbym; dismap_mod.R has been modified to work with 4.0. From CARBayes 4.2, the acceptance thresholds (hardcoded in code in that package) were changed, leading to different results; the format of the output also changed.

Pages 344-346: chunk 67: Updating from R2BayesX 0.1-2 to 0.3-1 changes the names of components of the returned objects; dismap_mod.R has been modified to work with older and current versions of the package. 

Page 347: chunk 72 commented out to handle temporary issue between BayesX and akima; no code run for Fig. 10.20.

Pages 357-359: chunk 93: the code creating figure 10.23 failed on 2016-02-29, as this was the first leap year day encountered - dismap_mod.R has been modified to avoid the problem (next time in four years ...)

Pages 359-361: chunk 101: Updating from R2BayesX 0.1-2 to 0.3-1 changes the names of components of the returned objects; dismap_mod.R has been modified to work with older and current versions of the package. 

## after

Page 365: EFFIS 2011 forest fire dataset - 1769 fire locations in the period
from 2011-01-22 to 2011-12-16, downloaded using WFS on 2012-01-04 from
WFS:http://geohub.jrc.ec.europa.eu/effis/ows; saved as shapefile fires_120104
and interaction data with the WFS server in geohub.RData.
