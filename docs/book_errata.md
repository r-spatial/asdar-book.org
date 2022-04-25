# Errata

## hello

Page 7: PROJ.4 string requires +ellps from PROJ 4.9.

## cm

Page 32: The book was built with sp version 0.9-24. From sp version 0.9-29,
rownames on the input matrix of coordinates if they are present are preserved
in SpatialPoints objects, and are displayed when the coordinates are printed.
This changes the appearance of the tabulation of the coordinates subset in
the middle of the page. Thanks to Mic Jackson for reporting the change.
<br>
Page 39: The line of code "library(maptools)" in the central code chunk is not
visible and should be. Thanks to Jerry Davis for reporting the omission.
<br>
Page 43: Between mid December 2009 and the release of sp version 0.9-61 in
March 2010 (sp releases 0.9-56, 0.9-57, 0.9-59, 0.9-60), a function
introduced in R 2.9.1 (anyDuplicates) was used without version protection.
This means that for those sp versions and R < 2.9.1, the SpatialPolygon
construction code at the foot of the page (and elsewhere) did not run. Either
return to sp version 0.9-52, or upgrade R to 2.9.1 or later. Thanks to 
Larry Winner and a participant on a course in Toulouse for reporting the
problem.
<br>
Page 45: Sometime during 2010, the book code and the downloadable code diverged as a response to changes in sp. The changes had the effect of removing the previous behaviour of automatically subsetting data.frames to match Polygons objects, and the second sentence on the page should now read: "Here, we subset to the matched rows of the data frame, to ensure that one row corresponds to each Polygons object:". Thanks to Alex Bigazzi for reporting the problem.
<br>
Pages 49-52: From sp version 0.9-94 (January 2012), the class definition of SpatialGrid objects loses "coords" and "grid.index" slots.
## vis
Page 58: Changes in class definitions and image methods led to the bottom right panel of Figure 3.1 being wrongly plotted. The code in vis\_mod.R has been updated to correct the problem reported by ZhiJie Zhang.
Page 59: Top of page - from sp version 0.9-66, Lines objects must be give a
valid ID; changed to m.sl <- SpatialLines(list(Lines(list(Line(cc)), "1"))).
Page 71: The code for Figure 3.11, first panel, as published on the website was broken in the sp package 0.9-29 update, which forced colour regions for factors to have the same number of values as the number of levels in the factor. The vis\_mod.R file has been changed and now runs with earlier and current sp package versions.
Page 74-75: From sp 1.1-0, overlay is defunct and over methods must be used. The code in vis\_mod.R has been updated to correct the problem.
## die
Page 84: URL for Grids & Datums (footnote 10) is now http://www.asprs.org/Grids-Datums.html.
Page 85: From sp 1.2-4, a bug in the the calculation of great circle distances has been corrected, and the printed distance of 124.0994 should be 124.1372.
Page 90: footnote 17: http://web1.sph.emory.edu/users/lwaller/WGindex.htm
Pages 90 and 92: The CRS in the middle of the pages has an omitted + before ellps.
Pages 97 and 105: From sp 1.1-0, overlay is defunct and over methods must be used. The code in die\_mod.R has been updated to correct the problem.
## cm2
Sections 5.4 and 5.5: The files for download from the US Census bondaries site
are now provided locally in the chapter bundle, as the URL and available files
for download have changed.
Pages 116 and 118: From sp 1.1-0, overlay is defunct and over methods must be used. The code in cm2\_mod.R has been updated to correct the problem.
## csdacm
Pages 137-140: From sp 1.1-0, overlay is defunct and over methods must be used. The code in csdacm\_mod.R has been updated to correct the problem.
## sppa
Page 161, chunk 19: Changes in spatstat 1.45-0 enforce a tighter spacing of the r vector, changed in code from by = 0.005 to by = 0.001. Pages 161, 162 and 172: The code lines in cbind() giving name DATASET should give name y; downloadable code updated. Page 169: The adapt package has been replaced by the cubature package for license reasons; changes applied to online chapter code. Page 176: From sp 1.1-0, overlay is defunct and over methods must be used. The code in sppa\_mod.R has been updated to correct the problem. Page 181: For mgcv version 1.5-2 only, Spatial\*DataFrame objects cannot be used as values for the gam() data= argument - coerce to data.frame first.
## geos
Pages 217 and 229: From sp 1.1-0, overlay is defunct and over methods must be used. The code in geos\_mod.R has been updated to correct the problem.
Page 218: df constructed wrongly; "it would appear as though the estimate of the intercept is -2.47 and the estimate of beta1 is 6.95.  I wonder if these labels are switched, as a simple lm of log(zinc) on sqrt(dist) has intercept 6.99 and slope -2.54 - there is a negative relationship of log zinc with sqrt(dist)"; thanks to Susan Service. Script updated 30 August 2017.
Pages 219-221, section 8.5.10, chunk 82: Starting with gstat 1.1-0, the option to set cn\_max is no longer needed (nor present from 1.1-1), due to the replacement of the meschach matrix library with the Lapack/BLAS library. The new behaviour in case of non-positive definiteness is printing of the warning (Covariance matrix singular at location [x,y,z]: skipping...) and continuing with the next location. Singularity is now by default detected by the Lapack routine dpotrs, which uses Choleski decomposition and breaks on non-positive definiteness. Gstat versions prior to 1.1-0 used the LDL' decomposition instead of the Choleski decomposition. Some non-positive definite matrices can be decomposed by LDL', but not by Choleski. Compatibility with the pre-gstat 1.1-0 behaviour is obtained when the variable choleski is set to the value 0, in the same way cn\_max was defined.

## lat1
Page 245: spdep now uses the deldir package rather than tripack for license reasons.
Page 265: Chunk 65: boot must now be loaded explicitly.
Page 269: The sentence introducing local Moran's I is potentially misleading; the formula is given after Lloyd (2007) p. 67, eq. 4.16, originally from Anselin (1995) p. 99, eq. 12 [Anselin, L. 1995. Local indicators of spatial association, Geographical Analysis, 27, 93-115], and Getis and Ord (1996) p. 267-268, eq. 14.4 [Getis, A. and Ord, J. K. 1996 Local spatial statistics: an overview. In P. Longley and M. Batty (eds) Spatial analysis: modelling in a GIS environment (Cambridge: Geoinformation International), 261-277]. The "sum" referred to is the sum of the calculated values of local Moran's divided by the sum of the weights [for example using Szero() on the listw object].
## lat2
Page 297, 299: For mgcv version 1.5-2 only, Spatial\*DataFrame objects cannot be used as values for the gam() data= argument - coerce to data.frame first.

Page 301: From nlme 3.1-157, offset terms are no longer accepted, so the example cannot be reproduced as it was previously, chunks 54-55.

Page 304: seed and alpha= values differ from book (no seed); alpha= was 0.4 for R < 3.6, from R >= 3.6 changes in sample() make change to alpha 0.44 necessary to extract at least two eigenvectors.

From **spdep** 1.1-1, functions for fitting models are moved to the new package **spatialreg**, and changes are made to adapt. Present changes pass most deprecated **spdep** functions through to **spatialreg**.

## dismap
## after
Page 344: The version of the splancs package used for building the book was 2.01-24, not 2.01-23 - this affects Figure 7.10 on page 177, which cannot be reproduced with the earlier version.
