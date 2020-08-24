## shadow 0.3.0 (2017-03-31)

* Initial complete release

## shadow 0.3.2 (2017-05-29)

* Fixed 'toSeg' behavior when 'x' has single attribute 
* Fixed 'shiftAz' behavior when 'object' is 'SpatialPoints*' 
* 'parallel' support in functions 'shadowHeight' and 'SVF'

## shadow 0.3.3 (2017-06-14)

* Fixed missing drawing in vignette

## shadow 0.3.5 (2017-08-17)

* Minor documentation update
* SVF also works for 3D points (above ground)

## shadow 0.4.0 (2017-11-05)

* Correction in SVF calculation
* Updated SVF tests and documentation
* Expanded SVF example based on Erell et al. (2012)

## shadow 0.4.5 (2018-01-04)

* Added 'surfaceGrid' function
* Added 'plotGrid3D' function
* Added 'inShadow' function
* Corrections in documentation

## shadow 0.5.0 (2018-03-06)

* Fixed attribute propagation in 'surfaceGrid'
* Added 'tmy' (Typical Meteorological Year) dataset
* Added 'coefDirect' function
* Fixed bug when using 'solar_pos' with >1 rows in 'inShadow'
* Added progress bar to 'inShadow'
* Added 'radiation' function
* Removed the 'message' parameter
* Added test for 'inShadow'

## shadow 0.5.3 (2018-05-16)

* Minor change in 'radiation' example
* Expanded package vignette
* Added test for 'radiation'
* Added 'radius' parameter to 'radiation' for restricted obstacles search

## shadow 0.5.5 (2018-07-19)

* 'returnList' argument in 'radiation'
* Fixed mistakes in the 'time' column of 'tmy'
* Added temperature and wind speed columns in 'tmy'
* Update vignette to comply with new 'tmy' table

## shadow 0.5.7 (2018-09-29)

* Added 'beersheva' sample dataset with Beer-Sheva buildings
* Added 'tmy2' sample dataset with Beer-Sheva TMY data
* Added 'elev' sample dataset with Beer-Sheva elevation
* Fixed 'Obstacles outline union' step in 'surfaceGrid' function to work with polygons with holes

## shadow 0.5.9 (2018-12-04)

* Added 'flowlength' function
* Added 'row.names=NULL' in 'coefDirect' to avoid row names warning
* 'surfaceGrid' now returns 'roof' only points when res/2 > h, instead of error

## shadow 0.6.0 (2019-01-07) R-Journal paper

* Added 'solarpos2' helper function
* Added 'time' parameter in functions 'shadowHeight', 'inShadow', 'shadowFootprint' and 'radiation'
* Renamed datasets ("rishon" -> "build", etc.)
* Updated vignette

## shadow 0.6.1 (2019-06-03)

* Fixed compatibility with 'shift' in new version of 'raster'

## shadow 0.6.2 (2019-09-07)

* Input to 'surfaceGrid' is checked for absence of reserved column names: 'obs_id', 'type', 'seg_id', 'xy_id', 'facade_az' and 'ZZZ'

## shadow 0.6.3 (2019-11-11)

* Replaced vignette with final version of R-Journal paper

## shadow 0.6.4 (2020-02-03)

* Replaced 'class(x) == "matrix"' to comply with R syntax

## shadow 0.6.4.1 (2020-04-01)

* Automatic update by Kurt Hornik to reduce PDF size

## shadow 0.6.7 (2020-06-13)

* Removed 'plyr' dependency
* Removed function 'flowlength' (moved to package 'geobgu')
* Fixed 'shadowFootprint' error when the polygon has duplicated points by removing zero-length segments
* Reduced vignette PDF size

## shadow 0.6.8

* Replaced sample 'sp' layers to comply with WKT2 CRS specification

## Other ideas

* Replace 'testthat' with 'tinytest'
* Switch from 'sp' to 'sf'
* Add new tests
* Set 'surfacegrid' class and define 'print' and 'plot' methods for it
* Pass obstacles height as 'units' and check for agreement with CRS units
* Return 'shadowHeight' as 'units' matrix
* Visibility algorithm (e.g. https://www.redblobgames.com/articles/visibility/)


