\name{GeoXp-package}
\alias{GeoXp-package}
\alias{GeoXp}
\docType{package}
\title{
Interactive exploratory spatial data analysis
}
\description{
GeoXp is a tool for researchers in spatial statistics, spatial econometrics, 
geography, ecology etc allowing to link dynamically statistical plots with 
elementary maps. This coupling consists in the fact that the selection of a 
zone on the map results in the automatic highlighting of the corresponding points 
on the statistical graph or reversely, the selection of a portion of the graph 
results in the automatic highlighting of the corresponding points on the map. 
GeoXp includes tools from different areas of spatial statistics 
including geostatistics as well as spatial econometrics and point processes. 
Besides elementary plots like boxplots, histograms or simple scatterplos, GeoXp 
also couples with maps Moran scatterplots, variogram cloud, Lorentz Curves,...
In order to make the most of the multidimensionality of the data, GeoXp includes 
some dimension reduction techniques such as PCA.
}
\details{
\tabular{ll}{
Package: \tab GeoXp\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2008-01-14\cr
License: \tab GPL Vesion 2 or later \cr
}
GeoXp don't use classes yet. The coordinates of sites are represented by 
two vectors $long$ (for $x-axis$) and $lat$ (for $y-axis$) of size $n$ of 
numeric values. Attribute value is designed by argument $var$ of size $n$ and 
can be a vector of numeric values (function histomap, densitymap,...) or 
a factor (function barmap,...). Some functions use both a vector of numeric
values and a factor (function histobarmap, polyboxplotmap,...). Few functions as
pcamap and clustermap use a matrix of numeric variables. \cr
Contrary to spdep package (Roger Bivand), a site cannot be represented by 
lattice/area style. However, user can represent a spatial polygonal contour
as background map with option $carte$. A spatial polygonal contour is a matrix
of numeric values with 2 columns ($x$ and $y$ coordinates of the vertices of the polygons) 
where polygons are seperated from each other 
by 3 rows of NaN. We include in GeoXp 3 functions (polylist2list, spdf2list 
and map2list) which convert some spatial objects (`Polylist', `SpatialPolygonDataFrame' 
and `Map' object) into matrix as decribed above to draw a background map. \cr
Among options which are common to each function, users have the 
possibility to give a $criteria$, vector of boolean of size $n$ with TRUE on 
specific sites. These sites are then represented by a croice on the  map 
by clicking on `non intercative selection' button. \cr
Moreover, users have the possibility to make bubbles and add some graphs 
(histogram, barplot or scattermap) giving both a matrix of variables 
$listvar$ of size $n \times p$ and a list of character of size $p$
for variable names. Users can choose a proportional
symbol mapping : in function plot, we give value
$var^{0.5}$ to argument $cex$. User can choose if a legend has to appear on 
the map. He could choose then three values represented by bubbles of 
corresponding sizes. \cr
Finally, users can choose to represent sites on map with different colors
(choosed by default and not modifiable by users)
in the case of a graph using a factor (argument $color$ with 1 for different colors,
0 otherwise). Users could choose if a legend with corresponding colors
will appear on the map. Users can also modify the representation of sites on map 
(choosed by default and not modifiable by users) with argument
$symbol$ (with 1 for different symbols, 0 otherwise).  

 
}


\author{
Christine Thomas-Agnan, Yves Aragon, Anne Ruiz-Gazen, Thibault Laurent, Laurianne Robidou
Maintainer: Christine Thomas-Agnan <cthomas@cict.fr>
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\keyword{spatial}

