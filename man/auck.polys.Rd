\name{auck.polys}
\alias{auck.polys}
\docType{data}
\title{Spatial polygonal contours of auckland dataset}

\description{
A `polylist' object for drawing spatial polygonal contours of auckland dataset.}

\usage{data(auck.polys)}

\format{
  The format is a list of polygons of class polylist.
}

\details{
This object is used in the `spdep' package, created by Roger Bivand. In the `GeoXp' package, we don't use 
`polylist', `SpatialPolygonDataFrame' or 'Map' object because a site can only be represented by a pair
of coordinates and not by a polygonnal contour. However, users have the possibility to repesent spatial 
polygonal contours as a backgroud map. That's why we created functions `spdf2list', `polylist2list' and 
`map2list' for extracting from a spatial object only the vertices of the polygons.  
}

\source{
Marshall R M (1991) Mapping disease and mortality rates using Empirical Bayes Estimators, Applied Statistics, 40, 283-294. \cr \cr
Bailey T, Gatrell A (1995) Interactive Spatial Data Analysis, Harlow: Longman - INFOMAP data set used with permission. 
}
\references{
Bivand R. (2006), `The spdep package', \url{http://cran.r-project.org/doc/packages/spdep.pdf}.
}
\examples{
data(auck.polys)
contours.auckland<-polylist2list(auck.polys)
}

\keyword{datasets}
