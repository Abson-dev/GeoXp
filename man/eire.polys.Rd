\name{eire.polys}
\alias{eire.polys}
\docType{data}
\title{Spatial polygonal contours of eire dataset}

\description{
A `polylist' object for drawing spatial polygonal contours of eiredataset.}

\usage{data(eire.polys)}
\format{
  The format is a list of polygons of class polylist.
}
\details{
This object is used in the `spdep' package, created by Roger Bivand. In the `GeoXp' package, we don't use 
`polylist', `SpatialPolygon' or 'Map' object because a site can only be represented by a pair
of coordinates and not by a polygonnal contour. However, users have the possibility to repesent spatial 
polygonal contours as a backgroud map. That's why we create functions `sp2list', `polylist2list' and 
`map2list' for extracting from a spatial object only the vertices of the polygons.  
}
\source{
Upton and Fingleton 1985, Bailey and Gatrell 1995, ch. 1 for
blood  group data, Cliff and Ord (1973), p. 107 for remaining
variables (also after  O'Sullivan, 1968). ``Polygon borders and
Irish data sourced from Michael Tiefelsdorf's SPSS Saddlepoint''
}
\references{
Bivand R. (2008), `The spdep package', http://cran.r-project.org/doc/packages/spdep.pdf.
}
\examples{
data(eire.polys)
}
\keyword{datasets}
