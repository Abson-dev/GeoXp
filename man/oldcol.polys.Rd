\name{oldcol.polys}
\alias{oldcol.polys}
\docType{data}
\title{Spatial polygonal contours of oldcol dataset}

\description{
A `polylist' object for drawing spatial polygonal contours of oldcol dataset.}

\usage{data(oldcol.polys)}

\format{
  The format is a list of polygons of class `polylist'.
}

\details{
This object is used in the `spdep' package, created by Roger Bivand. In the `GeoXp' package, we don't use 
`polylist', `SpatialPolygon' or `Map' object because a site can only be represented by a pair
of coordinates and not by a polygonnal contour. However, users have the possibility to repesent spatial 
polygonal contours as a backgroud map. That's why we create functions `sp2list',`polylist2list' and 
`map2list' for extracting of spatial object only the vertices of the polygons.  
}

\source{
Anselin, Luc (1988). ``Spatial econometrics: methods and models''. Dordrecht: Kluwer Academic, Table 12.1 p 189.}

\references{
Bivand R. (2006), ``The spdep package'', \url{http://cran.r-project.org/doc/packages/spdep.pdf}.
}

\note{
This data file prepared by Luc Anselin, Spatial Analysis Laboratory, Department of Agriculture and Consumer 
Economics, University of Illinois, Urbana-Champaign.
}

\examples{
data(oldcol.polys)
}

\keyword{datasets}


