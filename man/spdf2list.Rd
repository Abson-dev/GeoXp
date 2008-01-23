\name{spdf2list}
\alias{spdf2list}

\title{Extract from a `SpatialPolygonsDataFrame' object the middle coordinates of boundary box and the vertices of the polygons (in the case where polygons are given) }

\description{
The function `spdf2list' calculates
the middle coordinates of boundary box and in the case where polygons are included into a
`SpatialPolygonsDataFrame' object, it extracts the vertices of the polygons, seperating polygons from each other by 3 rows of NaN.
}

\usage{
spdf2list(data)
}

\arguments{
  \item{data}{A `SpatialPolygonsDataFrame' object}
}
\details{
The user can then represent the coordinates of sites of a `SpatialPolygonsDataFrame' object
as background map, using the option `carte' included in all interactive functions.
}

\note{
The data of a `SpatialPolygonsDataFrame' object can be directly extract using @data}


\value{
It returns two vectors of middle coordinates for x-axis and y-axis cooresponding to middle of each boundary box.  It returns a matrix of numeric values with 2 columns ($x$ and $y$ coordinates of the vertices of the polygons) where polygons are seperated from each other by 3 rows of NaN.
}

\references{
Bivand R. (2008), `The rgdal package', \url{http://cran.r-project.org/doc/packages/rgdal.pdf}.
}

\author{Thibault Laurent}

\seealso{\code{\link{polylist2list}},\code{\link{map2list}}}

\examples{
midiP <- readOGR(system.file("vectors/region.mif", package = "GeoXp")[1], 
"region")
cont_midiP<-spdf2list(midiP)$poly
}

\keyword{manip}
