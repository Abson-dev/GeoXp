\name{map2list}
\alias{map2list}

\title{Extract from a `Map' object the middle coordinates of boundary box and 
the vertices of the polygons (in the case where polygons are given) }

\description{
The function `map2list' calculates
the middle coordinates of boundary box and in the case where polygons are included into the
`Map' object, it extracts the vertices of the polygons, seperating polygons from each other 
by 3 rows of NaN.
}

\usage{
map2list(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A `Map' object}
}
\details{
The user can then represent the coordinates of sites of a `Map' object
as background map, using the option `carte' included in all interactive functions.
}
\note{
The data of a `Map' object can be directly extract using \$att.data}


\value{
It returns two vectors of middle coordinates for x-axis and y-axis cooresponding to middle of 
each boundary box.  It returns a matrix of numeric values with 2 columns ($x$ and $y$ coordinates 
of the vertices of the polygons) where polygons are seperated from each other by 3 rows of NaN.
}

\references{
Bivand R. (2008), `The maptools package', \url{http://cran.r-project.org/doc/packages/maptools.pdf}.
}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{polylist2list}},\code{\link{spdf2list}}}

\examples{
#x <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
#colombus.contours<-map2list(x)
#colombus<-x$att.data
}


\keyword{manip}
