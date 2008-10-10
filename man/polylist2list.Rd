\name{polylist2list}
\alias{polylist2list}

\title{Extract from a `polylist' object the vertices of the polygons}

\description{
The function `polylist2list' extracts the vertices of the polygons from a `polylist' object, seperating polygons from each other by 3 rows of NaN.
}
\usage{
polylist2list(data)
}

\arguments{
  \item{data}{A `polylist' object}
}
\details{
The user can then represent the coordinates of sites of a `polylist' object
as background map, using the option `carte' included in all interactive functions of GeoXp.
}

\value{
It returns a matrix of numeric values with 2 columns ($x$ and $y$ coordinates 
of the vertices of the polygons) where polygons are seperated from each other by 3 rows of NaN.
}

\references{
Bivand R. (2008), `The spdep package', \url{http://cran.r-project.org/doc/packages/spdep.pdf}.
}

\author{Thibault Laurent}

\seealso{\code{\link{spdf2list}}}



\examples{
data(eire)
eire.contours<-polylist2list(eire.polys.utm)
}

\keyword{manip}
