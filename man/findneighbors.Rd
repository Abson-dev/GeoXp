\name{findneighbors}
\alias{findneighbors}
\title{Research of the $m$ nearest neighbors for sites.}
\description{
The function `findneighbors' gives for each site of coordinates
$(x_c,y_c)$ the $m$ nearest neighbors in the set of sites.}

\usage{
findneighbors(xc, yc, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{a numerical vector $x$ of size $n$ }
  \item{yc}{a numerical vector $y$ of size $n$ }
  \item{m}{number of nearest neighbors}
}
\details{
In case of ties, the nearest neighbor is randomly chosen among the ties. 
This function is called in makeneighborsw.r.}

\value{
A matrix of size $n \times n$ containing 1 on position $(i,j)$ if
$j$ is m-nearest neighbour to $i$.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}
\keyword{utilities}
\seealso{\code{\link{moranplotmap}},\code{\link{makedistanceW}},\code{\link{makeneighborsw}}}

\examples{
data(auckland)
W<-findneighbors(auckland$Easting,auckland$Northing,1)
}

