\name{makedistancew}
\alias{makedistancew}
\title{Spatial weight matrix based on distance threshold}
\description{
The function `makedistancew' creates a spatial weight matrix
based on a threshold distance, assigny 1 if the distance between the two sites
is lower or equal to a given threshold and 0 otherwise.}

\usage{
makedistancew(x, y, distance)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector $x$ of size $n$ }
  \item{y}{a vector $y$ of size $n$ }
  \item{distance}{threshold point}
}
\details{
If site $i$ is seperated from $j$ by a distance lower
or equal to a given threshold : 
\deqn{W_{ij}=1} 
else
\deqn{W_{ij}=0}
}

\value{
A matrix W of size $n \times n$ containing 0 and 1.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}

\seealso{\code{\link{moranplotmap}},\code{\link{makeneighborsw}},\code{\link{normw}},\code{\link{findneighbors}}}

\examples{
# data auckland
data(auckland)
W<-makedistancew(auckland$Easting,auckland$Northing,5)
} 