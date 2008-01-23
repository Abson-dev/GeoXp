\name{makeneighborsw}
\alias{makeneighborsw}
\title{Spatial weight matrix based on nearest neighbors}
\description{
The function `makeneighborsw' creates a spatial weight matrix
based on a given number of nearest neighbors.  }
\usage{
makeneighborsw(xc, yc, m, cum = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{a vector $x$ of size $n$}
  \item{yc}{a vector $y$ of size $n$}
  \item{m}{ number of nearest neighbors}
  \item{cum}{if cum=TRUE, $W$ is the sum of spatial weight matrix based on $k$ nearest neighbours (for $k \leq m$; 
if FALSE $W$ is the spatial weight matrix based only on $m^{th}$ nearest neighbours}
}
\details{
For each site, we order the other sites by their distance from this
site. If cum=TRUE, for $i$, if $j$ is among the $m^{th}$
nearest sites, then :     
\deqn{W_{ij}=1} 
else
\deqn{W_{ij}=0}
If cum=FALSE, for $s_i$, if $s_j$ is the $m^{th}$
nearest site, then :     
\deqn{W_{ij}=1} 
else
\deqn{W_{ij}=0}
In case of ties, the nearest neighbour is randomly chosen.}

\value{
A spatial weight matrix of size $n \times n$}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas'' }

\author{Aragon Y., Thomas-Agnan C., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}

\seealso{\code{\link{moranplotmap}},\code{\link{findneighbors}},\code{\link{makedistancew}},\code{\link{normw}}}

\examples{
# data auckland
data(auckland)
W<-makeneighborsw(auckland$Easting,auckland$Northing,5)
} 