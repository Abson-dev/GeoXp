\name{normw}
\alias{normw}

\title{Normalize a spatial weight matrix}
\description{
The function `normw' normalizes a spatial weight matrix}

\usage{
normw(w)
}

\arguments{
  \item{w}{A spatial weight matrix of size $n \times n$}
}
\details{
 \deqn{W_{ij}=\frac{W_{ij}}{\sum_kW_{ik}}}
}

\value{
A matrix of size $n \times n$
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas'' }

\author{Aragon Y., Thomas-Agnan C., Ruiz-Gazen A., Robidou L.}

\keyword{spatial}

\seealso{\code{\link{neighbourmap}}, \code{\link{moranplotmap}},\code{\link{makedistanceW}},\code{\link{makeneighborsw}}}

\examples{
# data auckland
data(auckland)
W<-makedistanceW(auckland$Easting,auckland$Northing,5)
W2<-normw(W)
} 