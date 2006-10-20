\name{baltimore}
\alias{baltimore}
\docType{data}
\title{House sales prices, Baltimore, MD 1978}
\description{
House sales price and characteristics for a spatial hedonic regression, Baltimore, MD 1978.
}
\usage{data(baltimore)}
\format{
  A data frame with 211 observations on the following 17 variables.
  \describe{
    \item{\code{STATION}}{Station}
    \item{\code{PRICE}}{Price}
    \item{\code{NROOM}}{Number of rooms}
    \item{\code{DWELL}}{Presence of well (0 or 1)}
    \item{\code{NBATH}}{Number of baths}
    \item{\code{PATIO}}{Presence of patio (0 or 1)}
    \item{\code{FIREPL}}{Presence of fire station}
    \item{\code{AC}}{AC}
    \item{\code{BMENT}}{BMENT}
    \item{\code{NSTOR}}{NSTOR}
    \item{\code{GAR}}{Number of garages}
    \item{\code{AGE}}{Age}
    \item{\code{CITCOU}}{CITCOU}
    \item{\code{LOTSZ}}{LOTSZ}
    \item{\code{SQFT}}{SQFT}
    \item{\code{X}}{a vector $x$ of size $n$}
    \item{\code{Y}}{a vector $y$ of size $n$}
  }
}
\details{This data provides from spdep package.}
\source{
Prepared by Luc Anselin. Original  data made available by Rodin Dubin, Weatherhead School of Management, Case Western Research University, Cleveland, OH.
}
\references{
Bivand R. (2006), `The spdep package', \url{http://cran.r-project.org/doc/packages/spdep.pdf} \cr \cr
Dubin, Robin A. (1992). Spatial autocorrelation and neighbourhood quality. Regional Science and Urban Economics 22(3), 433-452.}
\examples{
data(baltimore)
}
\keyword{datasets}
