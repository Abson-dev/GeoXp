\name{eire}
\alias{eire}
\docType{data}
\title{Eire data sets}
\description{
The `eire' data frame has 26 rows and 11 columns.
}
\usage{data(eire)}
\format{
  A data frame with 26 observations on the following 11 variables.
  \describe{
    \item{\code{V1}}{a vector x of size n}
    \item{\code{V2}}{a vector y of size n}
    \item{\code{A}}{Percentage of sample with blood group A}
    \item{\code{towns}}{Towns/unit area}
    \item{\code{pale}}{Beyond the Pale 0, within the Pale 1}
    \item{\code{size}}{number of blood type samples}
    \item{\code{ROADACC}}{arterial road network accessibility in 1961}
    \item{\code{OWNCONS}}{percentage in value terms of gross agricultural output of each county consumed by itself}
    \item{\code{POPCHG}}{1961 population as percentage of 1926}
    \item{\code{RETSALE}}{value of retail sales £000}
    \item{\code{INCOME}}{total personal income £000}
  }
}

\details{This data provides from spdep package.}
\source{
Bivand R. (2006), `The spdep package', \url{http://cran.r-project.org/doc/packages/spdep.pdf} \cr \cr 

Upton and Fingleton 1985, Bailey and Gatrell 1995, ch. 1 for
blood  group data, Cliff and Ord (1973), p. 107 for remaining
variables (also after  O'Sullivan, 1968). ``Polygon borders and
Irish data sourced from Michael Tiefelsdorf's SPSS Saddlepoint''
}

\examples{
data(eire)
}
\keyword{datasets}
