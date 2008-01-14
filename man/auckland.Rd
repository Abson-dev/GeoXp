\name{auckland}
\alias{auckland}
\docType{data}
\title{Marshall's infant mortality in Auckland dataset}
\description{
The Auckland data frame has 167 rows (census area units - CAU) and 4 columns.
}
\usage{data(auckland)}
\format{
  A data frame with 167 observations on the following 4 variables.
  \describe{
    \item{\code{Easting}}{a numeric vector of $x$ coordinates in an unknown spatial reference system}
    \item{\code{Northing}}{a numeric vector of $y$ coordinates in an unknown spatial reference system}
    \item{\code{Deaths.1977.85}}{a numeric vector of counts of infant (under 5 years of age) deaths in Auckland 1977-1985}
    \item{\code{Under.5.1981}}{a numeric vector of population under 5 years of age at the 1981 Census}
  }
}

\details{This data provides from spdep package.}
\source{
Bailey T, Gatrell A (1995) Interactive Spatial Data Analysis, Harlow: Longman - INFOMAP data set used with permission. \cr \cr 

Bivand R. (2008), `The spdep package', \url{http://cran.r-project.org/doc/packages/spdep.pdf}. \cr \cr 

Marshall R M (1991) Mapping disease and mortality rates using Empirical Bayes Estimators, Applied Statistics, 40, 283-294; 
}
\examples{
data(auckland)
}
\keyword{datasets}
