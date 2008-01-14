\name{boston}
\alias{boston}
\docType{data}
\title{Corrected Boston Housing Data}
\description{
The boston data frame has 506 rows and 20 columns. It contains the Harrison and Rubinfeld (1978)
data corrected for a few minor errors and augmented with the latitude and longitude of the 
observations. Gilley and Pace also point out that MEDEV is censored, in that median values at or over
USD 50,000 are set to USD 50,000.
}
\usage{data(boston)}
\format{
  A data frame with 506 observations on the following 22 variables.
  \describe{
    \item{\code{x}}{a vector $x$ of size $n$}
    \item{\code{y}}{a vector $y$ of size $n$}
    \item{\code{TOWN}}{a factor with levels given by town names}
    \item{\code{TOWNNO}}{a numeric vector corresponding to town}
    \item{\code{TRACT}}{a numeric vector of tract ID numbers}
    \item{\code{LON}}{a numeric vector of tract point longitudes in decimal degrees}
    \item{\code{LAT}}{a numeric vector of tract point latitudes in decimal degrees}
    \item{\code{MEDV}}{a numeric vector of median values of owner-occupied housing in USD 1000}
    \item{\code{CMEDV}}{a numeric vector corrected median values of owner-occupied housing in USD 1000}
    \item{\code{CRIM}}{a numeric vector of per capita crime}
    \item{\code{ZN}}{a numeric vector of proportions of residential land zoned for lots over 25000 sq. ft per town (constant for all Boston tracts)}
    \item{\code{INDUS}}{a numeric vector of proportions of non-retail business acres per town (constant for all Boston tracts)}
    \item{\code{CHAS}}{a factor with levels 1 if tract borders Charles River; 0 otherwise}
    \item{\code{NOX}}{a numeric vector of nitric oxides concentration (parts per 10 million) per town}
    \item{\code{RM}}{a numeric vector of average numbers of rooms per dwelling}
    \item{\code{AGE}}{a numeric vector of proportions of owner-occupied units built prior to 1940}
    \item{\code{DIS}}{a numeric vector of weighted distances to five Boston employment centres}
    \item{\code{RAD}}{a numeric vector of an index of accessibility to radial highways per town (constant for all Boston tracts)}
    \item{\code{TAX}}{a numeric vector full-value proprty-tax rate per USD 10,000 per town (constant for all Boston tracts)}
    \item{\code{PTRATIO}}{a numeric vector of pupil-teacher ratios per town (constant for all Boston tracts)}
    \item{\code{B}}{a numeric vector of $1000*(Bk - 0.63)^2$ where Bk is the proportion of blacks}
    \item{\code{LSTAT}}{a numeric vector of percentage values of lower status population}
  }
}

\details{This data provides from spdep package.}

\references{
Bivand R. (2008), `The spdep package', \url{http://cran.r-project.org/doc/packages/spdep.pdf} \cr \cr
Harrison, David, and Daniel L. Rubinfeld, ``Hedonic Housing Prices and the Demand for Clean Air'', Journal of 
Environmental Economics and Management, Volume 5, (1978), 81-102. Original data. \cr \cr

Gilley, O.W., and R. Kelley Pace, ``On the Harrison and Rubinfeld Data'', Journal of Environmental Economics 
and Management, 31 (1996), 403-405. Provided corrections and examined censoring. \cr \cr

Pace, R. Kelley, and O.W. Gilley, ``Using the Spatial Configuration of the Data to Improve Estimation'', 
Journal of the Real Estate Finance and Economics, 14 (1997), 333-340.}

\examples{
data(boston)
}
\keyword{datasets}
