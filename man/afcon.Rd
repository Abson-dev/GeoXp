\name{afcon}
\alias{afcon}
\docType{data}
\title{Spatial patterns of conflict in Africa 1966-78}
\description{
The afcon data frame has 42 rows and 5 columns, for 42 African countries, exclusing then South
West Africa and Spanish Equatorial Africa and Spanish Sahara. The dataset is used in Anselin
(1995), and downloaded from before adaptation.
}
\usage{data(afcon)}
\format{
  A data frame with 42 observations on the following 5 variables.
  \describe{
    \item{\code{x}}{an easting in decimal degrees (taken as centroïd of shapefile polygon)}
    \item{\code{y}}{a northing in decimal degrees (taken as centroïd of shapefile polygon)}
    \item{\code{totcon}}{index of total conflict 1966-78}
    \item{\code{name}}{country name}
    \item{\code{id}}{country id number as in paper}
  }
}

\details{This data provides from spdep package.}
\note{
This data file prepared by Luc Anselin, Spatial Analysis Laboratory, Department of Agriculture and Consumer 
Economics, University of Illinois, Urbana-Champaign.
}

\source{
Anselin, L. and John O'Loughlin (1992), ``Geography of international conflict and cooperation:
spatial dependence and regional context in Africa''. In The New Geopolitics, ed. M. Ward, pp.39-75. \cr \cr

Anselin, L. (1995), ``Local indicators of spatial association''. In Geographical Analysis, 27, Table 1, p.103. 
}

\examples{
data(afcon)
}
\keyword{datasets}
