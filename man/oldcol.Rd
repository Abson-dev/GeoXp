\name{oldcol}
\alias{oldcol}
\docType{data}
\title{Columbus OH spatial analysis data set}
\description{
The columbus data frame has 49 observations and 22 columns. Unit of 
analysis: 49 neighbourhoods in Columbus, OH, 1980 data.
}
\usage{data(oldcol)}
\format{
  A data frame with 49 observations on the following 22 variables.
  \describe{
    \item{\code{PERIMETER}}{Computed by ArcView}
    \item{\code{COLUMBUS.}}{internal polygon ID}
    \item{\code{COLUMBUS.I}}{another internal polygon ID}
    \item{\code{POLYID}}{yet another internal polygon ID}
    \item{\code{NEIG}}{neighbourhood id value (1-49); conforms to id value used in Spatial Econometrics book}
    \item{\code{HOVAL}}{housing value (in 1,000 dollars)}
    \item{\code{INC}}{household income (in 1,000 dollars)}
    \item{\code{CRIME}}{residential burglaries and vehicle thefts per thousand households in the neighborhood}
    \item{\code{OPEN}}{open space in the neighborhood}
    \item{\code{PLUMB}}{percentage housing units without plumbing}
    \item{\code{DISCBD}}{distance to CBD}
    \item{\code{X}}{x coordinate}
    \item{\code{Y}}{y coordinate}
    \item{\code{AREA}}{neighborhood area}
    \item{\code{NSA}}{north south dummy (North=1)}
    \item{\code{NSB}}{north south dummy (North=1)}
    \item{\code{EW}}{east west dummy (East=1)}
    \item{\code{CP}}{core-periphery dummy (Core=1)}
    \item{\code{THOUS}}{constant=1,000}
    \item{\code{NEIGNO}}{Neig+1,000, alternative neighborhood id value}
    \item{\code{PERIM}}{polygon perimeter}
  }
}

\note{
This data file prepared by Luc Anselin, Spatial Analysis Laboratory, Department of Agriculture and Consumer 
Economics, University of Illinois, Urbana-Champaign.
}
\source{
Anselin, Luc (1988). ``Spatial econometrics: methods and models''. Dordrecht: Kluwer Academic, Table 12.1 p 189. \cr \cr
Bivand R. (2006), `The spdep package', \url{http://cran.r-project.org/doc/packages/spdep.pdf} }
\examples{
data(oldcol)
}
\keyword{datasets}
