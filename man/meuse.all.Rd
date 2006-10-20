\name{meuse.all}
\alias{meuse.all}
\docType{data}
\title{Meuse river data set - original, full data set}

\description{
This data set gives locations and top soil heavy metal concentrations (ppm), 
along with a number of soil and landscape variables, collected in a 
flood plain of the river Meuse, near the village Stein. 
Heavy metal concentrations are bulk sampled from an area of approximately $15 \times 15 m$.
}

\usage{data(meuse.all)}
\format{
  A data frame with 164 observations on the following 17 variables.
  \describe{
    \item{\code{sample}}{sample number}
    \item{\code{x}}{a numeric vector; x-coordinate (m) in RDM (Dutch topographical map coordinates)}
    \item{\code{y}}{a numeric vector; y-coordinate (m) in RDM (Dutch topographical map coordinates)}
    \item{\code{cadmium}}{topsoil cadmium concentration, ppm.; note that zero cadmium values in the original data set have been shifted to 0.2 (half the lowest  non-zero value)}
    \item{\code{copper}}{topsoil copper concentration, ppm. }
    \item{\code{lead}}{lead topsoil lead concentration, ppm.}
    \item{\code{zinc}}{topsoil zinc concentration, ppm.}
    \item{\code{elev}}{elev relative elevation}
    \item{\code{dist.m}}{distance to river Meuse (metres), as obtained during the field survey}
    \item{\code{om}}{organic matter, as percentage}
    \item{\code{ffreq}}{flooding frequency class}
    \item{\code{soil}}{soil type}
    \item{\code{lime}}{lime class}
    \item{\code{landuse}}{landuse class}
    \item{\code{in.pit}}{logical; indicates whether this is a sample taken in a pit}
    \item{\code{in.meuse155}}{logical; indicates whether the sample is part of the `meuse' (i.e., filtered) data set; in addition to the samples in a pit, an sample (139) with outlying zinc content was removed }
    \item{\code{in.BMcD}}{logical; indicates whether the sample is used as part of the subset of 98 points in the various interpolation examples of Burrough & McDonnell}
  }
}

\details{
This data set provides from gstat package.
}

\source{
The actual field data were collected by Ruud van Rijn and Mathieu Rikken; data compiled for R by Edzer J. Pebesma
}

\references{
P.A. Burrough, R.A. McDonnell, 1998. Principles of Geographical Information Systems. Oxford University Press, \url{http:/www.gstat.org/}
}

\examples{
data(meuse.all)
}

\keyword{datasets}
