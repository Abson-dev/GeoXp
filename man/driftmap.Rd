\name{driftmap}
\alias{driftmap}
\title{Detection of an eventual trend}
\description{
The function `driftmap' creates a grid on the map and calculates the mean and
median for each cell. The right plot (resp. left plot) represents  
means and medians calculated by row (resp. column).
}
\usage{
driftmap(long, lat, var, interpol = 1,nuage = 0, carte = NULL, 
         lablong = "",lablat = "", labvar = "")
}

\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{}{}
  \item{lat}{a vector $y$ of size $n$}
  \item{}{}
  \item{var}{a vector of numeric values of size $n$}
  \item{}{}
  \item{interpol}{if 1, the mean and median calculated are linearly interpoled}
  \item{}{}
  \item{nuage}{if 1, the individuals are also represented on the graphics}
  \item{}{}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{}{}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{}{}
  \item{lablat}{name of the y-axis that will be printed on the map}
  \item{}{}
  \item{labvar}{name of variable `var'}

}
\details{
Possibility to change the number of cells in the grid with the tcltk window, to interpolate the means and medians calculated (by default), 
to work on a rotated map. Notice that this is not an interactive function.}

\value{
No values returned, only drawing of a graphic.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}

\seealso{\code{\link{angleplotmap}},\code{\link{variocloudmap}},{\link{rotation}}}

\examples{
# data auckland
data(auckland)
data(auck.polys)
contours.auckland<-polylist2list(auck.polys)
driftmap(auckland$Easting,auckland$Northing,auckland$Deaths.1977.85,
interpol=0,nuage=1,lablong="X",lablat="Y",labvar="Deaths.1977.85")

# data afcon
data(afcon)
driftmap(afcon$x,afcon$y,afcon$totcon,interpol=0,
nuage=1,lablong="X",lablat="Y",labvar="Number of conflicts")

# data meuse
data(meuse.all)
data(meuse.riv)
driftmap(meuse.all$x,meuse.all$y,meuse.all$copper,
nuage=1,lablong="X",lablat="Y",
labvar="Concentration de cuivre (en ppm)")
}



