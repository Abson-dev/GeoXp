\name{barnbmap}
\alias{barnbmap}
\title{Bar plot of the number of neighbour and map}
\description{
The function \code{barnbmap} draws the bar plot of the link number distribution for
a neighbourhood structure given by a \code{nb} object (\code{spdep} package) and links
this bar plot with a map.
}

\usage{
barnbmap(sp.obj, nb.obj,
criteria=NULL, carte=NULL, identify=FALSE, cex.lab=0.8, pch=16, col="lightblue3",
xlab="", ylab="", axes=FALSE, lablong="", lablat="")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp.obj}{object of class extending Spatial-class}
  \item{nb.obj}{object of class nb}
  \item{criteria}{a vector of boolean of size the number of spatial units, which permit to represent preselected sites with a cross, using the tcltk window}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{identify}{if not FALSE, identify plotted objects (currently only working for points plots). Labels for identification are the row.names of the attribute table row.names(as.data.frame(sp.obj)).}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, symbol for selected points}
  \item{col}{"lightblue3" by default, color of bars on the barplot}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
For a selected site j on the map, are represented on the map its neighbours.
For a selected bar on the graph, the corresponding sites are represented on the map with a link which
means that two sites are neighbours.}

\note{
When user select sites on the graph or on the map, he cannot add a selection by using the other graphic.}

\value{
No value returned.
}

\references{Thibault Laurent, Anne Ruiz-Gazen, Christine Thomas-Agnan (2012), GeoXp: An R Package for Exploratory Spatial Data Analysis. \emph{Journal of Statistical Software}, 47(2), 1-23. \cr \cr

Roger S.Bivand, Edzer J.Pebesma, Virgilio Gomez-Rubio (2009),  \emph{Applied Spatial Data Analysis with R}, Springer.
}

\author{Thomas-Agnan C., Ruiz-Gazen A., Laurent T.}

\keyword{spatial}

\examples{
######
# data on price indices of real estate in France
data(immob)
row.names(immob)<-immob$Nom

# immob is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
immob.sp = SpatialPoints(cbind(immob$longitude,immob$latitude))

# Spatial weight matrix based on nearest neighbours
immob.nb <- dnearneigh(coordinates(immob.sp), 0,175000)

# a simple use of barnbmap
barnbmap(immob.sp,immob.nb)

######
# Data columbus
require("maptools")
example(columbus)

# col.gal.nb is a spatial weight matrix included in spdep package...
barnbmap(columbus,col.gal.nb,criteria=(columbus$EW==1),
col=colors()[98:106], identify=TRUE, cex.lab=0.7, pch=1:9)
}

\seealso{\code{\link{moranplotmap}},\code{\link{makeneighborsw}},\code{\link{normw}},\code{\link{nonormmoran}} }

