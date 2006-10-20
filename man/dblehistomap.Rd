\name{dblehistomap}
\alias{dblehistomap}
\title{Double Interactive Histogram and map}
\description{
The function `dblehistomap' draws two histograms of the given variables $var1$ and $var2$
and a map with sites of coordinates $(long,lat)$. Each site is associated to a value of $var1$
and $var2$ and there is interactivity between the two windows created.}
\usage{
dblehistomap(long, lat, var1, var2, listvar = NULL, listnomvar = NULL, 
             nbcol1 = 10, nbcol2 = 10, carte = NULL, criteria = NULL, 
             symbol = 0, color = 1, labvar = c("", ""), label = "", 
             axis = FALSE, lablong = "", lablat = "")

}

\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{}{}
  \item{lat}{a vector $y$ of size $n$}
  \item{}{}
  \item{var1}{a vector of numeric values of size $n$}
  \item{}{}
  \item{var2}{a vector of numeric values of size $n$}
  \item{}{}
  \item{listvar}{matrix of variables}
  \item{}{}
  \item{listnomvar}{names of variables $listvar$}
  \item{}{}
  \item{nbcol1}{number of cells for histogram 1 (10 by default)}
  \item{}{}
  \item{nbcol2}{number of cells for histogram 2 (10 by default)}
  \item{}{}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{}{}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{}{}
  \item{label}{vector of character of size $n$ with names of sites}
  \item{}{}
  \item{color}{0 or 1 (by default), choice of representation of selected points (if user has selected a barplot as an additionnal graph). If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor}
  \item{}{}
  \item{symbol}{0 (by default) or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{}{}  
  \item{labvar}{name of variable $var$}
  \item{}{}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
  \item{}{}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{}{}
  \item{lablat}{name of the y-axis that will be printed on the map}
}

\details{The selection of sites by `points' or `polygons' on the map results in the
drawing of the red histograms of the subdistributions corresponding to this subset of sites.}

\value{A vector of boolean of size $n$. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{dblehistomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# data auckland
data(auckland)
data(auck.polys)
contours.auckland<-polylist2list(auck.polys)
dblehistomap(auckland$Easting,auckland$Northing,var1=auckland$Deaths.1977.85,
var2=auckland$Under.5.1981,carte=contours.auckland,
listvar=cbind(auckland,as.numeric(auckland$Deaths.1977.85>
mean(auckland$Deaths.1977.85))),listnomvar=c(names(auckland),"sup to mean"),
labvar=c("Deaths.1977.85","Under.5.1981"),
criteria=(auckland$Deaths.1977.85>mean(auckland$Deaths.1977.85)))

# data colombus
#x <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
#colombus.contours<-map2list(x)
#colombus<-x$att.data
#dblehistomap(colombus.contours$X,colombus.contours$Y,
#colombus$HOVAL,colombus$CRIME,label=colombus$NEIGNO, carte=colombus.contours$poly,
#listvar=colombus,listnomvar=names(colombus))


}
\keyword{spatial}
\keyword{multivariate} 