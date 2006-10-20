\name{densitymap}
\alias{densitymap}
\title{Kernel density estimates and map}
\description{
The function `densitymap' draws kernel density estimates of the variable $var$ with 'bkde.R'
and a map with sites of coordinate $(long,lat)$. Each site is associated to a value of $var$ and
there is interactivity between the two windows. 
}
\usage{
densitymap(long, lat, var,kernel='triweight',listvar=NULL, 
           listnomvar=NULL,carte=NULL, criteria=NULL,label="", 
           symbol=0,color=1, labvar="",axis=FALSE,
           lablong="", lablat="")
}

\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{}{}
  \item{lat}{a vector $y$ of size $n$}
  \item{}{}
  \item{var}{a vector of numeric values of size $n$}
  \item{}{}
  \item{kernel}{Smoothing kernel (see help(bkde) for list of options)}
  \item{}{}
  \item{listvar}{matrix of variables}
  \item{}{}
  \item{listnomvar}{names of variables $listvar$}
  \item{}{}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{}{}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{}{}
  \item{label}{vector of character of size $n$ with names of sites}
  \item{}{}
  \item{symbol}{0 or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{}{}
  \item{color}{0 or 1, choice of representation of selected points (if user has selected a barplot as an additionnal graph). If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor}
  \item{}{}
  \item{labvar}{name of variable $var$}
  \item{}{}
  \item{axis}{a boolean with True for drawing axes on the map}
  \item{}{}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{}{}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
The user can choose an interval on the density curve by mouse clicking on the graph on
the extremities of interval or by specifying directly values. The sites selected by an interval are 
then represented on the map in red. 
The selection of sites on the map by `points' or `polygon' results in the drawing of the kernel 
densities of the subdistributions corresponding to this subset of sites. 
Finally, the user can modify the bandwith parameter with a cursor in the tcltk 
window (parameter $\alpha$). $\alpha$ is the smoothing parameter for the kernel smooth : it represents the mean 
percentage of sample points involved in the local averaging (example : $\alpha=20$ means that on average, 
$n \times 0.2$ points are in any interval of length $2h$ where h is the usual bandwidth).}
}

\value{A vector of boolean of size $n$. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas'' \cr \cr

Venables, W. N. and Ripley, B. D. (2002) ``Modern Applied Statistics with S''. New York: Springer. \cr \cr

Wand M.P. et Jones M.C. (1995), ``Kernel Smoothing'', Chapman \& Hall.}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{histomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# library tcltk and KernSmooth
# data olcol
data(oldcol)
data(oldcol.polys)
contours.OLD<-polylist2list(oldcol.polys)
densitymap(oldcol$X,oldcol$Y,oldcol$CRIME,listvar=oldcol,
carte=contours.OLD, listnomvar=names(oldcol),
criteria=(oldcol$CRIME>mean(oldcol$CRIME)))


# data columbus
#x <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
#colombus.contours<-map2list(x)
#colombus<-x$att.data
#densitymap(colombus.contours$X,colombus.contours$Y,
#colombus$CRIME,label=colombus$NEIGNO, carte=colombus.contours$poly,
#listvar=colombus,listnomvar=names(colombus))

# data eire
data(eire)
data(eire.polys)
eire.contours<-polylist2list(eire.polys)
densitymap(eire$V1,eire$V2,eire$A,carte=eire.contours,
listvar=eire,listnomvar=names(eire),
labvar="Taux d'individus au groupe sanguin A")

}

\keyword{spatial}
\keyword{smooth}
\keyword{univar}
\keyword{multivariate} 
