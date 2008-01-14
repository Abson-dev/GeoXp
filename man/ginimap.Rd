\name{ginimap}
\alias{ginimap}
\title{Lorentz curve and map}
\description{
The function `ginimap' computes a Lorentz curve from 'var' and calculates the Gini Index associated to $var$.
}
\usage{
ginimap(long, lat, var, listvar = NULL, listnomvar = NULL,
carte = NULL, criteria = NULL, label = "", symbol = 0, color = 1,
labvar = "", axis = FALSE, lablong = "", lablat = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{lat}{a vector $y$ of size $n$}
  \item{var}{a vector of numeric values of size $n$}
  \item{listvar}{matrix of variables}
  \item{listnomvar}{names of variables in $listvar$}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{label}{vector of character of size $n$ with names of sites}
  \item{symbol}{0 or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{color}{0 or 1 (by default), choice of representation of selected points (if user has selected a barplot as an additionnal graph). If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor}
  \item{labvar}{name of $var$}
  \item{axis}{a boolean with TRUEfor drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Users have the possibility to choose a threshold by mouse clicking on the Lorentz curve or by specifying it in the menu. The corresponding
pair $(F,G)$ and the value of `var' are then printed on the graph and the sites
with a value of `var' lower or equal to the threshold are then selected on the map.
} 

\note{The Gini Index is given in the tcltk window (see function gini.r for the formula used to calculate it).}
 
\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y.,  Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{gini}}}

\examples{
# données eire
data(eire)
data(eire.polys)
eire.contours<-polylist2list(eire.polys)
obs<-ginimap(eire$V1,eire$V2,eire$INCOME,
listvar=eire,listnomvar=names(eire),carte=eire.contours)
}


\keyword{spatial}
\keyword{univar} 