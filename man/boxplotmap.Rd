\name{boxplotmap}
\alias{boxplotmap}
\title{Interactive boxplot and map}
\description{
The function `boxplotmap' draws a boxplot of the given variable $var$
and a map with site of coordinates $(long,lat)$. }

\usage{
boxplotmap(long, lat, var, listvar = NULL, listnomvar = NULL,
carte = NULL, criteria = NULL, label = "", symbol = 0, color = 1,
labvar = "", axis = FALSE, lablong = "", lablat = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{lat}{a vector $y$ of size $n$}
  \item{var}{a vector of numerical values of size $n$}
  \item{listvar}{matrix of variables fo size $n \times p$ for optional additional graph and bubbles}
  \item{listnomvar}{list of character of size $p$ with names of variables from $listvar$}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{label}{vector of character of size $n$ with names of each site}
  \item{symbol}{0 or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{color}{0 or 1 (by default), choice of representation of selected points (if user has selected a barplot as an additionnal graph). If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor}
  \item{labvar}{name of variable $var$}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
There is an interactivity only in one direction : the sites selected by interquartile on the boxplot 
are represented on the map in red. 
}

\value{A vector of boolean of size $n$. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{histomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# donnees oldcol
data(oldcol)
data(oldcol.polys)
contours.OLD<-polylist2list(oldcol.polys)
boxplotmap(oldcol$X, oldcol$Y,oldcol$CRIME,listvar=oldcol,
carte=contours.OLD, listnomvar=names(oldcol),criteria=(oldcol$CP==1),
axis=TRUE,lablong="X",lablat="Y") 

# donnees boston
data(boston)
boxplotmap(boston$x,boston$y,boston$CRIM,listvar=boston,
listnomvar=names(boston),criteria=(boston$CHAS==1)) 

# données eire
data(eire)
data(eire.polys)
eire.contours<-polylist2list(eire.polys)
boxplotmap(eire$V1,eire$V2,eire$POPCHG,
listvar=eire,listnomvar=names(eire),carte=eire.contours,
labvar="Taux d'évolution de la population de 1921 à
1962")
}

\keyword{spatial}
\keyword{univar} 