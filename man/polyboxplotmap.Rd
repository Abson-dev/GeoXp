\name{polyboxplotmap}
\alias{polyboxplotmap}
\title{Interactive polyboxplot and map}
\description{
Parallel Boxplots of a numerical variable by levels of a factor. 
It interacts with a map.
}
\usage{
polyboxplotmap(long, lat, var2, var1, listvar = NULL,
listnomvar = NULL, carte = NULL, criteria = NULL, label = "",
labmod = "", labvar = "", symbol = 0, color = 1, axis = FALSE,
lablong = "", lablat = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{lat}{a vector $y$ of size $n$}
  \item{var2}{a vector of factor of size $n$}
  \item{var1}{a vector of numerical values of size $n$}
  \item{listvar}{matrix of variables fo size $n \times p$ for optional additional graph and bubbles}
  \item{listnomvar}{list of character of size $p$ with names of variables inluded in $listvar$}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{label}{vector of character of size n with name of each site}
  \item{labmod}{names of factors $var2$}
  \item{labvar}{list of names for variables $var2$ and $var1$}
  \item{symbol}{0 or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{color}{0 or 1, choice of representation of selected points. If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor of $var2$}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Each site is associated to a value of $var1$ and a level of $var2$. 
There is an interactivity only in one direction : the sites selected by quartile-intervals on one of the boxplots 
are then represented on the map in red (or colors according to the options). 
}

\value{A vector of boolean of size $n$. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{dblehistomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{dbledensitymap}}}


\examples{
# Data eire
data(eire)
data(eire.polys)
eire.contours<-polylist2list(eire.polys)
polyboxplotmap(eire$V1,eire$V2,eire$pale,eire$POPCHG,
listvar=eire,listnomvar=names(eire),carte=eire.contours,
labvar="Appartenance ou non à la région du Pale",
labmod=c("Hors Pale","Pale"))
}
\keyword{spatial}
\keyword{multivariate} 