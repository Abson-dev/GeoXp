\name{barmap}
\alias{barmap}
\title{Interactive Bar plot and map}
\description{
The function `barmap' draws a bar plot (vertical bar) of the given factor variable $var$
and a map with sites of coordinates $(long,lat)$.}
\usage{
barmap(long, lat, var, listvar = NULL, listnomvar = NULL, 
       criteria = NULL, carte = NULL, label = "", color = 1, 
       symbol = 0, labvar = "", labmod = "", axis = FALSE, 
       lablong = "", lablat = "")

}

\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{}{}
  \item{lat}{a vector $y$ of size $n$}
  \item{}{}
  \item{var}{a vector of factor of size $n$}
  \item{}{}
  \item{listvar}{matrix of variables fo size $n \times p$ for optional additional graph and bubbles}
  \item{}{}
  \item{listnomvar}{list of character of size $p$ with name of variables from $listvar$}
  \item{}{}
  \item{criteria}{a vector of size n of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{}{}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{}{}
  \item{label}{vector of character of size $n$ with name of each site}
  \item{}{}
  \item{color}{0 or 1, choice of representation of selected points. If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor}
  \item{}{}
  \item{symbol}{0 or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{}{}
  \item{labvar}{name of variable $var$}
  \item{}{}
  \item{labmod}{names of factors of $var$}
  \item{}{}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
  \item{}{}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{}{}
  \item{lablat}{name of the y-axis that will be printed on the map}
}

\details{The selection of a bar on the bar plot results in the corresponding 
sites coloured on the map with the corresponding colour observed on the bar.
Reversely, the selection of sites on the map by `points' or `polygon' 
results in the drawing of the sub-barplot in red.
}

\value{A vector of boolean of size $n$. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{histomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# data oldcol 
data(oldcol)
data(oldcol.polys)
oldcol.contours<-polylist2list(oldcol.polys)
barmap(oldcol$X, oldcol$Y, oldcol$NSA, listvar=oldcol,carte=oldcol.contours, 
listnomvar=names(oldcol),criteria=(oldcol$CP==1),axis=TRUE,lablong="X",lablat="Y")

# data eire
data(eire)
data(eire.polys)
eire.contours<-polylist2list(eire.polys)
barmap(eire$V1,eire$V2,eire$pale,carte=eire.contours,
listvar=eire,listnomvar=names(eire),labvar="Appartenance à
la région du Pale",labmod=c("Hors Pale","Pale"))

}
\keyword{spatial}
\keyword{univar}
