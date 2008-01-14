\name{histomap}
\alias{histomap}

\title{Interactive Histogram and map}
\description{
The function `histomap' draws a histogram of the given variable $var$
and a map with sites of coordinates $(long,lat)$. Each site is associated to a value of $var$ and there is interactivity between the two windows.
}
\usage{
histomap(long, lat, var, nbcol = 10, listvar = NULL, listnomvar = NULL,
criteria = NULL, carte = NULL, label = "", symbol = 0, color = 1,
labvar = "", axis = FALSE, lablong = "", lablat = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{lat}{a vector $y$ of size $n$}
  \item{var}{a vector of numeric values of size $n$}
  \item{nbcol}{number of cells for histogram (10 by default)}
  \item{listvar}{matrix of variables}
  \item{listnomvar}{a list with names of variables $listvar$}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{label}{vector of character of size $n$ with names of sites}
  \item{symbol}{0 (by default) or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{color}{0 or 1 (by default), choice of representation of selected points (if user has selected a barplot as an additionnal graph). If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor}
  \item{labvar}{name of variable $var$}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Sites selected by a bar on the histogram are represented on the map in red and the values of sites selected on the map by `points' or `polygon' are 
represented in red as a sub-histogram on the histogram. 
}
\value{
A vector of boolean of size $n$. TRUE if the site was in the last selection.
}
\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y.,  Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{histomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}
\examples{
# data oldcol
data(oldcol)
data(oldcol.polys)
contours.OLD<-polylist2list(oldcol.polys)
histomap(oldcol$X,oldcol$Y,oldcol$CRIME,listvar=oldcol, 
listnomvar=names(oldcol),criteria=(oldcol$CRIME>mean(oldcol$CRIME)),
carte=contours.OLD)

# data afcon
data(afcon)
histomap(afcon$x,afcon$y,afcon$totcon,listvar=afcon,
listnomvar=names(afcon),criteria=(afcon$totcon>mean(afcon$totcon)),
label=afcon$name)

# data eire
data(eire)
data(eire.polys)
eire.contours<-polylist2list(eire.polys)
histomap(eire$V1,eire$V2,eire$A,carte=eire.contours,listvar=eire,
listnomvar=names(eire),labvar="Taux d'individus au groupe sanguin A")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
\keyword{univar}
