\name{neighbourmap}
\alias{neighbourmap}
\title{Neighbour plot and map}
\description{
The function `neighbourmap' identifies spatial outliers by comparing a variable value for a particular site with these of its neighbouring sites. It draws a scatterplot of the values of the variable at neighbouring sites for a neighbourhood structure given by a binary weight matrix $W$ and links this scatterplot with a map.
}

\usage{
neighbourmap(long, lat, var, W, listvar = NULL, listnomvar = NULL,
carte = NULL, criteria = NULL, label = "", symbol = 0,
labvar = c("", ""), opt1 = 0, axis = FALSE, lablong = "", lablat = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{long}{a vector $x$ of size $n$}
  \item{lat}{a vector $y$ of size $n$}
  \item{var}{a vector of numeric values of size $n$}
  \item{W}{A spatial weigth matrix of size $n \times n$}
  \item{listvar}{matrix of variables}
  \item{listnomvar}{names of variables $listvar$}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{label}{vector of character of size $n$ with names of sites}
  \item{symbol}{0 or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{labvar}{names of variable $var$}
  \item{opt1}{0=graphic only, 1=drawing of the linear predictor for `response' in linear model}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
For a selected site $j$ on the map, are represented on the map its neighbours, and on the graph, on the x-axis the value of $var$ for this site, and in y-axis, the values of $var$ for the neighbouring sites of $j$. For a selected point on the graph, the corresponding pair of sites is represented on the map with a link.}

\note{
When user select sites on the graph or on the map, he cannot add a selection by using the other graphic.}

\value{
Returns a matrix of size $n \times n$ of boolean with TRUE if pair $(i,j)$ was in the last selection.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Aragon Y., Thomas-Agnan C., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}

\examples{
# data oldcol
data(oldcol)
data(oldcol.polys)
contours.OLD<-polylist2list(oldcol.polys)
W <- makeneighborsw(oldcol$X,oldcol$Y,4)
obs<-neighbourmap(oldcol$X,oldcol$Y,oldcol$CRIME,W,carte=contours.OLD,
criteria=(oldcol$CRIME>mean(oldcol$CRIME)),listvar=oldcol,
listnomvar=names(oldcol))
}

\seealso{\code{\link{moranplotmap}},\code{\link{makeneighborsw}},\code{\link{makedistanceW}},\code{\link{normw}},\code{\link{nonormmoran}} }

