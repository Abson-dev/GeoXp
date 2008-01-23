\name{histobarmap}
\alias{histobarmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Histogram, barplot and map}

\description{
The function `histobarmap' draws a bar plot (vertical bar) of the given variable $var1$, 
a histogram of the given variable $var2$ and a map with sites of coordinates $(long,lat)$.}

\usage{
histobarmap(long, lat, var1, var2, criteria = NULL, carte = NULL,
label = "", symbol = 0, color = 1, nbcol = 10, labvar = c("", ""),
labmod = "", listvar = NULL, listnomvar = NULL, axis = FALSE,
lablong = "", lablat = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{lat}{a vector $y$ of size $n$}
  \item{var1}{a vector of factor of size $n$}
  \item{var2}{a vector of numerical values of size n}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{label}{vector of character of size $n$ with name of each site}
  \item{symbol}{0 or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{color}{0 or 1, choice of representation of selected points. If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor of $var1$}
  \item{nbcol}{number of cells for histogram (10 by default)}
  \item{labvar}{list of names for variable `var1' and `var2'}
  \item{labmod}{names of factors `var'}
  \item{listvar}{matrix of variables fo size $n \times p$ for optional additional graph and bubbles}
  \item{listnomvar}{list of character of size $p$ with names of variables in 'listvar'}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Each site is associated to a factor of $var1$ and to a value of $var2$. 
There is interactivity between the three windows created : the sites selected by a bar on the bar plot or on the histogram are represented on the map in red and the value and factor of sites selected on the map are represented in red on the bar plot and on the histogram.   
}

\value{A vector of boolean of size $n$. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y.,  Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{dblehistomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}},  \code{\link{dbledensitymap}}}



\examples{
# Data Colombus
x <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
colombus.contours<-map2list(x)
colombus<-x$att.data
obs<-histobarmap(colombus.contours$X,colombus.contours$Y,colombus$CP,
colombus$HOVAL,label=colombus$NEIGNO, carte=colombus.contours$poly,
listvar=colombus,listnomvar=names(colombus))

# Data eire
data(eire)
data(eire.polys)
eire.contours<-polylist2list(eire.polys)
histobarmap(eire$V1,eire$V2,eire$pale,eire$A,carte=eire.contours,
listvar=eire,listnomvar=names(eire),labvar="Appartenance à
la région du Pale",labmod=c("Hors Pale","Pale"))
}

\keyword{spatial}
\keyword{multivariate} 