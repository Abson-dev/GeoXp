\name{scattermap}
\alias{scattermap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interactive scatterplot and map}

\description{
The function `scattermap' draws a scatterplot of the given variables $(var1,var2)$ 
and a map with sites of coordinates $(long,lat)$. Boxplots of each variable $var1$ and $var2$ are represented
below the x-axis and y-axis.}

\usage{
scattermap(long, lat, var1, var2, listvar = NULL, listnomvar = NULL, 
           opt = 1, quantiles = 0.5, criteria = NULL, carte = NULL, 
           label = "", symbol = 0, labvar = c("", ""), color = 1, 
           axis = FALSE, lablong = "", lablat = "")
           }


%- maybe also 'usage' for other objects documented here.
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
  \item{opt}{1=graphic only, 2=drawing of the linear predictor for `response' in linear model, 3=drawing of the conditionnal quantile regression spline (user has to give option quantiles)}
  \item{}{}
  \item{quantiles}{vector which contains a list of order (if opt = 3)}
  \item{}{}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{}{}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{}{}
  \item{label}{vector of character of size $n$ with names of sites}
  \item{}{}
  \item{color}{0 or 1, choice of representation of selected points. If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor}
  \item{}{}
  \item{symbol}{0 or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{}{}
  \item{labvar}{names of variables $var1$ and $var2$}
  \item{}{}
  \item{axis}{a boolean with True for drawing axes on the map}
  \item{}{}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{}{}
  \item{lablat}{name of the y-axis that will be printed on the map}
}

\details{Each site is associated to a value of `var1' and a value of `var2'. 
There is an interactivity between the two windows: the sites selected by `point' or `polygon' on the scatterplot
are represented on the map in red; sites selected on the map are then
represented in red on the scatterplot. Users have the possibility to draw linear predictor for `response' in linear model
or conditionnal quantile regression spline (option $opt$ and $quantiles$).}

\value{A vector of boolean of size $n$. TRUE if the site was in the last selection.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\seealso{\code{\link{dblehistomap}}, \code{\link{histobarmap}}, \code{\link{scattermap}}, \code{\link{densitymap}}}

\examples{
# library tcltk and fields
# data baltimore
data(baltimore)
scattermap(baltimore$X,baltimore$Y,baltimore$PRICE,baltimore$SQFT,
listvar=baltimore, listnomvar=names(baltimore),labvar=c("SQFT","PRICE"),
criteria=(baltimore$CITCOU==1),axis=TRUE,lablong="X",lablat="Y",opt=2)

# Data Colombus
#x <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
#colombus.contours<-map2list(x)
#colombus<-x$att.data
#scattermap(colombus.contours$X,colombus.contours$Y,
#colombus$HOVAL,colombus$CRIME,label=colombus$NEIGNO, carte=colombus.contours$poly,
#listvar=colombus,listnomvar=names(colombus))

# Data Eire
data(eire)
data(eire.polys)
eire.contours<-polylist2list(eire.polys)
scattermap(eire$V1,eire$V2,eire$ROADACC,eire$OWNCONS,
listvar=eire,listnomvar=names(eire),carte=eire.contours,
labvar=c("Routes","Taux de consomation"),opt=2,quantiles=0.95)
}

\keyword{regression}
\keyword{spatial}
\keyword{multivariate} 