\name{moranplotmap}
\alias{moranplotmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moran scatterplot and map}
\description{
The function `moranplotmap' draws a graphic used to detect spatial 
autocorrelation in the variable $var$. On the x-absis, is represented $(var-\bar{var})$
and on the y-absis $W(var-\bar{var})$, where $W$ is the spatial weight matrix. 
It also calcultes also Moran's $I$ statistic (see nonnormoran.r) and
give a p-value associated to the autocorrelation test (gaussian version and permutation version).}

\usage{
moranplotmap(long, lat, var, W, flower = 0, opt1 = 1, 
             locmoran = 0, listvar = NULL, listnomvar = NULL, 
             carte = NULL, criteria = NULL, label = "", symbol = 0, 
             labvar = "", axis = FALSE, lablong = "", lablat = "")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{}{}
  \item{lat}{a vector $y$ of size $n$}
  \item{}{}
  \item{var}{a vector of numeric values of size $n$}
  \item{}{}
  \item{}{}
  \item{W}{A spatial weigth matrix of size $n \times n$}
  \item{}{}
  \item{flower}{link neighbouring sites (0, no link (by default) and 1, link (for points or polygon))}
  \item{}{}
  \item{opt1}{2 for drawing regression line for model $W(X-\bar{X})=\beta(X-\bar{X})+u$ else 1}
  \item{}{}
  \item{locmoran}{prints local Moran's $I$ statistic (0, no draw  (by default) and 1, draw (for points or polygon))}
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
\details{
For the permutation test, for each drawing the values of the variable $var$ are
randomly assigned to the sites. We then calculate Moran'I statistic associated to each drawing 
and we give the frequency of drawings when Moran'I statistic is lower or equal to the
observed Moran'$I$ statistic. Moreover, the function gives the opportunity to link 
neighbouring sites and give Local Moran's I statistic.
For a site $i$:
\deqn{I_i=(var_i-\bar(var))\sum_{j}W_{ij}(var_j-\bar{var})}
with $j$ not equal to $i$.}

\note{
The spatial weigth matrix has been normalized. Like that, the Moran'I statistic is equal to 
$\beta$ used in regression line for model $W(X-\bar{X})=\beta(X-\bar{X})+u$.
}

\value{
Returns $obs$, vector of size $n$ of boolean with selected sites, and Moran'I statistic $MORAN$.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas'' \cr \cr

Jim Lesage, ``Spatial Econometrics Toolbox'', \url{http://www.spatial-econometrics.com/}}

\author{Aragon Y., Thomas-Agnan C., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}

\examples{
# data baltimore
data(baltimore)
W <- makeneighborsw(baltimore$X,baltimore$Y,4)
moranplotmap(baltimore$X,baltimore$Y,baltimore$PRICE,W,label=baltimore$STATION,
criteria=(baltimore$PRICE>mean(baltimore$PRICE)),listvar=baltimore,
listnomvar=names(baltimore),axis=TRUE,lablong="X",lablat="Y",flower=1,locmoran=1,opt1=2)

}

\seealso{\code{\link{neighbourmap}},\code{\link{makeneighborsw}},\code{\link{makedistanceW}},\code{\link{normw}},\code{\link{nonormmoran}} }
