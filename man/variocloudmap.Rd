\name{variocloudmap}
\alias{variocloudmap}
\title{Interactive variocloud and map}

\description{
The function `variocloudmap' draws a semi-variocloud (directional or omnidirectional) and a map. 
It is used to detect spatial autocorrelation. Possibility to draw the empirical semi-variogram
and a robust empirical semi-variogram.}

\usage{
variocloudmap(long, lat, var, bin = NULL, quantiles = NULL, 
listvar = NULL, listnomvar = NULL, criteria = NULL, carte = NULL,
label = "", symbol = 0,labvar = "", axis = FALSE, 
lablong = "", lablat = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{lat}{a vector $y$ of size $n$}
  \item{var}{a vector of numeric values of size $n$}
  \item{bin}{list of values where empirical variogram is evaluated}
  \item{quantiles}{list of values of quantile orders (the regression quantile is obtained by spline smoothing)}
  \item{listvar}{matrix of variables}
  \item{listnomvar}{names of variables $listvar$}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{label}{vector of character of size $n$ with names of sites}
  \item{symbol}{0 (by default) or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{labvar}{name of $var$}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
For some couple of sites $(s_i,s_j)$, the graph represents on the $y-axis$ the semi squared difference
between $var_i$ and $var_j$ : 
\deqn{\gamma_{ij}=\frac{1}{2}(var_i-var_j)^2} 
and on the $x-absis$ the distance $h_{ij}$ between $s_i$ and $s_j$.
The semi Empirical variogram has been calculated as :
\deqn{\gamma(h)=\frac{1}{2|N(h)|}\sum_{N(h)}(Z(s_i)-Z(s_j))^2}
where
\deqn{N(h)=\{(s_i,s_j):s_i-s_j=h;i,j=1,...,n\}}
and the robust version :
\deqn{\gamma(h)=\frac{1}{2(0.457+\frac{0.494}{|N(h)|})}(\frac{1}{|N(h)|}\sum_{N(h)}|Z(s_i)-Z(s_j)|^{1/2})^4}
The number $N$ of points to evaluate the empirical variogram and the distance $\epsilon$ between 
points are set as follows :
\deqn{N=\frac{1}{max(30/n^2,0.08,d/D)}}
and :
\deqn{\epsilon=\frac{D}{N}}
with : \deqn{D=max(h_{ij})-min(h_{ij})} 
and : \deqn{d=max(h_{ij}^{(l)}-h_{ij}^{(l+1)}),} 
where $h^{(l)}$ is the vector of sorted distances.
In options, possibility to represent a regression quantile smoothing spline $g_{\alpha}$ (in that
case the points below this quantile curve are not drawn). }

\value{
A matrix of boolean of size $n \times n$. TRUE if the couple of site was in the last selection of points.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas'' \cr \cr

Cressie N. and Hawkins D. (1980), ``Robust estimation of the variogram'', in Journal of the international association for mathematical geology, 13, 115-125.}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}
\examples{
# data meuse
data(meuse.all)
data(meuse.riv)
obs<-variocloudmap(meuse.all$x,meuse.all$y,
meuse.all$zinc,quantiles=0.95,listvar=meuse.all,
listnomvar=names(meuse.all))
#points(meuse.riv, type = "l", asp = 1)
}


\seealso{\code{\link{angleplotmap}}, \code{\link{driftmap}}}