\name{angleplotmap}
\alias{angleplotmap}
\title{Detection of an eventual directional trend}
\description{
The function `angleplotmap' is used to detect an eventual directional trend associated 
to variable $var$. It represents the absolute difference between the value of $var$ at two sites 
as a function of the angle between vector $\overrightarrow{s_is_j}$ and the x-axis.
}
\usage{
angleplotmap(long, lat, var, quantiles = NULL, listvar = NULL,
listnomvar = NULL, criteria = NULL, carte = NULL, label = "",
symbol = 0, labvar = "", lablong = "", lablat = "", axis = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$ }
  \item{lat}{a vector $y$ of size $n$}
  \item{var}{a vector of numeric values of size $n$}
  \item{quantiles}{list of values of quantile orders (the regression quantile is obtained by spline smoothing)}
  \item{listvar}{matrix of variables}
  \item{listnomvar}{names of variables $listvar$}
  \item{criteria}{a vector of size n of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{label}{vector of character of size $n$ with names of sites}
  \item{symbol}{0 (by default) or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{labvar}{name of $var$}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
}
\details{
For each couple of sites $(s_i,s_j)$, the graphic represents on the y-axis the absolute difference
between $var_i$ and $var_j$ :
\deqn{D_{ij}=|var_i-var_j|} \cr and on the x-axis the angle $\theta_{ij}$ between the vector 
$\overrightarrow{s_is_j}$ and the x-axis. 
Possibility to represent a smoothing spline regression quantile $g_\alpha$. For $0<\alpha<1$,
\deqn{Pr[D_{ij}<g_\alpha(\theta{ij})]=\alpha} 
If that case, only the pair of sites $(s_i,s_j)$ verifying :
\deqn{D_{ij}>g_{max(\alpha)}(\theta{ij})} are represented.
}

\value{A matrix of boolean of size $n \times n$. TRUE if pair of sites was in the last selection}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan Christine, Aragon Yves, Ruiz-Gazen Anne, Laurent Thibault, Robidou Laurianne}

\keyword{spatial}

\examples{
# Data  afcon
data(afcon)
africa <- readOGR(system.file("vectors/Africa.MIF", package = "GeoXp")[1],
"Africa")

africa.contour<-spdf2list(africa)$poly

obs<-angleplotmap(afcon$x,afcon$y,afcon$totcon,listvar=afcon,
listnomvar=names(afcon),label=afcon$name,
criteria=(afcon$totcon>mean(afcon$totcon)),carte=africa.contour)

# Data Meuse
data(meuse.all)
data(meuse.riv)
obs<-angleplotmap(meuse.all$x,meuse.all$y,meuse.all$copper,
lablong="X",lablat="Y",quantiles=c(0.1,0.5,0.95),
listvar=meuse.all,listnomvar=names(meuse.all),
labvar="Concentration en plomb (en ppm)")
#points(meuse.riv, type = "l", asp = 1)
}


\seealso{\code{\link{variocloudmap}},\code{\link{driftmap}}}