\name{angleplotmap}
\alias{angleplotmap}
\title{Detection of an eventual directional trend}
\description{
The function `angleplotmap' is used to detect an eventual directional trend associated 
to variable $var$. It represents the absolute difference between the value of $var$ at two sites 
as a function of the angle between vector $\overrightarrow{s_is_j}$ and the x-axis.
}
\usage{
angleplotmap(long, lat, var, quantiles=NULL,listvar=NULL, listnomvar=NULL,
criteria=NULL,carte = NULL, label = "", cex.lab=1, pch = 16,col="blue",
xlab = "angle",ylab="absolut magnitude",lablong = "", lablat = "", axes=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$ }
  \item{lat}{a vector $y$ of size $n$}
  \item{var}{a vector of numeric values of size $n$}
  \item{quantiles}{list of values of quantile orders (the regression quantile is obtained by spline smoothing)}
  \item{listvar}{matrix of variables which permit to plot bubbles on map using the tk window}
  \item{listnomvar}{names of variables $listvar$}
  \item{criteria}{a vector of size n of boolean which permit to represent sites selected with a cross, using the tcltk window}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{label}{a list of character of size $n$ with names of sites}
  \item{cex.lab}{character size of label}
  \item{pch}{16 by default, choice of representation of selected points}
  \item{col}{"blue" by default, colors of points on the angle plot}
  \item{xlab}{a title for the graphic x-axis}
  \item{ylab}{a title for the graphic y-axis}
  \item{lablong}{a title for the map x-axis}
  \item{lablat}{a title for the map y-axis}
  \item{axes}{a boolean with TRUE for drawing axes on the map}
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

\author{Thomas-Agnan Christine, Aragon Yves, Ruiz-Gazen Anne, Laurent Thibault, Robidou Lauriane}

\keyword{spatial}

\examples{
# Data  afcon
data(afcon)
africa <- readOGR(system.file("vectors/Africa.MIF", package = "GeoXp")[1],
"Africa")

africa.contour<-spdf2list(africa)$poly

obs<-angleplotmap(afcon$x,afcon$y,afcon$totcon,listvar=afcon,
listnomvar=names(afcon),label=afcon$name,cex.lab=0.5,quantiles=c(0.1,0.5,0.95),
criteria=(afcon$totcon>mean(afcon$totcon)),carte=africa.contour)

# Data Meuse
data(meuse)
data(meuse.riv)
obs<-angleplotmap(meuse$x,meuse$y,meuse$copper,
col="green",quantiles=0.9,
listvar=meuse,listnomvar=names(meuse),
xlab="Concentration en plomb (en ppm)",pch=7,carte=meuse.riv)
}


\seealso{\code{\link{variocloudmap}},\code{\link{driftmap}}}