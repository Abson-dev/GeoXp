\name{pcamap}
\alias{pcamap}
\title{Generalized Principal Component Analysis and map}
\description{
The function `pcamap' draws the plots summarizing a generalized Principal Component Analysis (PCA), made with `genpca.r'. It draws the scatterplot of the individuals projected on a chosen principal component plane (with their percentage of inertia), together with the scatterplot of the variables projected into the same plane with the quality of representation in order to interpret the principal component axes. The individuals scatterplot interacts with the map.
}
\usage{
pcamap(long, lat, dataset, direct = c(1, 2), weight = rep(1/nrow(dataset),
length = nrow(dataset)), metric = diag(1, ncol(dataset), ncol(dataset)),
center = TRUE, reduce = TRUE, namedata = NULL, qualproj = 0, listvar = NULL,
listnomvar = NULL, criteria = NULL, carte = NULL, label = "", symbol = 0,
color = 1, axis = FALSE, lablong = "", lablat = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{lat}{a vector $y$ of size $n$}
  \item{dataset}{ matrix $n \times p$ of variables}
  \item{direct}{Number of component analysis to draw}
  \item{weight}{vector of size $n$ of weight (by default : $weight=t(1/n,...,1/n)$)}
  \item{metric}{ matrix $p \times p$ (by default : metric=Identity matrix)}
  \item{center}{boolean. if TRUE, centred PCA (by default : center=TRUE)}
  \item{reduce}{boolean. if TRUE, reduced PCA (by default : reduce=TRUE)}
  \item{namedata}{List of Names of dataset}
  \item{qualproj}{possibility to print the quality of representation of individuals (0 no print (by default) and 1 print)}
  \item{listvar}{matrix of variables}
  \item{listnomvar}{names of variables from $listvar$}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : $x$ and $y$ coordinates of the vertices of the polygon}
  \item{label}{vector of character of size $n$ with name of each site}
  \item{symbol}{0 (by default) or 1, choice of representation of selected points. If 0, selected points are circles, if 1, selected points are stars}
  \item{color}{0 or 1 (by default), choice of representation of selected points (if user has selected a barplot as an additionnal graph). If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor}
  \item{axis}{a boolean with TRUE for drawing axes on the map}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{lablat}{name of the y-axis that will be printed on the map}
}
\details{
Let \deqn{D=diag(\lambda_1,...,\lambda_p)}
\deqn{1_p=(1,...,1)'} 
Let the coordinates of individuals in the principals components \deqn{CC=(C_1',...,C_n')'} 
with \eqn{C_i=(C_i^1,...,C_i^p)} \cr
Let the coordinates of variables in the principals components \deqn{CC=(V_1',...,V_p')'} 
with \eqn{V_i=(V_i^1,...,V_i^p)} \cr
Part of inertia : \deqn{(\frac{\lambda_1}{\sum_i\lambda_i},...,\frac{\lambda_p}{\sum_i\lambda_i})'} 
Quality of representation of individual k projected on plane (i,j):
\deqn{Qu=\sqrt{\frac{(C_k^i)^2+(C_k^j)^2}{\sum_l(C_k^l)^2}}}
Quality of representation of variable k projected on plane (i,j):
\deqn{VQu=\sqrt{\frac{(V_k^i)^2+(V_k^j)^2}{\sum_l(V_k^l)^2}}}}

\value{
Return `obs', vector of size $n$ of boolean with sites selected, `inertia' vector of size $p$ with percent of inertia of each component, `casecoord' matrix $n \times p$ of indivuduals, `varcoord' matrix $p \times p$ of principal components.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2008), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas'' \cr \cr

Caussinus H., Fekri M., Hakam S., Ruiz-Gazen A. (2003) , ``A monitoring display of Multivariate Outliers'', Computational Statistics and Data Analysis, vol. 44, 1-2, 237-252.}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\examples{
# Data Colombus
x <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
colombus.contours<-map2list(x)
colombus<-x$att.data
obs<-pcamap(colombus.contours$X,colombus.contours$Y,colombus[,6:12],
label=colombus$NEIGNO, carte=colombus.contours$poly,listvar=colombus,
listnomvar=names(colombus),namedata=names(colombus[,6:12]))


# data boston
data(boston)
obs<-pcamap(boston$x,boston$y,boston[,15:22],label=boston$TOWN, 
listvar=boston,listnomvar=names(boston),namedata=names(boston[,15:22]))

}

\keyword{spatial}
\keyword{multivariate}
\seealso{\code{\link{clustermap}},\code{\link{genpca}}}
