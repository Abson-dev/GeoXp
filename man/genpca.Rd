\name{genpca}
\alias{genpca}

\title{Generalized Principal Component Analysis (PCA)}
\description{
The function `genpca' computes a generalized Principal Component Analysis (PCA). 
It calculates the principal components, the coordinates of the variables and 
in these principals components axes and the inertia of these principal components. 
}
\usage{
genpca(data, w=rep(1/nrow(data),length=nrow(data)), m=diag(1,ncol(data),ncol(data)),
       center=TRUE, reduc=TRUE)
}

\arguments{
  \item{data}{ matrix $n \times p$ }
  \item{}{}
  \item{w}{vector of size n of weight (by default : $weight=t(1/n,...,1/n)$)}
  \item{}{}
  \item{m}{ matrix $p \times p$ (by default : metric=Identity matrix)}
  \item{}{}
  \item{center}{boolean. if TRUE, centered PCA (by default : center=TRUE)}
  \item{}{}
  \item{reduc}{boolean. if TRUE, reduced PCA (by default : reduce=TRUE)}
}

\details{
Let \deqn{W=diag(w)} 
\deqn{x=data=(x_1',...,x_n')'} with \deqn{x_i=(x_i^1,...,x_i^p)} \cr
Let \deqn{1_n=(1,...,1)' } with n rows and : \deqn{1_p=(1,...,1)'} with p rows. 
Normalization of weight : \deqn{w_i=\frac{w_i}{\sum_iw_i}} 
Vector of means : \deqn{\bar{x}=(\bar{x^1},...,\bar{x^p})' } with:  
\deqn{\bar{x^j}=\sum_iw_ix_i^j} \cr
If center=True, \deqn{x_c=x-1_n\bar{x}'} 
Standart deviation : \deqn{(\sigma^j)^2=\sum_iw_i(x_i^j)^2-(\bar{x^j})^2} 
\deqn{\Sigma=diag((\sigma^1)^2,...,(\sigma^p)^2)'} 
If reduc=True : \deqn{x_{cr}=x_c \times \Sigma^{-1/2}} 
Variance-Covariance matrix: \cr
\deqn{C=x_{cr}'Wx_{cr}} 
Cholesky decomposition : \eqn{M=LL'} where M=m \cr
Let \deqn{C_l=LCL'}
Let U and D as : \deqn{C_lU=UD} with \eqn{D=diag(\lambda_1,...,\lambda_p)} \cr
Let \deqn{V=L'U} \cr
Then : \cr
Coordinates of individuals in the principals components basis :
\deqn{CC=x_{cr}V} 
Coordinates of variables in principals components :
\deqn{VC=CVD^{-1/2}} 
Inertia :
\deqn{I=D1_p}}


\value{
Returns `inertia' vector of size $p$ with percent of inertia of each component (corresponding to $I$),
`casecoord' matrix $n \times p$ (corresponding to matrix $CC$),  
`varcoord' matrix $p \times p$ (corresponding to matrix $VC$).}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas'' \cr \cr

Caussinus H., Fekri M., Hakam S., Ruiz-Gazen A. (2003) , ``A monitoring display of Multivariate Outliers'', Computational Statistics and Data Analysis, vol. 44, 1-2, 237-252.}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}
\keyword{multivariate}
\seealso{\code{\link{clustermap}},\code{\link{pcamap}}}



