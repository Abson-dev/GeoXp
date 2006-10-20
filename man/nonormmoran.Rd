\name{nonormmoran}
\alias{nonormmoran}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Detection of spatial autocorrelation}
\description{
The function `nonnormmoran' is used to detect spatial autocorrelation in
the residuals $u$ from the least squares model $Y=\beta \times X+u$. 
It calculates Moran's $I$ statistic of the residuals based on the gaussian asymptotic distribution 
and give a p-value associated to the test of spatial autocorrelation (gaussian version).}

\usage{
nonormmoran(y, x, W)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{vector of size $n$ of dependent variable}
  \item{}{}
  \item{x}{matrix $n \times p$ containing explanatory variables}
  \item{}{}
  \item{W}{spatial weight matrix}
}

\details{
W is supposed standartized :
\deqn{I=\frac{u'Wu}{u'u}}
\deqn{I \sim N(E(I),var(I))} 
let \eqn{M=(I-X(X'X)^{-1}X')}
\deqn{E(I)=\frac{tr(MW)}{n-k}}
\deqn{d=\frac{n-p}{n+p+2}}
\deqn{V(I)=[tr(MWMW')+tr(MW)^2+(tr(MW))^2]/d-E(I)^2}
\deqn{Z_I=\frac{I-E(I)}{v(I)^{1/2}}}}

\value{
(nobs,nvar,morani,imean,istat,ivar,prob) where `nobs' is the number of observations,
`nvar', the number of explanatory variables, `morani' is the Moran's $I$ statistic estimate, 
`imean' is $E(I)$, ivar is $var(I)$, `istat' is the normalized Moran's $I$ statistic
(corresponding to $Z_I$), and `prob' the associated p-value.}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Translated into R from Jim Lessage's  ``Spatial Econometrics Toolbox'', \url{http://www.spatial-econometrics.com/} }

\keyword{spatial}

\examples{
# data baltimore
data(baltimore)
W <- makeneighborsw(baltimore$X,baltimore$Y,4)
nonormmoran(baltimore$PRICE,baltimore[,14:15],W)
}

\seealso{\code{\link{moranplotmap}},\code{\link{neighbourmap}},\code{\link{makeneighborsw}},\code{\link{makedistanceW}},\code{\link{normw}} }
