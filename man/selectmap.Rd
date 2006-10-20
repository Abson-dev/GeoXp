\name{selectmap}
\alias{selectmap}

\title{Selection of a point or polygon on a scatterplot}

\description{
The function `selectmap' is used to select a point or a polygon on the map or on a scatterplot.
Called by any function which draws a scatterplot.
}
\usage{
selectmap(var1, var2, obs, Xpoly, Ypoly, method = "")
}

\arguments{
  \item{var1}{a vector $x$ of size $n$}
  \item{}{}
  \item{var2}{a vector $y$ of size $n$ }
  \item{}{}
  \item{obs}{a boolean vector of size $n$ : TRUE if a site is already selectioned, FALSE otherwise.}
  \item{}{}
  \item{Xpoly}{X-coordinates of the vertices of selected polygon}
  \item{}{}
  \item{Ypoly}{Y-coordinates of the vertices of selected polygon}
  \item{}{}
  \item{method}{`point' if the selected area is a point; `poly' if the selected area is a polygon}
}

\details{This function is called by all the functions which draw a scatterplot such as scattermap.r,
moranplotmap.r,... This is not an interactive function.}

\value{
A vector of boolean of size $n$. TRUE if a site has been selected, FALSE otherwise.
}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y.,  Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}
\keyword{utilities} 