\name{graphique}
\alias{graphique}
\title{Drawing a graphic}
\description{
The function `graphique' draws a specific graphic. Possibility to draw
one of these graphics : Histogram, Barplot, Boxplot, Polyboxplot, Scatterplot, Densityplot, Neighbourplot,
Lorentz, Moran, Angleplot, Variocloud, Cluster and PCA (graphics of individuals and variables).  
This function is called in most of the functions of GeoXp (this is not an interactive function). 
}
\usage{
graphique(var1, var2, var3, obs,num, 
          graph="",couleurs="", symbol=0,col=0, labvar="", 
          nbcol=10, alpha1, W, Xpoly, Ypoly, F, G, opt1=1,
          opt2=1, quantiles=0.5, labmod="", direct,inertie,
          label=0,kernel, obsq, locmoran=0,bin=NULL)
}


\arguments{
  \item{var1}{1st variable of size $n$}
  \item{}{}
  \item{var2}{2nd variable of size $n$}
  \item{}{}
  \item{var3}{3rd variable of size $n$ (used for variocloudmap)}
  \item{}{}
  \item{obs}{a vector of boolean of size $n$ with sites selected}
  \item{}{}
  \item{num}{number of windows which must be activated (3 ou 4)}
  \item{}{}
  \item{graph}{name of graphic which must be drawn : Histogram, Barplot, Boxplot, Polyboxplot, Scatterplot, Densityplot1, Densityplot2, Neighbourplot, Lorentz, Moran, Quadrant, Angleplot, Variocloud, Cluster, Acp1, Acp2}
  \item{}{}
  \item{couleurs}{Possibilty to change colors (for admin only)}
  \item{}{}
  \item{symbol}{0 or 1, choice of representation of points selected  (0, selected points are in red or 1, selectd points are stars)}
  \item{}{}
  \item{col}{0 or 1, choice of representation of points selected (0, sites are represented in blue and 1, sites are represented with different colors for each factor)}
  \item{}{}
  \item{labvar}{name(s) of variable(s) studied}
  \item{}{}
  \item{nbcol}{number of cells if the graphic choosen is the histogram (10 by default)  }
  \item{}{}
  \item{alpha1}{regression smoothing paramater}
  \item{}{}
  \item{W}{Spatial weight matrix}
  \item{}{}
  \item{Xpoly}{x-coordinates of the vertices of selected polygon}
  \item{}{}
  \item{Ypoly}{y-coordinates of the vertices of selected polygon}
  \item{}{}
  \item{F}{Used for Ginimap}
  \item{}{}
  \item{G}{Used for Ginimap}
  \item{}{}
  \item{opt1}{Option for scattermap (1 graphic only, 2 drawing of the conditionnal median regression spline, 3 drawing of the conditionnal quantile regression spline (user has to give option quantiles))}
  \item{}{}
  \item{opt2}{Option for drawing robust empirical variogram}
  \item{}{}
  \item{quantiles}{vector which contains the values of $\alpha$ for conditionnal quantile}
  \item{}{}
  \item{labmod}{names of factor if the graphic choosed is a barplot}
  \item{}{}
  \item{direct}{Used for PCA}
  \item{}{}
  \item{inertie}{Used for PCA}
  \item{}{}
  \item{label}{Name of sites}
  \item{}{}
  \item{kernel}{Name of the kernel choosed in densitymap}
  \item{}{}
  \item{obsq}{Used for clustermap and barmap}
  \item{}{}
  \item{locmoran}{Print local moran for each site}
  \item{}{}
  \item{bin}{The bins chosen to calculte empirical variogram}
}


\details{This function is called by any function which draws a graphic. A lot of options
are considered because of the large number of graphics proposed
} 
\value{No values, only drawing of a graphic}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y.,  Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial}
\keyword{utilities}



