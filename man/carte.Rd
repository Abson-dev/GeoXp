\name{carte}
\alias{carte}
\title{Drawing a map}
\description{
The function `carte' draws a map with sites of coordinates $(long,lat)$ and
represents sites which have been selected $(obs)$ in red if symbol=0 or with star if symbol=1. 
This function is called by most of the functions of GeoXp (this is not an interactive function).
}

\usage{
carte(long, lat, obs,criteria=NULL,buble=FALSE,cbuble=NULL,
      nointer=FALSE, carte=NULL,nocart=FALSE, label="",symbol=0,
      col=0, lablong="", lablat="", method="", W=NULL, couleurs="",
      classe, obsq,legmap=NULL,legends=list(FALSE,FALSE),labmod="",
      axis=FALSE)
}

\arguments{
  \item{long}{a vector $x$ of size $n$}
  \item{}{}
  \item{lat}{a vector $y$ of size $n$}
  \item{}{}
  \item{obs}{a boolean vector of size $n$ with TRUE on selected sites}
  \item{}{}
  \item{criteria}{a vector of size $n$ of boolean with TRUE on specific sites (these for non interactive selection)}
  \item{}{}
  \item{buble}{a boolean with TRUE for drawing bubbles, FALSE otherwise}
  \item{}{}
  \item{cbuble}{vector of size $n$ with size of each site depending on variable with which bubbles are constructed }
  \item{}{}
  \item{nointer}{a boolean with TRUE for drawing sites selected by criteria, FALSE otherwise}
  \item{}{}
  \item{carte}{matrix with 2 columns for drawing spatial polygonal contours : x and y coordinates of the vertices of the polygon}
  \item{}{}
  \item{nocart}{a boolean with TRUE for drawing spatial contours, FALSE otherwise}
  \item{}{}
  \item{label}{vector of character of size n with name of each site}
  \item{}{}
  \item{col}{0 or 1, choice of representation of selected points. If 0, sites are represented in blue, if 1, sites are represented with different colors for each factor}
  \item{}{}
  \item{symbol}{0 or 1, choice of representation of selected points. If 0, selected points are in red, if 1, selected points are stars}
  \item{}{}
  \item{lablong}{name of the x-axis that will be printed on the map}
  \item{}{}
  \item{lablat}{name of the y-axis that will be printed on the map}
  \item{}{}
  \item{method}{Specification for some graphics such as  
                `Neighbourplot1', `Neighbourplot2', `Cluster', `Angleplot', `Variocloud'}
  \item{}{}
  \item{W}{neighbourhood matrix of size $n \times n$, necessary when method=`Neighbourplot'}
  \item{}{}
  \item{couleurs}{Possibility to change colors (only for admin)}
  \item{}{}
  \item{classe}{vector of $n$ whith class, necessary when method=`Cluster'}
  \item{}{}
  \item{obsq}{vector of size $n$ with number of quadrant to which the site belongs, necessary when method=`Quadrant'.}
  \item{}{}
  \item{legmap}{a list of (numeric,numeric,numeric,numeric,numeric,numeric,character) with the sizes and the corresponding values of the 3 bubbles represented in the legend and the variable name of variable choosed }
  \item{}{}
  \item{legends}{a list of c(boolean,boolean,c(numeric,numeric),c(numeric,numeric)) with TRUE for drawing legends of bubbles (1st argument) or factors (2nd argument) and the coordinates of the location of the upper left corner of the legend box}
  \item{}{}
  \item{labmod}{Name of factors}
  \item{}{}
  \item{axis}{a boolean with True for drawing axes on the map}
}

\details{For factors, possibility to represent sites with different colors for each level.
Others options permit to represent with a croice some sites with special `criteria', with `bubble' according
to a variable given, a spatial contours with `carte', names of sites with `label' or a representation
in flower. Possibility to draw a legend giving the sizes of bubbles and colors of levels.
} 
\value{No values, only drawing of a map}

\references{Aragon Yves, Perrin Olivier, Ruiz-Gazen Anne, Thomas-Agnan Christine (2006), ``Statistique et Econométrie pour données géoréférencées : modèles et études de cas''}

\author{Thomas-Agnan C., Aragon Y., Ruiz-Gazen A., Laurent T., Robidou L.}

\keyword{spatial} 
\keyword{utilities} 