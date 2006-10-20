\name{distchi2}
\alias{distchi2}

\title{It gives an euclidian distance matrix for factors using metric of $\chi^2$.}

\description{
The function `distchi2' calculates an euclidian distance matrix for factors using metric of $\chi^2$.
}

\usage{
distchi2(mat)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{A matrix of factors given as numeric values}
}

\details{
The euclidian distance between two individuals $i$ and $k$ using the metric of $\chi^2$ is given by :

\deqn{d_{\chi^2}^2=\frac{n}{p}\sum_{j}p\sum_l^{m_j}\delta_{ik}^{jl}\frac{1}{n_l^j}} 

with $n$, number of individuals, $p$ number of variables, $m_j$ is the number of possibilities for variable $Y^j$, $n_l^j$ is the number of individuals 
where $l$ has been observed for $Y^j$ and $\delta_{ik}^{jl}=1$ if there is a discordance 
between $i$ and $k$ for $l$ and 0 otherwise. It is not an interactive function.
}


\value{
It returns a $dist$ object (see help(dist)). .
}

\references{Besse Philippe, ``Data mining 2 : Modélisation
statistique et apprentissage'', Publication du LSP, \url{http://www.lsp.ups-tlse.fr/Besse/}}

\author{Laurent Thibault}

\seealso{\code{\link{clustermap}}}

\keyword{manip}

