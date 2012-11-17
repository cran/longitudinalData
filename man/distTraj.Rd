\name{distTraj}
\alias{distTraj}

\title{ ~ Function: distance for trajectories ~}

\description{
     This function computes and returns the distance computed by using the
     specified distance measure between two trajectorie.}

\usage{
distTraj(x, y, method = "euclidean", p = 2)
}

\arguments{
  \item{x}{[vector(numeric)]: first trajectories}
  \item{y}{[vector(numeric)]: second trajectories}
  \item{method}{[character]: the distance measure to be used. This must be one of "euclidean",
    "maximum", "manhattan", "canberra", "binary" or
    "minkowski".  Unambiguous substring can not be given.}
  \item{p}{[numeric]:  The power of the Minkowski distance.}
}

\details{
  This function compute the same distances than the
  \link[stats:dist]{dist} function but is optimized for trajectories. It
  can compute only a single distance at a time (whereas \code{dist} can
  return a matrix of distances) but on a single couple of trajectories,
  it is arround 5 times faster.

  Available distance measures are (written for two vectors x and y):

  \itemize{
    \item 'euclidean': Usual square distance between the two vectors (2 norm).

    \item 'maximum': Maximum distance between two components of x and y (supremum norm)

    \item 'manhattan': Absolute distance between the two vectors (1 norm).

    \item 'canberra': sum(|x_i - y_i| / (|x_i| + |y_i|)).  Terms with zero numerator and
    denominator are omitted from the sum and treated as if the values were
    missing.

    \item 'binary': (aka _asymmetric binary_): The vectors are regarded as binary bits,
    so non-zero elements are 'on' and zero elements are 'off'.  The
    distance is the _proportion_ of bits in which only one is on amongst those in
    which at least one is on.

    \item 'minkowski': The p norm, the pth root of the sum of the pth powers of the
    differences of the components.
  }

  Missing values are allowed, and are excluded from all computations involving the
  column within which they occur.  Further, when 'Inf' values are involved, all
  pairs of values are excluded when their contribution to the distance gave 'NaN'
  or 'NA'.

  If some columns are excluded in calculating a Euclidean, Manhattan, Canberra or
  Minkowski distance, the sum is scaled up proportionally to the number of columns
  used (Gower adjustement). If all pairs are excluded when calculating a particular distance, the value
  is 'NA'.
}

\value{
  A numeric value.
}



\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] Brian Everitt, Sabine Landau & Morven Leese\cr
  "Cluster Analysis"\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [3] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link[stats:dist]{dist}
}
\examples{
    x <- -1+rnorm(25);x[floor(runif(5,1,26))] <- NA
    y <- 1+rnorm(25);y[floor(runif(5,1,26))] <- NA

    plot(x,type="b",col=2,ylim=c(-5,5))
    lines(y,type="b",col=3)

    system.time(for(i in 1:10000)dist(rbind(x,y)))
    system.time(for(i in 1:10000)distTraj(x,y))

    system.time(for(i in 1:10000)dist(rbind(x,y),method="maximum"))
    system.time(for(i in 1:10000)distTraj(x,y,method="maximum"))

    system.time(for(i in 1:10000)dist(rbind(x,y),method="manhattan"))
    system.time(for(i in 1:10000)distTraj(x,y,method="manhattan"))
}

