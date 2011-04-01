\name{generateArtificialLongData3d}
\alias{gald3d}
\alias{generateArtificialLongData3d}


\title{~ Function: generateArtificialLongData3d (or gald3d) ~}

\description{
  This function builp up an artificial longitudinal data set (joint
  trajectories) an turn them
  into an object of class \code{\linkS4class{LongData}}.
}
\usage{
gald3d(nbEachClusters=50,time=0:10,varNames=c("V","T"),
    functionClusters=list(function(t){c(0,0)},function(t){c(10,10)},function(t){c(10-t,10-t)}),
    constantPersonal=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    functionNoise=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    decimal=2,percentOfMissing=0)

generateArtificialLongData3d(nbEachClusters=50,time=0:10,varNames=c("V","T"),
    functionClusters=list(function(t){c(0,0)},function(t){c(10,10)},function(t){c(10-t,10-t)}),
    constantPersonal=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    functionNoise=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    decimal=2,percentOfMissing=0)
}

\arguments{
  \item{nbEachClusters}{\code{[vector(numeric)]}: number of trajectories that each
    cluster must contain. If a single
    number is given, it is duplicated for all groups.}

  \item{time}{\code{[vector(numeric)]}: time at which measures are made.}

  \item{varNames}{\code{[vector(character)]}: names of the variables.}

  \item{functionClusters}{\code{[list(function)]}: lists the functions
    that define the average trajectories of each cluster.
    Each functions shall return a vector containing one value for each variable of \code{varNames}.}

  \item{constantPersonal}{\code{[function]} or \code{[list(function)]}: lists the functions
    defining the personnal variation between an individual and the mean
    trajectories of its cluster. Note that these function should be
    constant function (the personal variation can not evolve with time). If
    a single function is given, it is duplicated for all groups (see detail).}

  \item{functionNoise}{\code{[function]} or
    \code{[list(function)]}: lists the functions
    generating the noise of each trajectory within its own cluster.
    Each functions shall return a vector containing one value for each variable of \code{varNames}.
    If a single function is given, it is duplicated for all groups.}

  \item{decimal}{\code{[numeric]}: number of decimals used to round up values.}

  \item{percentOfMissing}{\code{[numeric]}: percentage (between 0 and 1)
    of missing data generated in each cluster. If a single value is
    given, it is duplicated for all groups. The missing values are
    Missing Completly At Random (MCAR).}
}

\details{
  \code{generateArtificialLongData3d} (\code{gald3d} in short) is a
  function that contruct a set of artificial joint longitudinal data.
  Each individual is considered as belonging to a group. This group
  follows a theoretical trajectory, function of time.
  These functions (one per group) are given via the argument \code{functionClusters}.

  Even if it belong to a clusers, individual does not perfectly follow
  the mean trajectories. So a personal variation is added via the
  argument \code{constantPersonal}. This personal variation is constant
  over time.

  Then some residual noise is added to all the trajectories via the argument \code{functionNoise}.

  The number of individuals in each group is given by \code{nbEachClusters}.

  Finally, it is possible to add missing values randomly (MCAR) striking the
  data thanks to \code{percentOfMissing}.
}

\value{
  Object of class \code{\linkS4class{LongData}}.
}

\section{Author}{
  Christophe Genolini\cr
  1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
  2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\seealso{
  \code{\linkS4class{LongData3d}}, \code{\link{longData3d}}, \code{\link{generateArtificialLongData}}
}

\examples{
#####################
### Default example

ex1 <- generateArtificialLongData3d()
plot3d(ex1)
part1 <- partition(rep(1:3,each=50))
plot3d(ex1,part1)


#####################
### 4 lines with unbalanced groups

ex2 <- generateArtificialLongData3d(
  nbEachClusters=c(5,10,20,40),
  functionClusters=list(
     function(t)c(t,t^3/100),
     function(t)c(0,t),
     function(t)c(t,t),
     function(t)c(0,t^3/100)
  ),
  functionNoise = function(t){c(rnorm(1,0,1),rnorm(1,0,1))}
)
plot3d(ex2)
part2 <- partition(rep(1:4,time=c(5,10,20,40)))
plot3d(ex2,part2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{datagen}  % Functions for generating data sets
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
