\name{generateArtificialLongData}
\alias{gald}
\alias{generateArtificialLongData}


\title{~ Function: generateArtificialLongData (or gald) ~}

\description{
  This function builp up an artificial longitudinal data set (single
  variable-trajectory) an turn it
  into an object of class \code{\linkS4class{LongData}}.
}
\usage{
gald(nbEachClusters=50,time=0:10,varNames="V",
    functionClusters=list(function(t){0},function(t){t},function(t){10-t},function(t){-0.4*t^2+4*t}),
    constantPersonal=function(t){rnorm(1,0,2)},
    functionNoise=function(t){rnorm(1,0,2)},
    decimal=2,percentOfMissing=0)


generateArtificialLongData(nbEachClusters=50,time=0:10,varNames="V",
    functionClusters=list(function(t){0},function(t){t},function(t){10-t},function(t){-0.4*t^2+4*t}),
    constantPersonal=function(t){rnorm(1,0,2)},
    functionNoise=function(t){rnorm(1,0,2)},
    decimal=2,percentOfMissing=0)
}

\arguments{
  \item{nbEachClusters}{[numeric] or [vector(numeric)]: number of trajectories that each
    cluster must contain. If a single
    number is given, it is duplicated for all groups.}
  \item{time}{[vector(numeric)]: time at which measures are made.}
  \item{varNames}{[character]: name of the variable.}
  \item{functionClusters}{[list(function)]: lists the functions
    defining the average trajectories of each cluster.}
  \item{constantPersonal}{[function] or [list(function)]: lists the functions
    defining the personnal variation between an individual and the mean
    trajectories of its cluster. Note that these function should be
    constant function (the personal variation can not evolve with time). If
    a single function is given, it is duplicated for all groups (see detail).}
  \item{functionNoise}{[function] or [list(function)]: lists the functions
    generating the noise of each trajectory within its own cluster. If
    a single function is given, it is duplicated for all groups (see detail).}
  \item{decimal}{[numeric]: number of decimals used to round up values.}
  \item{percentOfMissing}{[numeric]: percentage (between 0 and 1)
    of missing data generated in each cluster. If a single value is
    given, it is duplicated for all groups. The missing values are
    Missing Completly At Random (MCAR).}
}

\details{
  \code{generateArtificialLongData} (\code{gald} in short) is a
  function that contruct a set of artificial longitudinal data.
  Each individual is considered as belonging to a group. This group
  follows a theoretical trajectory, function of time. These functions (one per group) are given via the argument \code{functionClusters}.

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
  An object of class \code{\linkS4class{LongData}}.
}

\seealso{
  \code{\linkS4class{LongData}}, \code{\link{longData}}, \code{\link{generateArtificialLongData3d}}
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



\examples{
par(ask=TRUE)


#####################
### Default example

(ex1 <- generateArtificialLongData())
plot(ex1)
part1 <- partition(rep(1:4,each=50))
plot(ex1,part1)


#####################
### Three diverging lines

ex2 <- generateArtificialLongData(functionClusters=list(function(t)0,function(t)-t,function(t)t))
part2 <- partition(rep(1:3,each=50))
plot(ex2,part2)


#####################
### Three diverging lines with high variance, unbalance groups and missing value

ex3 <- generateArtificialLongData(
   functionClusters=list(function(t)0,function(t)-t,function(t)t),
   nbEachClusters=c(100,30,10),
   functionNoise=function(t){rnorm(1,0,3)},
   percentOfMissing=c(0.25,0.5,0.25)
)
part3 <- partition(rep(1:3,c(100,30,10)))
plot(ex3,part3)


#####################
### Four strange functions

ex4 <- generateArtificialLongData(
    nbEachClusters=c(300,200,100,100),
    functionClusters=list(function(t){-10+2*t},function(t){-0.6*t^2+6*t-7.5},function(t){10*sin(t)},function(t){30*dnorm(t,2,1.5)}),
    functionNoise=function(t){rnorm(1,0,3)},
    time=0:10,decimal=2,percentOfMissing=0.3)
part4 <- partition(rep(1:4,c(300,200,100,100)))
plot(ex4,part4)


#####################
### To get only longData (if you want some artificial longData
###    to deal with another algorithm), use the getteur ["traj"]

ex5 <- gald(nbEachCluster=3,time=1:3)
ex5["traj"]

par(ask=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{datagen}  % Functions for generating data sets
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
