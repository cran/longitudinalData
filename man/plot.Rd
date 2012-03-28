\name{plot,LongData}
\Rdversion{1.1}
\alias{plot}
\alias{plot,LongData}
\alias{plot,LongData-method}
\alias{plot,LongData,missing-method}
\alias{plot,LongData,Partition-method}
\alias{plot,LongData3d}
\alias{plot,LongData3d-method}
\alias{plot,LongData3d,missing-method}
\alias{plot,LongData3d,Partition-method}

\title{ ~ function: plot for LongData or LongData3d~ }
\description{
  \code{plot} the \code{\linkS4class{LongData}} or \code{\linkS4class{LongData3d}} optionnaly relatively
  to a \code{\linkS4class{Partition}}. For joint trajectories, one
  graphe for each variable trajectory is displayed.
}

\usage{
\S4method{plot}{LongData,Partition}(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar'],addLegend=TRUE),nbSample=1000)
\S4method{plot}{LongData3d,Partition}(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar'],addLegend=TRUE),nbSample=1000)
}

\arguments{
  \item{x}{\code{[LongData]} or \code{[LongData3d]}: Object containing the trajectories
    to plot.}
  \item{y}{\code{[numeric]}: \code{Partition} that will be use to plot
    the object. If \code{y} is missing, a
    \code{Partition} with a single cluster is considered.}
  \item{parTraj}{\code{[ParLongData]}: Set the graphical parameters
    used to plot the trajectories. See
  \code{\linkS4class{ParLongData}} and examples
    for details.}
  \item{parMean}{\code{[ParLongData]}: Set the graphical parameters
    used to plot the mean trajectories of each clusters
    (only when \code{y} is non missing). See \code{\linkS4class{ParLongData}}
    and examples for details.}
  \item{parWin}{\code{[ParWindows]}: Set the graphical display of
    the windows. See \code{\linkS4class{ParWindows}} for details.}
  \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
    be time consumming. This parameters fixe the maximum number of
    trajectories (randomly chosen) that will be drawn.}
}

\details{
  \code{plot} either a \code{\linkS4class{LongData}},
  or each variable of a \code{\linkS4class{LongData3d}}
  optionnaly according to the \code{Partition} define by \code{y}.

  Graphical option concerning the individual trajectory (col, type, pch
  and xlab) can be change using \code{parTraj}.
  Graphical option concerning the cluster mean trajectory (col, type, pch,
  pchPeriod and cex) can be change using \code{parMean}. For more
  detail on  \code{parTraj} and  \code{parMean}, see object of
  class \code{\linkS4class{ParLongData}}.
}

\seealso{\code{\linkS4class{LongData}}, \code{\linkS4class{LongData3d}}, \code{\link[=plot3d,LongData3d]{plot3d}}.}


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
##################
### Construction of the data

ld <- gald3d()
part <- partition(rep(1:3,each=50))

### Basic plotting
plot(ld)
plot(ld,part)

### Change the windows orientation
plot(ld,parWin=windowsCut(c(1,2),addLegend=FALSE))


##################
### Changing graphical parameters 'par'

### No letters on the mean trajectories
plot(ld,part,parMean=parMEAN(type="l"))

### Only one letter on the mean trajectories
plot(ld,part,parMean=parMEAN(pchPeriod=Inf))

### Color individual according to its clusters (col="clusters")
plot(ld,part,parTraj=parTRAJ(col="clusters"))

### Mean without individual
plot(ld,part,parTraj=parTRAJ(type="n"))


### No mean trajectories (type="n")
### Color individual according to its clusters (col="clusters")
plot(ld,part,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"))

### Only few trajectories
plot(ld,part,nbSample=10,parTraj=parTRAJ(col='clusters'),parMean=parMEAN(type="n"))

##################
### single variable trajectory

ld2 <- gald()
part2 <- partition(rep(1:4,each=50))
plot(ld2)
plot(ld2,part2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{aplot}