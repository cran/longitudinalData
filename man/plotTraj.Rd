\name{plotTraj,LongData}
\Rdversion{1.1}
\alias{plotTraj}
\alias{plotTraj,LongData}
\alias{plotTraj,LongData-method}
\alias{plotTraj,LongData,missing-method}
\alias{plotTraj,LongData,Partition-method}
\alias{plotTraj,LongData3d}
\alias{plotTraj,LongData3d-method}
\alias{plotTraj,LongData3d,missing-method}
\alias{plotTraj,LongData3d,Partition-method}

\title{ ~ function: plotTraj for LongData or LongData3d~ }
\description{
  Plot the \code{\linkS4class{LongData}} or \code{\linkS4class{LongData3d}} optionnaly relatively
  to a \code{\linkS4class{Partition}}. For joint trajectories, one
  graphe for each variable trajectory is displayed.
}

\usage{
\S4method{plotTraj}{LongData,Partition}(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar'],addLegend=TRUE),nbSample=1000,...)
\S4method{plotTraj}{LongData3d,Partition}(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar'],addLegend=TRUE),nbSample=1000,...)
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
  \item{...}{Arguments to be passed to methods, such as graphical parameters.}
}

\details{
  Plot either a \code{\linkS4class{LongData}},
  or each variable of a \code{\linkS4class{LongData3d}}
  optionnaly according to the \code{Partition} define by \code{y}.

  Graphical option concerning the individual trajectory (col, type, pch
  and xlab) can be change using \code{parTraj}.
  Graphical option concerning the cluster mean trajectory (col, type, pch,
  pchPeriod and cex) can be change using \code{parMean}. For more
  detail on  \code{parTraj} and  \code{parMean}, see object of
  class \code{\linkS4class{ParLongData}}.
}

\seealso{\code{\linkS4class{LongData}}, \code{\linkS4class{LongData3d}}, \code{\link{plotTraj3d}}.}


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
data(artificialData3d)
ld <- longData3d(artificialData3d,timeInData=list(var1=2:12,var2=13:23))
part <- partition(rep(1:3,each=50))

### Basic plotting
plotTraj(ld)
plotTraj(ld,part)

### Change the windows orientation
plotTraj(ld,parWin=windowsCut(c(1,2),addLegend=FALSE))


##################
### Changing graphical parameters 'par'

### No letters on the mean trajectories
plotTraj(ld,part,parMean=parMEAN(type="l"))

### Only one letter on the mean trajectories
plotTraj(ld,part,parMean=parMEAN(pchPeriod=Inf))

### Color individual according to its clusters (col="clusters")
plotTraj(ld,part,parTraj=parTRAJ(col="clusters"))

### Mean without individual
plotTraj(ld,part,parTraj=parTRAJ(type="n"))


### No mean trajectories (type="n")
### Color individual according to its clusters (col="clusters")
plotTraj(ld,part,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"))

### Only few trajectories
plotTraj(ld,part,nbSample=10,parTraj=parTRAJ(col='clusters'),parMean=parMEAN(type="n"))


##################
### single variable trajectory

data(artificialData)
ld2 <- longData(artificialData)
part2 <- partition(rep(1:4,each=50))
plotTraj(ld2)
plotTraj(ld2,part2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{aplot}