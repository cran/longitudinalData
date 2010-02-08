\name{plot,LongData}
\Rdversion{1.1}
\alias{plot}
\alias{plot,LongData}
\alias{plot,LongData,ANY-method}
\alias{plot,LongData,missing-method}
\alias{plotTraj}
\alias{plotTraj,LongData}
\alias{plotTraj,LongData-method}

\title{ ~ function: plot for LongData ~ }
\description{
  \code{plot} the \code{\linkS4class{LongData}}, either alone or relatively
  to a \code{\linkS4class{Partition}}. This function can plot the individual trajectories,
  the mean trajectories or both.
}

\usage{
plot(x,y,...)
%plotTraj(x,y,...)
}

\arguments{
  \item{x}{\code{[LongData]}: Object containing the trajectories to plot.}

  \item{y}{\code{[Partition]}: Gives the \code{Partition} used to color the trajectories. If \code{y} is missing, a
    \code{Partition} with a single cluster is considered.}

  \item{\dots}{Graphical parameters to be passed to methods (see \code{plot}). For \code{LongData} object specificly :

    \itemize{
      \item{subGroups}{\code{[vector(character)]}: which subGroups of
	the \code{Partition} should be ploted ? By default, all are selected.}
      \item{type}{\code{[character]}: what type of plot should be drawn
      for the individual trajectories.}
      \item{type.mean}{\code{[character]}: what type of plot should be drawn for the mean trajectories.}
      \item{col}{\code{[character]}, \code{[numeric]} or \code{vector[numeric]}:
        Specification of the ploting color of the individual trajectories.
        In addition to the standard possibles values, \code{col="clusters"}
        can be use to color the individual trajectories according to their clusters.}
      \item{col.mean}{\code{[character]}, \code{[numeric]} or \code{vector[numeric]}:
        Specification of the ploting color of the mean trajectories.
        In addition to the standard possibles values, \code{col="clusters"}
        can be use to color each mean trajectories according to its clusters.}
      \item{lty}{\code{[character]}: the line type for the individual trajectories.}
      \item{lty.mean}{\code{[character]}: the line type for the mean trajectories.}
      \item{pch}{\code{[character]}: specify the symbol to be used as plotting
        point on the individual trajectories.}
      \item{pch.mean}{\code{[character]}: specify the symbol to be used as plotting
        point on the mean trajectories. Option
        \code{pch.mean="symbols"} or \code{pch.mean="letters"} can be used.}
      \item{pch.time}{\code{[vector(numeric)]}: precise the time at which
        a point should be plot (usefull if there is a important number
      of time, see example).}
      \item{cex.mean}{\code{[numeric]}: The magnification to be used for the symbol used as plotting point for the mean trajectories.}
      \item{legends}{\code{[logical]}: print the percent of individual in each groups.}
      \item{minSize}{\code{[numeric]}: minimum number of non missing value that an individual trajectories must contain to be drawn.}
    }
  }
}
\details{
  \code{plot} the \code{\linkS4class{LongData}}, either alone or relatively
  to a \code{\linkS4class{Partition}}. This function can plot the individual trajectories,
  the mean trajectories or both. Some graphical parameters are available
      twice, once for the individual trajectories (like \code{type} or \code{col}), once for the
  mean trajectories (like \code{type.mean} or \code{col.mean}).
}

\seealso{\code{\linkS4class{LongData}}, \code{\link[=plotSubGroups]{plotSubGroups(LongData)}}}

\examples{
#############
### Small example
traj1 <- gald()
part1 <- partition(rep(LETTERS[1:4],each=50),nbClusters=4)

### Several way to print the longData
par(ask=TRUE)
plot(traj1)
plot(traj1,part1)

### Only the longData, in black
plot(traj1,part1,type.mean="n", col=1)

### Only the clusters centers
plot(traj1,part1,type="n")

### only the clusters centers, in black with no letters
plot(traj1,part1,type="n",col.mean="black",type.mean="l")

### Big letter, in black (for publication...)
plot(traj1,part1,type="n",col.mean="black",cex.mean=2)


#############
### Big example
traj2 <- gald(time=0:500,functionClusters=list(
    function(t){5},
    function(t){-5},
    function(t){5*sin(t/25)}
))
part2 <- partition(rep(LETTERS[1:3],each=50),nbClusters=3)
plot(traj2,part2)

### Only the clusters mean
plot(traj2,part2,type="n")

### With not as much letters...
plot(traj2,part2,pch.time=25+0:9*50,cex=2,type="n")
par(ask=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{ts}
\keyword{aplot}