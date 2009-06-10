\name{plotSubGroups}
\Rdversion{1.1}
\alias{plotSubGroups}
\alias{plotSubGroups,LongData}
\alias{plotSubGroups,LongData-method}

\title{ ~ Function: plotSubGroups for LongData ~ }
\description{
  Plot the trajectories of all the subgroups on several graph (one on
  each graph) of an object \code{\linkS4class{LongData}} relativly
  to a \code{\linkS4class{Partition}}. This function can plot the individual trajectories,
  the mean trajectories or both.
}
\usage{
plotSubGroups(x, y, ...)
}
\arguments{
  \item{x}{[LongData]: Object containing the trajectories to plot.}

  \item{y}{\code{[Partition]}: Give the \code{Partition} used to color the individual trajectories. If \code{y} is missing, a
    \code{Partition} with a single cluster is considered.}

  \item{\dots}{Graphical parameters to be passed to methods (see \code{plot}). For \code{LongData} object specificly :

    \itemize{
      \item{subGroups}{\code{[vector(character)]}: which subGroups of
	the \code{Partition} should be ploted ? By default, all are selected.}
      \item{type}{\code{[character]}: what type of plot should be drawn for the individual trajectories.}
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
        a point should be plot (usefull if there is a important number of time).}
      \item{cex.mean}{\code{[numeric]}: The magnification to be used for the symbol used as plotting point for the mean trajectories.}
      \item{legends}{\code{[logical]}: print the percent of individual in each groups.}
      \item{minSize}{\code{[numeric]}: minimum number of non missing value that an individual trajectories must contain to be drawn.}
    }
  }
}


\details{
  Plot the trajectories of all the subgroups on several graph (one on
  each graph) of an object \code{\linkS4class{LongData}} relativly
  to a \code{\linkS4class{Partition}}. This function can plot the individual trajectories,
  the mean trajectories or both.
  Some graphical parameters are available
  twice, once for the individual trajectories (like \code{type} or \code{col}), once for the
  mean trajectories (like \code{type.mean} or \code{col.mean}).
}


\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : <genolini@u-paris10.fr>
}

\seealso{\code{\linkS4class{LongData}},\code{\link[=plot]{plot(LongData)}}}


\examples{
#############
### Building data
ld <- gald()
par(ask=TRUE)

### Default ploting
plotSubGroups(ld)

### Only the trajectories in black
plotSubGroups(ld,col=1,type.mean="n")

### Plot according to a partition
###   (regular plot, then plotSubGroups
part <- partition(clusters=rep(LETTERS[1:4],each=50),nbClusters=4)
plot(ld,part,type.mean="n")
plotSubGroups(ld,part,type.mean="n")

### Only the mean trajectories, with letters (for publication ?)
plotSubGroups(ld,part,type="n",col.mean=1,size=2)

### All at once.
plotSubGroups(ld,part)

par(ask=FALSE)
}

\keyword{hplot}    % Computations Related to Plotting
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
\keyword{methods}

