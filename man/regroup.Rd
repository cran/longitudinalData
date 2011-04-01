\name{regroup}
\Rdversion{1.1}
\alias{regroup}

\title{
 ~ Function: regroup ~
}
\description{
Remove duplicate \code{\linkS4class{Partition}} present in a
 \code{\linkS4class{ListPartition}} (or, by inheritance, in
 \code{ClusterLongData} and  \code{ClusterLongData3d} objects.
}
\usage{
regroup(object)
}

\arguments{
  \item{object}{\code{[\linkS4class{ListPartition}]}: object
    that should be simplified.
  }
}

\details{
  A clusterizing algorithm can find a \code{Partition} several time. It
  is store several time in object \code{ListPartition}(or in
  \code{ClusterLongData} or in \code{ClusterLongData3d}), encombering
  the memory. \code{regroup} remove the duplicate
  \code{\linkS4class{Partition}}. Note that if the \code{ListPartition}
  is not ordered, then
  \code{regroup} sort it unless \code{toOrder=FALSE}.
}

\value{
  None (this function change internaly the field of an object, it does
  not return any values.)
}

\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011
}


\examples{
### Some data
data <- gald(30)["traj"]

### Some clustering
part2 <- partition(rep(c("A","B","A"),time=40),data)
part3 <- partition(rep(c("A","B","C"),time=40),data)

################
### ListPartition
listPart <- listPartition()

listPart["add"] <- part2
listPart["add"] <- part3
listPart["add"] <- part2
listPart["add"] <- part3

### Some clustering has been found several time
### regroup will suppress the duplicate one
regroup(listPart)
plotCriterion(listPart)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{classes}
\keyword{cluster}  % Partition
\keyword{ts}       % Time Series
