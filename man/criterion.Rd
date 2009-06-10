\name{criterion}
\Rdversion{1.1}
\alias{criterion}
\alias{criterion,matrix,Partition,ANY-method}
\alias{criterion,LongData,Partition,ANY-method}
\alias{criterion,matrix,Partition-method}
\alias{criterion,LongData,Partition-method}

\title{
 ~ Function: criterion ~
}
\description{
Given a \code{\linkS4class{LongData}} and a
\code{\linkS4class{Partition}}, the fonction \code{criterion} calculate
some criterions that are classicaly use to estimate the quality of a clusterization.
}
\usage{
criterion(object, partition, method)
}
\arguments{
  \item{object}{\code{[LongData]}: object on which the criterion are calculate}
  \item{partition}{\code{[Paritition]}: clusterization of the \code{LongData}}
  \item{method}{\code{[character]}: if some value are missing in the
    \code{LongData}, it is necessary to impute them. The function
    \code{criterion} call the function \code{\link{imputation}} using
    the method \code{method}.}
}

\details{
Given a \code{\linkS4class{LongData}} and a
\code{\linkS4class{Partition}}, the fonction \code{criterion} calculate
some criterions that are classicaly use to estimate the quality of a clusterization.

If some individual have no clusters (ie if \code{Partition} has some
missing values), the corresponding trajectories are exclude from the
calculation.

Note that if there is an empty cluster or an empty longData, most of
the criterions are anavailable.
}

\value{A list:
  \itemize{
    \item{varBetween}{\code{[matrix]}: variance between}
    \item{varWithin}{\code{[matrix]}: variance within}
    \item{calinski}{\code{[numeric]}: Calinski and Harabatz criterion}
  }
}

\seealso{
  \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}}, \code{\link{imputation}}
}

\examples{
##################
### Preparation of some artificial data
par(ask=TRUE)
traj <- gald()

### Correct partition
part1 <- partition(rep(1:4,each=50),4)
(cr1 <- criterion(traj,part1))
plot(traj,part1,main=paste("Calinski =",formatC(cr1[["calinski"]])))


### Random partition
part2 <- partition(floor(runif(200,1,5)),4)
(cr2 <- criterion(traj,part2))
plot(traj,part2,main=paste("Calinski =",formatC(cr2[["calinski"]])))

### Partition with 3 clusters instead of 4
part3 <- partition(rep(c(1,2,3,3),each=50),3)
(cr3 <- criterion(traj,part3))
plot(traj,part3,main=paste("Calinski =",formatC(cr3[["calinski"]])))

### Comparisons of the Partition
cr1["calinski"]
cr2["calinski"]
cr3["calinski"]
par(ask=FALSE)
}

\keyword{package}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
\keyword{methods}
