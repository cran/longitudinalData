\name{criterion3}
\Rdversion{1.1}
\alias{criterion3}
\alias{criterion3,matrix,Partition-method}
\alias{criterion3,LongData,Partition-method}

\title{
 ~ Function: criterion3 ~
}
\description{
Given a \code{\linkS4class{LongData}} and a
\code{\linkS4class{Partition}}, the fonction \code{criterion3} calculate
some criterions that are classicaly use to estimate the quality of a clusterization.
}
\usage{
criterion3(object, partition, method="linearInterpolation")
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
three criterions that are classicaly use to estimate the quality of a
clusterization : Calinski and Harabatz, Ray and Turi, Davies and Bouldin

If some individual have no clusters (ie if \code{Partition} has some
missing values), the corresponding trajectories are exclude from the
calculation.

Note that if there is an empty cluster or an empty longData, most of
the criterions are anavailable.
}

\value{A list:
  \itemize{
    \item{calinski}{\code{[numeric]}: Calinski and Harabatz criterion c(k)=Trace(B)/Trace(W)*(n-k)/(k-1)}
    \item{ray}{\code{[numeric]}: Ray and Turi R(k) =
      1/N Sum_{i in 1:k} sum_x in C_i |x-z_i|^2 / min(|z_i - z_j|^2)}
    \item{davies}{\code{[numeric]}: Davies and Bouldin D(k) = Avg(max(pairwise comparison))}
  }
}

\seealso{
  \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}},
  \code{\link{imputation}}, \code{\link{criterion}}
}

\examples{
##################
### Preparation of some artificial data
par(ask=TRUE)
traj <- gald()

### Correct partition
part1 <- partition(rep(1:4,each=50),4)
(cr1 <- criterion3(traj,part1))
plot(traj,part1,main=paste("Ray =",formatC(cr1[["ray"]])))


### Random partition
part2 <- partition(floor(runif(200,1,5)),4)
(cr2 <- criterion3(traj,part2))
plot(traj,part2,main=paste("Ray =",formatC(cr2[["ray"]])))

### Partition with 3 clusters instead of 4
part3 <- partition(rep(c(1,2,3,3),each=50),3)
(cr3 <- criterion(traj,part3))
plot(traj,part3,main=paste("Ray =",formatC(cr3[["ray"]])))

### Comparisons of the Partition
cr1["ray"]
cr2["ray"]
cr3["ray"]
par(ask=FALSE)
}

\keyword{package}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
\keyword{methods}
