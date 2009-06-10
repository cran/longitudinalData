\name{longData}
\alias{longData}
\alias{longData,ANY,ANY,ANY,ANY,ANY-method}
\alias{longData,missing,missing,missing,missing,missing-method}
\title{ ~ Function: longData ~}
\description{
\code{longData} is the constructor of the class \code{\linkS4class{LongData}}.
}
\usage{
longData(traj, id, time, varName,other)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{\code{[matrix(numeric)]}: structure containning the trajectories.}
  \item{id}{\code{[vector(character)]}: single identifier for each trajectory (ie each individual).}
  \item{time}{\code{[vector(numeric)]}: time at which measures were made.}
  \item{varName}{\code{[character]}: name of the variable being measured.}
  \item{other}{\code{[list]}: list of additionnal information.}
}

\details{
  \code{longData} construct a object of class
  \code{\linkS4class{LongData}}. It does not provide any default
  values.
}
\value{
  An object of class \code{\linkS4class{LongData}}.
}

\seealso{\link{as.longData}, \linkS4class{LongData}, \link[=plotTraj]{plot}}

\examples{
### Small data
mat <- matrix(c(1,NA,3,2,3,6,1,8,10),3,3,dimnames=list(c(101,102,104),c("T2","T4","T8")))
(ld1 <- longData(traj=mat,id=as.character(c(101,102,104)),time=c(2,4,8),varName="T",other=list()))
plot(ld1,col=1,type.mean="n")

### Big data
mat <- matrix(runif(1051*325),1051,325)
(ld2 <- longData(traj=mat,id=paste("I-",1:1051,sep=""),time=(1:325)+0.5,varName="Random"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{package}
\keyword{ts}
\keyword{cluster}
\keyword{methods}



