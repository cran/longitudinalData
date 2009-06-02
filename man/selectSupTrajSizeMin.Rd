\name{selectSupTrajMinSize}
\alias{selectSupTrajMinSize}
\alias{selectSupTrajMinSize,LongData-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{~ Function: selecSupTrajMinSize ~}
\description{
  When an individual trajectories contain too many missing value, it
  might be necessary to exclude it.
  Give an object of class \code{\linkS4class{LongData}},
  \code{selecSupTrajMinSize} select the trajectories that does not contain
  too many missing value.
}
\usage{
selectSupTrajMinSize(object,minSize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{[LongData]}: Object from who some individual
    trajectories may be exclude}
  \item{minSize}{\code{[numeric]}: minimum number of non missing value
    that an individual trajectories must contain to not be exclude.}
}
\details{
  Individual trajectories whose values are partially missing can either be
  excluded or included in a computation. \code{minSize} sets the
  minimum number of values that a trajectory must contain to not be
  excluded. For example, if a trajectories have 7 mesurements
  (\code{time=7}) and \code{minSize} is set to 3, the trajectory
  \code{(5,3,NA,4,NA,NA,NA)} will be include while
  \code{(2,NA,NA,NA,4,NA,NA)} will be exclude. Please note that
  trajectories that are totally missing (i.e. 0 present values) are always excluded.
}
\value{
  A logical vecteur taking values TRUE for the trajectories to include, FASLE for the
  trajectories to exclude.
}

\seealso{\code{\linkS4class{LongData}}}


\examples{
### Construction of the LongData
trajA <- as.longData(matrix(c(4,1,1,NA,NA,1,NA,NA,2,2,3,NA),4))

### Selection of the longData with at least 2 values
selectSupTrajMinSize(trajA,2)

### Selection of the longData with no missing value
selectSupTrajMinSize(trajA,3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{ts}
\keyword{methods}
