\name{partition}
\alias{partition}
\alias{partition,ANY,ANY-method}
\alias{partition,missing,missing-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: partition ~ }
\description{
\code{partition} is the constructor of the class \code{\linkS4class{Partition}}.

}
\usage{
partition(clusters, nbClusters)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusters}{\code{[vector(factor)]}: cluters to which each
    individual belongs. Each clusters is represented by a letters (upper
    and lower represent different clusters).}
  \item{nbClusters}{\code{[numeric]}: number of clusters.}
}
\details{
  \code{partition} construct a object of class
  \code{\linkS4class{Partition}}. It does not provide any default
  values.
}
\value{
  An object of class \code{\linkS4class{Partition}}.
}

\seealso{\linkS4class{Partition},\code{\link{ordered}}}


\examples{
### Empty partition
partition()

### Small partition
partition(clusters=c("A","B","A","C","C"),nbClusters=3)


### Big random partition
partition(clusters=LETTERS[floor(runif(1000,1,5))],nbClusters=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{cluster}  % Clustering
\keyword{methods}
