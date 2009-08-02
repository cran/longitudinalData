\name{ordered}
\alias{ordered}
\alias{ordered,Partition}
\alias{ordered,Partition-method}
%\alias{partition,missing,missing-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: ordered ~ }
\description{
Given a \code{Partition}, \code{ordered} order its clusters letters
according to the effectif of each clusters. The is meanly a "label changing".
}
\usage{
ordered(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{[Partition)]}: Partition to be ordered.}
  \item{...}{For compatibility only, no other arguments are accepted.}
}
\details{
Given a \code{Partition}, \code{ordered} order its clusters letters
according to the effectif of each clusters. So A will be the cluster
with be biggest frequency, then B, then C...
}
\value{
  An object of class \code{\linkS4class{Partition}}.
}

\seealso{\linkS4class{Partition}}


\examples{
### Small partition
smallPart <- partition(clusters=c("A","B","A","C","C","C"),nbClusters=3)


### Same partition but ordered
ordered(smallPart)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{cluster}  % Clustering
\keyword{methods}
