\name{Partition-class}
\docType{class}
\alias{Partition-class}
\alias{[,Partition-method}
\alias{[<-,Partition-method}
\alias{show,Partition-method}

\title{~ Class: Partition ~}
\description{
  An object of class \code{Partition} is a clusterization of
  \code{\linkS4class{LongData}} in subgroups.
}

\section{Objects from the Class}{
  Objects are mainly intend to be created by some clusterization methods
  (like k-means, fuzzy k-means, mixture modeling, latent class
  analysis,...)
}

\section{Slots}{
  \describe{
    \item{\code{nbClusters}}{[numeric]: number of groups, between 1 and 52}
    \item{\code{clusters}}{[vector(factor)]: vector containing the affectation
      groups of each individual. The 26 first groups are in upper-case
      letters, the 26 next are in lower case letters.}
  }
}

\section{validation rules}{
  A class \code{Partition} object must follow some rules to be valid:
  \itemize{
    \item Slots should be either all empty, or all non empty.
    \item \code{nbClusters} has to be lower or equal to 52 (fifty two clusters maximum).
    \item \code{clusters} is a factor in \code{LETTERS[1:nbCluster]} if
      \code{nbClusters<=26}, in \code{c(LETTERS,letters[1:(nbCluster-26)])}
      otherwise).
  }
}


\section{Construction}{
  Class \code{Partition} objects are mainly constructed by some clusterization methods
  (like k-means, fuzzy k-means, mixture modeling, latent class
  analysis,...) Neverdeless, it is also possible to construct them from
  scratch using the fonction \code{\link{partition}}.
}


\section{Get [}{
  \describe{
    \item{Object["nbClusters"]}{[numeric]: Gets the number of clusters (the value of the slot \code{nbClusters})}
    \item{Object["clusters"]}{[vector(factor)]: Gets the cluster of each individual (the value of the slot \code{clusters})}
    \item{Object["clustersAsInteger"]}{[vector(integer)]: Gets the
      cluster of each individual and turn them into integer}
  }
}

\section{Setteur [<-}{
  \describe{
    \item{Object["nbClusters"]<-value}{[numeric]: Sets the number of
      clusters to value.}
    \item{Object["clusters"]<-value}{[vector(factor)]: Sets the cluster of each individual.}
  }
}


\references{Article submited\cr
}
\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : <genolini@u-paris10.fr>
}


\seealso{
  Overview: \code{\link{longitudinalData-package}} \cr
  Classes: \code{\linkS4class{LongData}} \cr
  Methods: \code{\link{partition}}\cr
}



\examples{
### Building Partition
part <- partition(rep(c("A","B","C"),time=10),nbClusters=4)

### '[' and '[<-'
part["clusters"]
part["nbClusters"]

part["nbClusters"]<-3
(part)
}


\keyword{classes}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
