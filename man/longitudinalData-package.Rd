\name{longitudinalData-package}
\alias{longitudinalData}
\alias{longitudinalData-package}
\docType{package}
\title{~  Package overview: longitudinalData ~}

\description{
  \code{longitudinalData} provide some tools to deal with the clusterization
  of longitudinal data.
}
\details{
  \tabular{ll}{
    Package: \tab longitudinalData\cr
    Type: \tab Package\cr
    Version: \tab 2.0\cr
    Date: \tab 2012-04-01\cr
    License: \tab GPL (>= 2)\cr
    Lazyload: \tab yes\cr
    Depends: \tab methods,clv,rgl,misc3d\cr
    URL:\tab http://www.r-project.org\cr
  }
}


\section{Overview}{
  \code{longitudinalData} provide some tools to deal with the clusterization
  of longitudinal data, mainly:

  \enumerate{
    \item \link[=plot]{plot}

    \item \link{imputation}

    \item \link{qualityCriterion}
  }
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

\seealso{
  Classes: \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}} \cr
  Methods: \code{\link{longData}}, \code{\link{partition}}, \code{\link{ordered}}\cr
  Plot: \code{\link[=plot,LongData]{plot(LongData)}}, \code{\link[=plot3d,LongData3d]{plot3d(LongData)}}\cr
  Imputation: \code{\link{imputation}}\cr
  Criterion: \code{\link{qualityCriterion}}\cr
}

\examples{
### Generation of artificial longData
data <- gald3d(percentOfMissing=0.3)
part <- partition(rep(1:3,each=50))
plot3d(data,part)

### Imputation
data1 <- imputation(data,method="copyMean")

### Quality criterion
# qualityCriterion(data,part)
}

\keyword{package}  % Package Summaries
\keyword{dplot}    % Computations Related to Plotting
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering

