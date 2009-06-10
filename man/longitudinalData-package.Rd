\name{longitudinalData-package}
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
    Version: \tab 0.5\cr
    Date: \tab 2009-01-01\cr
    License: \tab GPL (>= 2)\cr
    Lazyload: \tab yes\cr
    Depends: \tab methods,clv\cr
    URL:\tab http://www.r-project.org\cr
  }
}


\section{Overview}{
  \code{longitudinalData} provide some tools to deal with the clusterization
  of longitudinal data, mainly:

  \enumerate{
    \item \link[=plotTraj]{plot}

    \item \link{plotSubGroups}

    \item \link{imputation}

    \item quality \link{criterion}
  }
}

\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : <genolini@u-paris10.fr>
}

\seealso{
  Classes: \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}} \cr
  Methods: \code{\link{longData}}, \code{\link{as.longData}}, \code{\link{selectSupTrajMinSize}}, \code{\link{partition}}\cr
  Plot: \code{\link[=plotTraj]{plot(LongData)}},\code{\link[=plotSubGroups]{plotSubGroups(LongData)}}\cr
  Imputation: \code{\link{imputation}}\cr
  Criterion: \code{\link{criterion}}\cr
}

\examples{
### Generation of artificial longData
data <- gald(percentOfMissing=0.3)
part <- partition(rep(1:4,each=50),4)
plot(data,part)

### Two methods of imputation
par(mfrow=c(1,2))
data1 <- imputation(data,method="linearInterpolation",partition=part)
plot(data1,part,legend=FALSE)

data2 <- imputation(data,method="LOCF")
plot(data2,part,legend=FALSE)
}

\keyword{package}  % Package Summaries
\keyword{dplot}    % Computations Related to Plotting
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series

