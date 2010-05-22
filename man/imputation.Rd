\name{imputation}
\alias{imputation}
\alias{imputation,matrix-method}
\alias{imputation,LongData-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: imputation ~ }
\description{
\code{imputation} is a function that offer different methods to impute
missing value of a \code{\linkS4class{LongData}}.
}
\usage{
imputation(object, method, partition)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{[LongData]} or \code{[matrix]} : longitudinal
    data to impute}
  \item{method}{\code{[character]}: Name of the imputation method (see detail)}
  \item{partition}{\code{[Partition]} : for some imputation technique (like
    "copyMean"), a specific partition is needed. See detail.}
}
\details{
  \code{imputation} is a function that impute
  missing value of a \code{\linkS4class{LongData}}.
  Several imputation methods are available. For each method, the
  imputation has to deal with three kind of missing value :
  at start of the trajectorie (first
  values are missing), at the end (last values are
  missing) or in the middle (the missing value have surround by
  non-missing value). Here is a description of each methods (for all of
  them, an example is provided in the \code{Examples} section):

  \itemize{
    \item{LOCF (Last Occurence Carried Forward)}{
      \itemize{
	\item{Missing in the middle / at the end:}{ the previous non-missing value is
	  dipplicated forward.}
	\item{Missing at start:}{ the first non-missing occurence is
	  dupplicated backward (FOCB).}
      }
    }
    \item{FOCB (First Occurence Carried Backward)}{
      \itemize{
	\item{Missing at start / in the middle:}{ the next non-missing value is
	  dipplicated backward.}
	\item{Missing at the end:}{ the previous non-missing
	  occurence is dupplicated forward (LOCF).}
      }
    }
        \item{'LI-Global' (linear interpolation, global slope)}{
      \itemize{
	\item{Missing in the middle:}{ the value imediatly
	  surounding the missing are join by a line.}
	\item{Missing at start / at the end:}{ the line joining the first and last non-missing value
	  is considered (this line is the everage progression of the actual
	  individual trajectoire). Missing-value at start and at the end are chose on
	  this line.}
      }
    }
    \item{'LI-Local' (linear interpolation, local slope)}{
      \itemize{
	\item{Missing in the middle:}{ like
	  \code{LI-Global}, the value imediatly
	  surounding the missing are join by a line;}
	\item{Missing at start:}{ they are
	  chosen to prolongate the line going through the first non
	  missing and second non missing value.}
	\item{Missing at the end:}{ They are chosen to prolongate the line going through the last
	  missing value and the penultimate value.}
      }
    }
    \item{'LI-Bisector', (linear interpolation, mixed between global and local)}{
      \itemize{
	\item{Missing in the middle:}{ like
	  \code{LI-Global}, the value imediatly
	  surounding the missing are join by a line.}
	\item{Missing at start / at end:}{ since LI-Global is not
	  sensitive to local variation and LI-Local is too much sensitive to
	  abnormal value, LI-Mixed offer a medium solution by considering the
	  bissectrice of Global and Local solution. Point are chosen on
	  the bissectrices.}
      }
    }
    \item{'linearInterpolation'}{Same than LI-Bisector, for backward compatibility only.}
    \item{'LI-OCBF', (linear interpolation, FOCB and LOCF)}{
      \itemize{
	\item{Missing in the middle:}{ like
	  \code{LI-Global}, values imediatly
	  surounding the missing are join by a line.}
	\item{Missing at start:}{ imputed by FOCB.}
	\item{Missing at end:}{ imputed by LOCF.}
      }
    }
    \item{'copyMean'}{
      this method impute \code{LongData} relativly to
      a \code{Partition}. More precisely, each individual trajectorie is
      imputed relatively to its clusters center. For all kind of missing
      value, the shape of the clusters center is copied
      and is level up (or down) to fit with the non missing value of
      the imputed trajectorie.
    }
  }
}
\value{
  A \code{matrix} with no missing values.
}

\seealso{
  \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}}, \code{\link{criterion}}
}

\examples{
##################
### Preparation of the data
timeV <- 1:19
trajMissing <- longData(
   matrix(c(NA,NA ,2 ,3   ,NA,5 ,5.5,5.8,6 ,NA ,NA,6.5 ,7.5 ,NA ,NA ,NA ,4 ,NA,NA,
            2 ,0.5,-2,-2.6,2 ,1 ,1.5,0  ,-2,1.2,1 ,-3.5,-4.9,0.7,1.2,2.5,-1,-1, 1),2,byrow=TRUE),
   id=1:2,time=timeV,varName="V"
)

plot(timeV,trajMissing["traj"][1,],col=2,type="o",lwd=3,ylim=c(-2,10),
  xlab="Trajectorie to impute",ylab="")
par(ask=TRUE)

##################
### LOCF
trajImp <- imputation(trajMissing,method="LOCF")
plot(timeV,trajImp["traj"][1,],type="o",ylim=c(-2,10),ylab="",xlab="LOCF")
lines(timeV,trajMissing["traj"][1,],col=2,type="o",lwd=3)

### LOCB
trajImp <- imputation(trajMissing,method="FOCB")
plot(timeV,trajImp["traj"][1,],type="o",ylim=c(-2,10),ylab="",xlab="LOCB")
lines(timeV,trajMissing["traj"][1,],col=2,type="o",lwd=3)

### linearInterpolation
trajImp <- imputation(trajMissing,method="LI-Global")
plot(timeV,trajImp["traj"][1,],type="o",ylim=c(-2,10),ylab="",xlab="LI-Global")
lines(timeV,trajMissing["traj"][1,],col=2,type="o",lwd=3)

### linearInterpolation2
trajImp <- imputation(trajMissing,method="LI-Local")
plot(timeV,trajImp["traj"][1,],type="o",ylim=c(-2,10),ylab="",xlab="LI-Local")
lines(timeV,trajMissing["traj"][1,],col=2,type="o",lwd=3)

### linearInterpolation3
trajImp <- imputation(trajMissing,method="LI-Bisector")
plot(timeV,trajImp["traj"][1,],type="o",ylim=c(-2,10),ylab="",xlab="LI-Bissector")
lines(timeV,trajMissing["traj"][1,],col=2,type="o",lwd=3)

### linearInterpolation3
trajImp <- imputation(trajMissing,method="LI-OCFB")
plot(timeV,trajImp["traj"][1,],type="o",ylim=c(-2,10),ylab="",xlab="LI-LOCFB")
lines(timeV,trajMissing["traj"][1,],col=2,type="o",lwd=3)

### copyMean
meanTraj <- apply(trajMissing["traj"],2,meanNA)
plot(timeV,trajMissing["traj"][1,],type="o",ylim=c(-2,10),col=2,lwd=3,ylab="",
  xlab="LongData to impute and its model (in green)")
lines(timeV,meanTraj,col=3,type="b",lwd=3)

trajImp <- imputation(trajMissing,method="copyMean",partition=partition(nbCluster=1,clusters=c(1,1)))
plot(timeV,trajImp["traj"][1,],type="o",ylim=c(-2,10),ylab="",xlab="copyMean")
lines(timeV,trajMissing["traj"][1,],col=2,type="o",lwd=3)
lines(timeV,meanTraj,col=3,type="o",lwd=3)


### copyMean with several clusters
trajMiss <- longData(
   matrix(c(2,3,NA,0, NA,4,0,NA, 4,NA,-1,0),4),
   id=1:4,time=1:3
)
part <- partition(nbCluster=2,clusters=c(1,1,2,2))
imputation(trajMiss,method="copyMean",partition=part)
par(ask=FALSE)
}

\keyword{package}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
\keyword{NA}
\keyword{methods}
