\name{as.longData}
\alias{as.longData}
\alias{as.longData,data.frame-method}
\alias{as.longData,matrix-method}
\title{ ~ Function: as.longData ~}
\description{
\code{as.longData} turns a \code{data.frame} or a \code{matrix} into an
object of class \code{\linkS4class{LongData}}.
}
\usage{
as.longData(data, id, timeCol, timeReal, varName,other)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{[data.frame]} or \code{[matrix]}: structure
    containning the longitudinal data.}
  \item{id}{\code{[character]}: single identifier for each trajectory (ie each individual).
    \itemize{
      \item If \code{data} is a \code{data.frame}, the default \code{id} is the first column of the \code{data.frame}.
      \item If \code{data} is a \code{matrix}, the default \code{id} is the vector \code{1:nrow(matrix)}.
    }
  }
  \item{timeCol}{\code{[numeric]}: column number in which longitudinal data can be found.
    \itemize{
      \item If \code{data} is a \code{data.frame}, the default \code{timeCol} is all the column of the \code{data.frame} but the first.
      \item If \code{data} is a \code{matrix}, the default \code{timeCol} is all the column of the \code{matrix}.
    }
  }
  \item{timeReal}{\code{[numeric]}: time at which measures were
    made in "real" life. By default,\code{timeReal} is \code{0:(length(timeCol)-1)}.}
  \item{varName}{\code{[character]}: Name of the variable being measured ("V" by default).}
  \item{other}{\code{[list]}: list of additionnal information.}
}

\details{
  \code{as.longData} turns a \code{data.frame} or a \code{matrix} into an
object of class \code{\linkS4class{LongData}}.
}
\value{
  An object of class \code{\linkS4class{LongData}}.
}

\seealso{\link{longData}, \linkS4class{LongData}}

\examples{
#################
### Simple data.frame
df <- data.frame(id=11:12,Va1=c(1,4),Va2=c(2,5))
as.longData(df)


#################
### Complexe data.frame
df <- data.frame(id=11:12,Va1=c(1,4),To1=c(0.1,0.4),Va2=c(2,5),To2=c(0.1,0.3))

### Transformation in a LongData, selecting variable "Va"
as.longData(df,timeCol=c(2,4))

### Transformation in a LongData, selecting variable "To"
as.longData(df,timeCol=c(3,5))

### Changing the id's name
as.longData(df,id=c("Ind-1","Ind-2"),timeCol=c(2,4))

### Changing the real time and the variable name.
as.longData(df,timeCol=c(3,5,2,4),timeReal=c(1,2,4,8),varName="Size")



#################
### matrix
mat <- matrix(1:8,2,dimnames=list(c("I1","I2"),c("Size1.2","Size1.5","Size2.5","Size4")))
as.longData(mat)
as.longData(mat,timeReal=c(1.2,1.5,2.5,4))
as.longData(mat,timeReal=c(1.2,2.5,4),timeCol=c(1,3,4),varName="Size")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{package}
\keyword{ts}       % Time Series
\keyword{methods}


