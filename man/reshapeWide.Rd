\name{reshapeWide}
\alias{reshapeWide}
\title{
  ~ Function: reshapeWide ~
}
\description{
  This function reshapes a data frame in 'long' format (repeated
  measurements in the same column) into a data frame in 'wide' format
  (repeated measurements in separate columns).
}
\usage{
reshapeWide(data, varFixed, varLong, varTime, varDrop)
}

\arguments{
  \item{data}{\code{[data.frame]}: a data frame in 'long' format (repeated
    measurements in the same column)}
  \item{varFixed}{\code{[charater]} or \code{[vector(character)]}: names
    of classical (non-longitudinal) variables, variables that does not vary with time (like sex, blood groups).
  }
  \item{varLong}{\code{[charater]} or \code{[vector(character)]}: names
    of longitudinal variables, variables that vary with time (like sex, blood groups).
  }
  \item{varTime}{\code{[charater]}:
    variable that record the time at witch the longitudinal variable have
    been measured (like ages, year, month).
  }
  \item{varDrop}{\code{[charater]} or \code{[vector(character)]}:
    variable that should not appears in the new data frame.
  }
}

\details{
  This function reshapes a data frame in 'long' format (repeated
  measurements in the same column) into a data frame in 'wide' format
  (repeated measurements in separate columns). If one of the four
  arguments \code{varFixed}, \code{varLong}, \code{varTime} and
  \code{varDrop} is missing, it will take the value of all the variable
  non already include in one of the fields.
}

\value{
A data frame in 'wide' format (repeated measurements in separate columns).
}

\author{
Christophe Genolini
}
\note{
  \code{reshapeWide} is only a simplified version of \code{reshape}
  (package \code{stats}).
  If you need a more powerful version of \code{reshapedWide} with more options, see \code{\link[=stats]{reshape}}.
}


\seealso{
  \code{\link[=stats]{reshape}}.
}
\examples{

summary(Indometh)
reshapeWide(Indometh,varTime="time",varFixed="Subject",varLong="conc")


df2 <- data.frame(id = rep(1:4, rep(2,4)),
                 visit = I(rep(c("Before","After"), 4)),
                 x = rnorm(4), y = runif(4),
                 sex=rep(c("H","F","H"),time=c(4,2,2)))[1:7,]
reshapeWide(df2,varTime="visit",varFixed="id",varLong=c("x","y"))
reshapeWide(df2,varTime="visit",varFixed=c("id","sex"),varLong=c("x"))
reshapeWide(df2,varTime="visit",varFixed=c("id","sex"),varDrop="")
}
