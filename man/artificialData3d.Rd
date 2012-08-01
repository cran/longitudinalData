\name{artificialData3d}
\Rdversion{1.1}
\alias{artificialData3d}
\docType{data}

\title{~ Articial 3D Data ~}

\description{
  Some artificial joint longitudinal data.
}

\usage{data(artificialData3d)}

\format{
  Some joint longitudinal data in wide format. It includes 90 trajectories
  divided in 3 groups.

  \describe{
    \item{\code{id}}{unique idenfier for each patient.}
    \item{\code{v0}}{Measurment of variable 'V' at time t0}
    \item{\code{v1}}{Measurment of variable 'V' at time t1}
    \item{\code{...}}{...}
    \item{\code{v10}}{Measurment of variable 'V' at time t0}
    \item{\code{w0}}{Measurment of variable 'W' at time t0}
    \item{\code{w1}}{Measurment of variable 'W' at time t1}
    \item{\code{...}}{...}
    \item{\code{w10}}{Measurment of variable 'W' at time t0}
  }
}

\details{
  Some joint longitudinal data in wide format. It includes 90 trajectories
  divided in 3 groups.
}

\author{Christophe Genolini}

\examples{
data(artificialData3d)
str(artificialData3d)
}

\keyword{datasets}
\keyword{documentation}


