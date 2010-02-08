\name{pathFrechet}
\alias{pathFrechet}
\alias{pathFrechetR}

\title{~ Function: Frechet distance ~}

\description{
Compute Frechet distance and Frechet path between two trajectories.
}
\usage{
pathFrechet(P, Q, method = "max", Fdist = dist)
pathFrechetR(P, Q, method = "max", Fdist = dist)
}

\arguments{
  \item{P}{[vector(numeric)] First trajectories.}
  \item{Q}{[vector(numeric)] First trajectories.}
  \item{method}{[character] Method used. Can be either 'max' or 'sum'}
  \item{Fdist}{[numeric <- function(numeric,numeric)] Frechet distance
  between two trajectories use a distance that is use the compute the
  distance between points of the trajectories. Fdist can be used to
  define a specific distance. The special value "2D" is used for
  "euclidean" distance. The special value "1D" can be use for
  considering that the trajectories are in 1D.}

}
\details{
  Given two curve P and Q, given a distance d, Frechet distance between P and Q is define as
  \code{inf_{a,b} max_{t} d(P(a(t)),Q(b(t)))}. It's computation is a
  NP-complex problem. When P and Q are trajectories (discrete curve), the
  problem is polynomial (and quite simple).

  The Frechet distance can
  also be define using a sum instead of a max: \code{inf_{a,b} sum_{t} d(P(a(t)),Q(b(t)))}

  The Frechet path [...]

  The function \code{pathFrechetR} is code in R.
  The function \code{pathFrechet} is coded
  in C (and is thus much faster than the two other). Note that
  \code{pathFrechet} (the fastest) can only use the "2D" and "1D" distance.
}

\value{
  A numeric value and the Frechet path.
}
\references{
  Thomas Eiter & Heikki Mannila: "Computing Discrete Fr´echet Distance"
}
\author{Christophe Genolini}

\seealso{distFrechet}

\examples{
   P <- rnorm(7)
   Q <- rnorm(6)

   ### Optimized function
   pathFrechetR(P,Q)

   ### Function compiled in C
   pathFrechet(P,Q)

   ### Frechet using sum instead of max.
   pathFrechet(P,Q,method="sum")

   ### Frechet using "manhattan" distance
   pathFrechetR(P,Q,Fdist=function(x)dist(x,method="manhattan"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
