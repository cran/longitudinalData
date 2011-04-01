\name{distFrechet}
\alias{distFrechet}

\title{~ Function: Frechet distance ~}

\description{
Compute Frechet distance between two trajectories.
}
\usage{
distFrechet(P, Q, method = "max", Fdist = dist)
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
  Given two curve P and Q and a distance d, Frechet distance between P and Q is define as
  \code{inf_{a,b} max_{t} d(P(a(t)),Q(b(t)))}. It's computation is a
  NP-complex problem. When P and Q are trajectories (discrete curve), the
  problem is polynomial (and quite simple). The Frechet distance can
  also be define using a sum instead of a max: \code{inf_{a,b} sum_{t} d(P(a(t)),Q(b(t)))}

  The function \code{distFrechetRec} use the recursive algorithm define by Thomas
  Eiter and Heikki Mannila. The function \code{distFrechetR} use a more efficiant
  algorithm in R. The function \code{distFrechet} use the efficiant algorithm
  in C (and is thus much faster than the two other). Note that
  \code{distFrechet} (the fastest) can only use the "2D" and "1D".
}

\value{
  A numeric value.
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  Thomas Eiter & Heikki Mannila: "Computing Discrete Fr´echet Distance"\cr

  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\seealso{distTraj}

\examples{
   P <- rnorm(7)
   Q <- rnorm(6)

   ### Function from Eiter and Mannila compiled in C
   distFrechet(P,Q)

   ### Frechet using sum instead of max.
   distFrechet(P,Q,method="sum")

   ### Frechet using "manhattan" distance
   distFrechet(P,Q,Fdist=function(x)dist(x,method="manhattan"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
