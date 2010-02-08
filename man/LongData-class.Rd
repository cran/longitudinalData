\name{LongData-class}
\docType{class}
\alias{LongData}
\alias{LongData-class}
\alias{[,LongData-method}
\alias{[<-,LongData-method}
%\alias{addClusterization,LongData-method}
%\alias{addPartition,LongData-method}
%\alias{choice,LongData-method}
%\alias{getCalinskiCriterion,LongData-method}
%\alias{getClusterizList,LongData,missing,missing-method}
%\alias{getClusterizList,LongData,numeric,missing-method}
%\alias{getClusterizList,LongData,numeric,numeric-method}
%\alias{KmL,LongData-method}
%\alias{plot,LongData,ANY-method}
%\alias{print,LongData-method}
\alias{show,LongData-method}

\title{~ Class: LongData ~}
\description{\code{LongData} is an objet containing the longitudinal
  data (the individual trajectories) and some associate value (like time, individual
  identifiant,...)
}

\section{Objects from the Class}{
  Object \code{LongData} can be created either directly by calling
  the fonction \code{\link{longData}} or form an existing structure
  (\code{data.frame} or \code{matrix}) by using
  \code{\link{as.longData}}.
}

\section{Slots}{
  \describe{
    \item{\code{id}}{\code{[character]}: Single identifier
      for each of the longData (each individual).}
    \item{\code{time}}{\code{[numeric]}: Time during which measures were made.}
    \item{\code{varName}}{\code{[character]}: Name of the variable measured.}
    \item{\code{traj}}{\code{[array(numeric)]}: Contains
      the longitudianl data. Each line corresponds to the trajectory of an
      individual. The columns refer to the time during which measures
      were made.}
    \item{\code{other}}{\code{[list]}: list of additionnal information (see
      section \code{Value} in
      \code{\link{generateArtificialLongData}} for an example).}
  }
}

\section{Construction}{
  Object \code{LongData} can be created either directly by calling
  the fonction \code{\link{longData}} (build from scratch) or
  via \code{\link{as.longData}}
  (turning a \code{data.frame} into a \code{LongData}).
}

\section{Get [}{
  \describe{
    \item{Object["id"]}{[vecteur(character)]: Gets each individual
      indentifiant (the value of the slot \code{id})}
    \item{Object["time"]}{[vecteur(numeric)]: Gets the times (the value of the slot \code{time})}
    \item{Object["varName"]}{[character]: Gets the name of the variable (the value of the slot \code{varName})}
    \item{Object["traj"]}{[matrix(numeric)]: Gets all the longData'
      values (the value of the slot \code{traj})}
  }
}


\section{Set [<-}{
  \describe{
    \item{Object["id"]<-value}{[vecteur(character)]: Sets each individual
      indentifiant to \code{value}}
    \item{Object["time"]<-value}{[vecteur(numeric)]: Sets the times to
      \code{value}.}
    \item{Object["varName"]<-value}{[character]: Sets the name of the
      variable to \code{value}.}
    \item{Object["traj"]}{[matrix(numeric)]: Sets all the longData'
      to \code{values}.}
    \item{Object["traj"][3,]<-value}{[vecteur(numeric)]: Sets the trajectorie of
      the individual 3 to \code{value}.}
  }
}



\section{Methods}{
  \describe{
    \item{\code{\link[=plotTraj]{plot(LongData)}}}{display the LongData.}
    \item{\code{\link[=plotTraj]{plot(LongData,Partition)}}}{display the LongData according to a specific
      \code{\linkS4class{Partition}}.}
    \item{\code{\link[=plotSubGroups]{plotSubGroups(LongData,Partition)}}}{display
      one graph for each clusters, sub-groups by sub-groups
      according to a specific\code{\linkS4class{Partition}}.}
    \item{\code{\link[=selectSupTrajMinSize]{selecSupTrajMinSize(LongData,minSize)}}}{select
      the individual trajectories that are made by at least \code{minSize} values
      (usefull to exclude from the LongData some individual trajectories with to many missing values.)}
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
  Methods: \code{\link{longData}}, \code{\link{as.longData}}, \code{\link{selectSupTrajMinSize}}, \code{\link{imputation}}, \code{\link{criterion}}\cr
  Plot: \code{\link[=plotTraj]{plot(LongData)}},\code{\link[=plotSubGroups]{plotSubGroups(LongData)}}
}


\examples{
### building longData
mat <- matrix(c(NA,2,3,4,1,6,2,5,1,3,8,10),4)
ld <- new("LongData",id=c("I1","I2","I3","I4"),time=c(2,4,8),varName="Age",traj=mat)

### '[' and '[<-'
ld["id"]
ld["time"]<- c(1,3,9)
ld["varName"]
ld["traj"]
ld["traj"][3,]<-c(2,7,9)
(ld)

### Plot
plot(ld,type.mean="n",legend=FALSE)

### Only trajectories with at least 3 values
selectSupTrajMinSize(ld,3)
}
\keyword{classes}
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
