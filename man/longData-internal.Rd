\name{longData-internal}
\alias{catShort}
%\alias{cleanProg}
\alias{LETTERSletters}
\alias{METHODS}

\alias{medianNA}
\alias{meanNA}
\alias{sdNA}
\alias{which.minNA}

\alias{is.tna}

\alias{NAtrunc}

%\alias{trajImput.copy.begin}
%\alias{trajImput.copy.end}
%\alias{trajImput.copy.middle}

%\alias{trajImput.copyMean}
%\alias{trajImput.globalSlope.begin}
%\alias{trajImput.globalSlope.end}
%\alias{trajImput.interpoLin}

%\alias{trajImput.interpoLin.middle}
%\alias{trajImput.interpoLin2}

%\alias{trajImput.interpoLin3}
%\alias{trajImput.localSlope.begin}

%\alias{trajImput.localSlope.end}
%\alias{trajImput.LOCB}
%\alias{trajImput.LOCB.begin}

%\alias{trajImput.LOCB.middle}
%\alias{trajImput.LOCF}
%\alias{trajImput.LOCF.end}

%\alias{trajImput.LOCF.middle}


\title{~ longData: internal objects and methods ~}
\description{longData: internal objects and methods}
\details{These are not to be called by the user.\cr
  If you have a specific need or for more details on longData, feel free to contact
  the author.
  \itemize{
    \item medianNA : median with option na.rm=TRUE
    \item meaanNA : mean with option na.rm=TRUE
    \item sdNA : sd with option na.rm=TRUE
    \item which.minNA : which.min that return NA if it argument has only
    NA values.
    \item NAtrunc : function that remove only the NA value in the tail
    of its argument.
    \item LETTERSletters : constant = c(LETTERS,letters)
    \item METHODS : constant=c("manhattan", "euclidean", "minkowski",
    "maximum", "canberra", "binary")
  }
}


\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : <genolini@u-paris10.fr>
}

\section{English translation}{
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}

\keyword{internal}

