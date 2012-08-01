\name{saveTrianglesAsASY}
\alias{saveTrianglesAsASY}

\title{
  ~ Function: saveTrianglesAsASY ~
}
\description{
  Export a \code{Triangle} object to an '.azy' file.
}
\usage{
saveTrianglesAsASY(scene, filename = "scene.asy")
}

\arguments{
  \item{scene}{\code{[Triangle]}: Object representing the graph to plot,
    probably produce by \code{\link{plot3dPdf}}.}
  \item{filename}{\code{[character]}: Name of exported file.}
}
\details{
  Export a \code{Triangle} object to an '.asy' file. See
  \code{\link{plot3dPdf}} for a summary of the overall procedure.
}
\value{
  An '.asy' file, in the current directory.
}

\seealso{
  \code{\link{plot3dPdf}},\code{\link{makeLatexFile}},\code{\link[misc3d:makeTriangles]{makeTriangles}}
}


\references{\url{http://www.stat.uiowa.edu/~luke/R/misc3d/misc3d-pdf/}}

\section{Author(s)}{
  Luke Tierney \cr
  Chair, Statistics and Actuarial Science \cr
  Ralph E. Wareham Professor of Mathematical Sciences\cr
  University of Iowa \cr
}

\examples{
  ### Generating the data
  data(artificialData3d)
  myLd <- longData3d(artificialData3d,timeInData=list(var1=2:12,var2=13:23))
  part <- partition(rep(1:3,each=50))
  plotTraj3d(myLd,part)

  ### Creation of the scene
  scene <- plot3dPdf(myLd,part)
  drawScene.rgl(scene)

  ### Export in '.azy' file
  saveTrianglesAsASY(scene)

  ### Creation of a '.prc' file
  # Open a console, then run:
  # 'asy -inlineimage -tex pdflatex scene.azy'

  ### Creation of the LaTeX main document
  makeLatexFile()

  ### Creation of the '.pdf'
  # Open a console window, then run
  # pdfLatex main.tex
}

