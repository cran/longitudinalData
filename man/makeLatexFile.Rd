\name{makeLatexFile}
\alias{makeLatexFile}

\title{
  ~ Function: makeLatexFile ~
}
\description{
  Create a LaTeX document that inclusde 3D objects into PDF documents.
}
\usage{
makeLatexFile(filename = "main.tex", asyToInclude = "scene+0.prc")
}

\arguments{
  \item{filename}{Name of the LaTeX file}
  \item{asyToInclude}{Name of the file holding the 3D graph to include.}
}

\details{
  Create a LaTeX document that inclusde 3D objects into PDF documents with PDF-1.5/1.6 compatibility.
}
\value{
  A LaTeX file, in the current directory.
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

\seealso{
  \link[misc3d:makeTriangles]{makeTriangles},\code{\link{plot3dPdf}}, \code{\link{saveTrianglesAsASY}.}
}



\examples{
  ### Generating the data
  myLd <- gald3d()
  part <- partition(rep(1:3,each=50))
  plot3d(myLd,part)

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


