library(rgl)
library(misc3d)

setwd("C:/Users/GENOLLINI/Documents/cgenolini/packages/longitudinalData/testsDev")
source("../data/constants.R")
source("testFunction.R")
source("testLongData.R")      ## Besoin de rien (1)
source("testLongData3d.R")    ## Besoin de rien (1)
source("testImputation.r")    ## longData (2)
source("testPartition.R")     ## Constructeur sans londData précède criterion
source("testListPartition.R") ## Suit partition
#source("testCriterion.r")    ## longData et partition
#source("testPartitionBis.R")  ## Constructeur final suit criterion
source("testParLongData.R")   ## Besoin de partition.
source("testParWindows.R")    ## Besoin de rien (1b)
source("testLongDataPlot.R")  ## Final
#source("testPlotOneByOne.R")
#source("../R/clean.r")
