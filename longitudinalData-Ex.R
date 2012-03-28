pkgname <- "longitudinalData"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('longitudinalData')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Constants")
### * Constants

flush(stderr()); flush(stdout())

### Name: Constants
### Title: ~ Constants define in the package ~
### Aliases: MAX_CLUSTERS CLUSTER_NAMES CRITERION_NAMES DISTANCE_METHODS
###   CHOICE_STYLE
### Keywords: datasets

### ** Examples

### Maximum number of clusters that kml can deal with
MAX_CLUSTERS

### Names of the field that save clusters in object 'ClusterLongData'
cat(CLUSTER_NAMES,"\n")

### List of the available criterion
CRITERION_NAMES

### Distance available
DISTANCE_METHODS[2]

### Define the style use by choice
CHOICE_STYLE[['typeTraj']][2]



cleanEx()
nameEx("ListPartition-class")
### * ListPartition-class

flush(stderr()); flush(stdout())

### Name: ListPartition-class
### Title: ~ Class: ListPartition ~
### Aliases: ListPartition ListPartition-class [,ListPartition-method
###   [<-,ListPartition-method show,ListPartition-method listPartition
###   listPartition-method
### Keywords: classes cluster ts

### ** Examples

##############
### Preparing data
data <- gald(30)["traj"]

### Some clustering
part2 <- partition(rep(c("A","B","A"),time=40),data)
part3 <- partition(rep(c("A","B","C"),time=40),data)
part3b <- partition(rep(c("A","B","C","B","C"),time=24),data)
part4 <- partition(rep(c("A","B","A","C","D"),time=24),data)


################
### ListPartition
listPart <- listPartition()
listPart['criterionActif'] <-"Davies.Bouldin"
plotCriterion(listPart)

listPart["add"] <- part2
listPart["add"] <- part3
listPart["add"] <- part3b
listPart["add"] <- part4
listPart["add"] <- part4
listPart["add"] <- part3
listPart["add"] <- part3b

plotCriterion(listPart)
ordered(listPart)
plotCriterion(listPart)
regroup(listPart)
plotCriterion(listPart)




cleanEx()
nameEx("LongData-class")
### * LongData-class

flush(stderr()); flush(stdout())

### Name: LongData-class
### Title: ~ Class: LongData ~
### Aliases: LongData LongData-class [,LongData-method [<-,LongData-method
###   show,LongData-method
### Keywords: classes classif cluster ts

### ** Examples

#################
### building trajectory (longData)
mat <- matrix(c(NA,2,3,4,1,6,2,5,1,3,8,10),4)
ld <- longData(mat,idAll=c("I1","I2","I3","I4"),time=c(2,4,8),varNames="Age")

### '[' and '[<-'
ld["idAll"]
ld["idFewNA"]
ld["varNames"]
ld["traj"]
(ld)

### Plot
plot(ld)


#################
### building join trajectories (longData3d)
dn <- data.frame(id=1:3,v1=c(11,14,16),t1=c(1,5,7),v2=c(12,10,13),t2=c(2,5,0),t3=c(3,6,8))
(ld <- longData3d(dn,timeInData=list(Vir=c(2,4,NA),Tes=c(3,5,6))))

### Scaling
scale(ld)
(ld)

### Plotting
plot(ld)
plot3d(ld)
restaureRealData(ld)



cleanEx()
nameEx("LongData3d-class")
### * LongData3d-class

flush(stderr()); flush(stdout())

### Name: LongData3d-class
### Title: ~ Class: LongData3d ~
### Aliases: LongData3d LongData3d-class [,LongData3d-method
###   [<-,LongData3d-method show,LongData3d-method
### Keywords: classes classif cluster ts

### ** Examples

#################
### building joint trajectories

dn <- data.frame(id=1:3,v1=c(11,14,16),t1=c(1,5,7),v2=c(12,10,13),t2=c(2,5,0),t3=c(3,6,8))
(ld <- longData3d(dn,timeInData=list(Vir=c(2,4,NA),Tes=c(3,5,6))))

### Scaling
scale(ld)
(ld)

### Plotting
plot(ld)
plot3d(ld)
restaureRealData(ld)



cleanEx()
nameEx("ParLongData-class")
### * ParLongData-class

flush(stderr()); flush(stdout())

### Name: ParLongData-class
### Title: ~ Class: ParLongData ~
### Aliases: ParLongData ParLongData-class [,ParLongData-method
###   [<-,ParLongData-method
### Keywords: classes cluster

### ** Examples

   ### Building ParLongData
   parMyData <- parLongData(type="n",col=3,pch="1",pchPeriod=20,cex=1,xlab="Time",ylab="Size")

   ### Get
   parMyData['col']

   ### Set
   parMyData['cex'] <- 3
   (parMyData)



cleanEx()
nameEx("ParWindows-class")
### * ParWindows-class

flush(stderr()); flush(stdout())

### Name: ParWindows-class
### Title: ~ Class: ParWindows ~
### Aliases: ParWindows ParWindows-class [,ParWindows-method
###   [<-,ParWindows-method
### Keywords: classes classif cluster ts

### ** Examples

### Building ParWindows
(paramWin <- parWindows(3,2,FALSE,TRUE))

### Get
figsScreen <- paramWin['screenMatrix']

### Usage
listScreen <- split.screen(figsScreen)
screen(listScreen[1])
plot(-5:5/10,2.5-(-5:5)^2/20,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20,ylim=c(0,6),type="l",lwd=3)

screen(listScreen[3])
plot(-5:5/10,2.5-(-5:5)^2/20,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20,ylim=c(0,6),type="l",lwd=3)

screen(listScreen[5])
plot(-5:5/10,(-5:5)^2/10,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20+1.25,ylim=c(0,6),type="l",lwd=3)
close.screen(all.screens=TRUE)

### Sorry for that...



cleanEx()
nameEx("Partition-class")
### * Partition-class

flush(stderr()); flush(stdout())

### Name: Partition-class
### Title: ~ Class: Partition ~
### Aliases: Partition-class [,Partition-method [<-,Partition-method
###   show,Partition-method
### Keywords: classes cluster

### ** Examples

############
### Building Partition

### number
part <- partition(rep(c(1,2,1,3),time=3))

### LETTERS
part <- partition(rep(c("A","B","D"),time=4),details=c(convergenceTime="3",multiplicity="1"))

### Others don't work
try(partition(rep(c("A","Bb","C"),time=3)))

#############
### Setteur and Getteur

### '['
part["clusters"]
part["clustersAsInteger"]
part["nbClusters"]

### '[<-'
part["multiplicity"] <- 2
(part)



cleanEx()
nameEx("distFrechet")
### * distFrechet

flush(stderr()); flush(stdout())

### Name: distFrechet
### Title: ~ Function: Frechet distance ~
### Aliases: distFrechet

### ** Examples

   P <- rnorm(7)
   Q <- rnorm(6)

   ### Function from Eiter and Mannila compiled in C
   distFrechet(P,Q)

   ### Frechet using sum instead of max.
   distFrechet(P,Q,method="sum")

   ### Frechet using "manhattan" distance
   distFrechet(P,Q,Fdist=function(x)dist(x,method="manhattan"))



cleanEx()
nameEx("distTraj")
### * distTraj

flush(stderr()); flush(stdout())

### Name: distTraj
### Title: ~ Function: distance for trajectories ~
### Aliases: distTraj

### ** Examples

    x <- -1+rnorm(25);x[floor(runif(5,1,26))] <- NA
    y <- 1+rnorm(25);y[floor(runif(5,1,26))] <- NA

#    plot(x,type="b",col=2,ylim=c(-5,5))
 #   lines(y,type="b",col=3)

    system.time(for(i in 1:10000)dist(rbind(x,y)))
    system.time(for(i in 1:10000)distTraj(x,y))

    system.time(for(i in 1:10000)dist(rbind(x,y),method="maximum"))
    system.time(for(i in 1:10000)distTraj(x,y,method="maximum"))

    system.time(for(i in 1:10000)dist(rbind(x,y),method="manhattan"))
    system.time(for(i in 1:10000)distTraj(x,y,method="manhattan"))



cleanEx()
nameEx("expandParLongData")
### * expandParLongData

flush(stderr()); flush(stdout())

### Name: expandParLongData
### Title: ~ Function: expandParLongData ~
### Aliases: expandParLongData expandParLongData,ParLongData,Partition
###   expandParLongData,ParLongData,Partition-method
###   expandParLongData,ParLongData,numeric
###   expandParLongData,ParLongData,numeric-method

### ** Examples

###################
### Some parameters for trajectories
(paramTraj <- parTRAJ(col="clusters"))

### Expand to a small partition with 3 clusters
part <- partition(LETTERS[rep(1:3,4)])
expandParLongData(paramTraj,part)


###################
### Some parameters for the mean trajectories
paramMean <- parMEAN()

### If there is 3 clusters :
expandParLongData(paramMean,3)

### If there is 5 clusters :
expandParLongData(paramMean,5)



cleanEx()
nameEx("generateArtificialLongData")
### * generateArtificialLongData

flush(stderr()); flush(stdout())

### Name: generateArtificialLongData
### Title: ~ Function: generateArtificialLongData (or gald) ~
### Aliases: gald generateArtificialLongData
### Keywords: datagen cluster ts

### ** Examples

par(ask=TRUE)


#####################
### Default example

(ex1 <- generateArtificialLongData())
plot(ex1)
part1 <- partition(rep(1:4,each=50))
plot(ex1,part1)


#####################
### Three diverging lines

ex2 <- generateArtificialLongData(functionClusters=list(function(t)0,function(t)-t,function(t)t))
part2 <- partition(rep(1:3,each=50))
plot(ex2,part2)


#####################
### Three diverging lines with high variance, unbalance groups and missing value

ex3 <- generateArtificialLongData(
   functionClusters=list(function(t)0,function(t)-t,function(t)t),
   nbEachClusters=c(100,30,10),
   functionNoise=function(t){rnorm(1,0,3)},
   percentOfMissing=c(0.25,0.5,0.25)
)
part3 <- partition(rep(1:3,c(100,30,10)))
plot(ex3,part3)


#####################
### Four strange functions

ex4 <- generateArtificialLongData(
    nbEachClusters=c(300,200,100,100),
    functionClusters=list(function(t){-10+2*t},function(t){-0.6*t^2+6*t-7.5},function(t){10*sin(t)},function(t){30*dnorm(t,2,1.5)}),
    functionNoise=function(t){rnorm(1,0,3)},
    time=0:10,decimal=2,percentOfMissing=0.3)
part4 <- partition(rep(1:4,c(300,200,100,100)))
plot(ex4,part4)


#####################
### To get only longData (if you want some artificial longData
###    to deal with another algorithm), use the getteur ["traj"]

ex5 <- gald(nbEachCluster=3,time=1:3)
ex5["traj"]

par(ask=FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("generateArtificialLongData3d")
### * generateArtificialLongData3d

flush(stderr()); flush(stdout())

### Name: generateArtificialLongData3d
### Title: ~ Function: generateArtificialLongData3d (or gald3d) ~
### Aliases: gald3d generateArtificialLongData3d
### Keywords: datagen cluster ts

### ** Examples

#####################
### Default example

ex1 <- generateArtificialLongData3d()
plot3d(ex1)
part1 <- partition(rep(1:3,each=50))
plot3d(ex1,part1)


#####################
### 4 lines with unbalanced groups

ex2 <- generateArtificialLongData3d(
  nbEachClusters=c(5,10,20,40),
  functionClusters=list(
     function(t)c(t,t^3/100),
     function(t)c(0,t),
     function(t)c(t,t),
     function(t)c(0,t^3/100)
  ),
  functionNoise = function(t){c(rnorm(1,0,1),rnorm(1,0,1))}
)
plot3d(ex2)
part2 <- partition(rep(1:4,time=c(5,10,20,40)))
plot3d(ex2,part2)



cleanEx()
nameEx("imputation")
### * imputation

flush(stderr()); flush(stdout())

### Name: imputation
### Title: ~ Function: imputation ~
### Aliases: imputation imputation,matrix-method imputation,array-method
###   imputation,LongData-method imputation,LongData3d-method
### Keywords: package cluster ts NA methods

### ** Examples

##################
### Preparation of the data
par(ask=TRUE)
timeV <- 1:14

matMissing <- matrix(
    c(NA  ,NA  ,NA  ,18  ,22  ,NA  ,NA  ,NA  ,NA  , 24  , 22  , NA  , NA  , NA,
      24  ,21  ,24  ,26  ,27  ,32  ,30  ,22  ,26  , 26  , 28  , 24  , 23  , 21,
      14  ,13  , 10 , 8  , 7  ,18  ,16  , 8  ,12  ,  6  ,  10 ,  10 ,  9  ,  7,
       3  ,1   , 1  , 1  ,  3,9   , 7  , -1 , 3   ,  2   ,  4 ,  1  ,  0  , -2
   ),4,byrow=TRUE
)


matplot(t(matMissing),col=c(2,1,1,1),lty=1,type="l",lwd=c(3,1,1,1),pch=16, xlab="Black=trajectories; Green=mean trajectory\nRed=trajectory to impute",ylab="",main="Four trajectories")
moy <- apply(matMissing,2,mean,na.rm=TRUE)
lines(moy,col=3,lwd=3)

 # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Illustration of the different imputing method   #
 #           The best are at end  !!!              #
  # # # # # # # # # # # # # # # # # # # # # # # # #



##################
### Methods using cross sectionnal information (cross-methods)

par(mfrow=c(1,3))
mat2 <- matrix(c(
  NA, 9, 8, 8, 7, 6,NA,
   7, 6,NA,NA,NA, 4,5,
   3, 4, 3,NA,NA, 2,3,
  NA,NA, 1,NA,NA, 1,1),4,7,byrow=TRUE)

### crossMean
matplot(t(imputation(mat2,"crossMean")),type="l",ylim=c(0,10),lty=1,col=1,main="crossMean")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### crossMedian
matplot(t(imputation(mat2,"crossMedian")),type="l",ylim=c(0,10),lty=1,col=1,main="crossMedian")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### crossHotDeck
matplot(t(imputation(mat2,"crossHotDeck")),type="l",ylim=c(0,10),lty=1,col=1,main="crossHotDeck")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)



##################
### Methods using trajectory information (traj-methods)

par(mfrow=c(2,3))
mat1 <- matrix(c(NA,NA,3,8,NA,NA,2,2,1,NA,NA),1,11)

### locf
matplot(t(imputation(mat1,"locf")),type="l",ylim=c(0,10),main="locf\n DO NOT USE, BAD METHOD !!!")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### nocb
matplot(t(imputation(mat1,"nocb")),type="l",ylim=c(0,10),main="nocb\n DO NOT USE, BAD METHOD !!!")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### trajMean
matplot(t(imputation(mat1,"trajMean")),type="l",ylim=c(0,10),main="trajMean")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### trajMedian
matplot(t(imputation(mat1,"trajMedian")),type="l",ylim=c(0,10),main="trajMedian")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### trajHotDeck
matplot(t(imputation(mat1,"trajHotDeck")),type="l",ylim=c(0,10),main="trajHotDeck 1")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### spline
matplot(t(imputation(mat1,"spline",lowerBound=NA,upperBound=NA)),type="l",ylim=c(-10,10),main="spline")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)





##################
### Different linear interpolation

par(mfrow=c(2,2))

### linearInterpol.locf
matplot(t(imputation(mat1,"linearInterpol.locf",NA,NA)),type="l",ylim=c(-5,10),lty=1,col=1,main="linearInterpol.locf")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16,lty=1)

### linearInterpol.global
matplot(t(imputation(mat1,"linearInterpol.global",NA,NA)),type="l",ylim=c(-5,10),lty=1,col=1,main="linearInterpol.global")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16,lty=1)

### linearInterpol.local
matplot(t(imputation(mat1,"linearInterpol.local",NA,NA)),type="l",ylim=c(-5,10),lty=1,col=1,main="linearInterpol.local")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16,lty=1)

### linearInterpol.bisector
matplot(t(imputation(mat1,"linearInterpol.bisector",NA,NA)),type="l",ylim=c(-5,10),lty=1,col=1,main="linearInterpol.bisector")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16,lty=1)



##################
### Copy mean

mat3 <- matrix(c(
  NA, 9, 8, 8, 7, 6,NA,
   7, 6,NA,NA,NA, 4,5,
   3, 4, 3,NA,NA, 2,3,
  NA,NA, 1,NA,NA, 1,1),4,7,byrow=TRUE)


par(mfrow=c(2,2))

### copyMean.locf
matplot(t(imputation(mat2,"copyMean.locf",NA,NA)),type="l",ylim=c(-5,10),lty=1,col=1,main="copyMean.locf")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### copyMean.global
matplot(t(imputation(mat2,"copyMean.global",NA,NA)),type="l",ylim=c(-5,10),lty=1,col=1,main="copyMean.global")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### copyMean.local
matplot(t(imputation(mat2,"copyMean.local",NA,NA)),type="l",ylim=c(-5,10),lty=1,col=1,main="copyMean.local")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### copyMean.bisector
matplot(t(imputation(mat2,"copyMean.bisector",NA,NA)),type="l",ylim=c(-5,10),lty=1,col=1,main="copyMean.bisector")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)




### crossMean
matImp <- imputation(matMissing,method="crossMean")
matplot(t(matImp),col=c(2,1,1,1),lty=c(2,1,1,1),type="l",lwd=c(2,1,1,1),pch=16, xlab="Dotted red=imputed trajectory\nFull red=trajectory to impute",ylab="",main="Method 'crossMean'")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)


### crossMedian
matImp <- imputation(matMissing,method="crossMedian")
matplot(t(matImp),col=c(2,1,1,1),lty=c(2,1,1,1),type="l",lwd=c(2,1,1,1),pch=16, xlab="Dotted red=imputed trajectory\nFull red=trajectory to impute",ylab="",main="Method 'crossMedian'")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### crossHotDeck
matImp <- imputation(matMissing,method="crossHotDeck")
matplot(t(matImp),col=c(2,1,1,1),lty=c(2,1,1,1),type="l",lwd=c(2,1,1,1),pch=16, xlab="Dotted red=imputed trajectory\nFull red=trajectory to impute",ylab="",main="Method 'crossHotDeck'")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)


##################
### Method using trajectory

par(mfrow=c(2,3))
### trajMean
matImp <- imputation(matMissing,method="trajMean")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="nocb")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### trajMedian
matImp <- imputation(matMissing,method="trajMedian")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="nocb")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### trajHotDeck
matImp <- imputation(matMissing,method="trajHotDeck")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="nocb")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### locf
matImp <- imputation(matMissing,method="locf")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="locf")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### nocb
matImp <- imputation(matMissing,method="nocb")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="nocb")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

par(mfrow=c(2,2))

### linearInterpol.locf
matImp <- imputation(matMissing,method="linearInterpol.locf")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### linearInterpol.local
matImp <- imputation(matMissing,method="linearInterpol.local")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### linearInterpol.global
matImp <- imputation(matMissing,method="linearInterpol.global")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### linearInterpol.bisector
matImp <- imputation(matMissing,method="linearInterpol.bisector")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)


par(mfrow=c(2,2))

### copyMean.locf
matImp <- imputation(matMissing,method="copyMean.locf")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)
lines(timeV,moy,col=3,type="o",lwd=3)

### copyMean.local
matImp <- imputation(matMissing,method="copyMean.local")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)
lines(timeV,moy,col=3,type="o",lwd=3)

### copyMean.global
matImp <- imputation(matMissing,method="copyMean.global")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)
lines(timeV,moy,col=3,type="o",lwd=3)

### copyMean.bisector
matImp <- imputation(matMissing,method="copyMean.bisector")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)
lines(timeV,moy,col=3,type="o",lwd=3)

par(ask=FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("initializePartition")
### * initializePartition

flush(stderr()); flush(stdout())

### Name: initializePartition
### Title: ~ Function: initializePartition ~
### Aliases: initializePartition
###   initializePartition,numeric,numeric,character,ANY
###   initializePartition,numeric,numeric,character,array
###   initializePartition,numeric,numeric,character,ANY-method
###   initializePartition,numeric,numeric,character,array-method

### ** Examples

par(ask=TRUE)
###################
### Constrution of some longitudinal data
dn <- gald()
plot(dn)

###################
### partition using randamAll
pa1a <- initializePartition(3,lengthPart=200,method="randomAll")
plot(dn,partition(pa1a),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))
pa1b <- initializePartition(3,lengthPart=200,method="randomAll")
plot(dn,partition(pa1b),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))

###################
### partition using randamK
pa2a <- initializePartition(3,lengthPart=200,method="randomK")
plot(dn,partition(pa2a),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))
pa2b <- initializePartition(3,lengthPart=200,method="randomK")
plot(dn,partition(pa2b),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))

###################
### partition using maxDist
pa3 <- initializePartition(3,lengthPart=200,method="maxDist",data=dn["traj"])
plot(dn,partition(pa3),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))
### maxDist is deterministic, so no need for a second example


###################
### Example to illustrate "maxDist" method on classical clusters
point <- matrix(c(0,0, 0,1, -1,0, 0,-1, 1,0),5,byrow=TRUE)
points <- rbind(point,t(t(point)+c(10,0)),t(t(point)+c(5,6)))
points <- rbind(points,t(t(points)+c(30,0)),t(t(points)+c(15,20)),t(-t(point)+c(20,10)))
plot(points,main="Some points")

paInit <- initializePartition(2,nrow(points),method="maxDist",points)
plot(points,main="Two farest points")
lines(points[!is.na(paInit),],col=2,type="p",pch=16)

paInit <- initializePartition(3,nrow(points),method="maxDist",points)
plot(points,main="Three farest points")
lines(points[!is.na(paInit),],col=2,type="p",pch=16)

paInit <- initializePartition(4,nrow(points),method="maxDist",points)
plot(points, main="Four farest points")
lines(points[!is.na(paInit),],col=2,type="p",pch=16)

par(ask=FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("longData")
### * longData

flush(stderr()); flush(stdout())

### Name: longData
### Title: ~ Function: longData ~
### Aliases: longData longData,ANY,ANY,ANY,ANY,ANY,ANY-method
###   longData,missing,missing,missing,missing,missing,missing-method
### Keywords: package cluster methods

### ** Examples

#####################
### From matrix

### Small data
mat <- matrix(c(1,NA,3,2,3,6,1,8,10),3,3,dimnames=list(c(101,102,104),c("T2","T4","T8")))
longData(mat)
(ld1 <- longData(traj=mat,idAll=as.character(c(101,102,104)),time=c(2,4,8),varNames="V"))
plot(ld1)

### Big data
mat <- matrix(runif(1051*325),1051,325)
(ld2 <- longData(traj=mat,idAll=paste("I-",1:1051,sep=""),time=(1:325)+0.5,varNames="Random"))

####################
### From data.frame

dn <- data.frame(id=1:3,v1=c(NA,2,1),v2=c(NA,1,0),v3=c(3,2,2),v4=c(4,2,NA))

### Basic
longData(dn)

### Selecting some times
(ld3 <- longData(dn,timeInData=c(1,2,4),varNames=c("Hyp")))

### Excluding trajectories with more than 1 NA
(ld3 <- longData(dn,maxNA=1))



cleanEx()
nameEx("longData3d")
### * longData3d

flush(stderr()); flush(stdout())

### Name: longData3d
### Title: ~ Function: longData3d ~
### Aliases: longData3d longData3d,ANY,ANY,ANY,ANY,ANY,ANY-method
###   longData3d,missing,missing,missing,missing,missing,missing-method
### Keywords: package cluster methods

### ** Examples

#################
### From array

### Small data
mat <- array(c(1,NA,3,2,3,6,1,8,10,1,NA,1,2,NA,3,2,3,2),dim=c(3,3,2))
longData3d(mat)
(ld1 <- longData3d(mat,varNames=c("Hyp","Col"),idAll=c("i101","i104","i105")))
plot3d(ld1)

### Big data
mat <- array(c(runif(1051*325),rnorm(1051*325),rexp(1051*325)),c(1051,325,3))
(ld2 <- longData3d(traj=mat,time=(1:325)+0.5,varNames=c("unif","norm","rexp")))
plot3d(ld2,nbSample=200)

#################
### From data.frame

dn <- data.frame(id=1:3,v1=c(2,2,1),t1=c(20,21,22),v1=c(3,2,2),t2=c(23,20,28),t3=c(25,24,29))
longData3d(dn,timeInData=list(c(2,4),c(3,5)),varNames=c("V","T"))
(ld3 <- longData3d(dn,timeInData=list(V=c(2,4,NA),T=c(3,5,6))))
plot3d(ld3)



cleanEx()
nameEx("longDataFrom3d")
### * longDataFrom3d

flush(stderr()); flush(stdout())

### Name: longDataFrom3d
### Title: ~ Function: longDataFrom3d ~
### Aliases: longDataFrom3d
### Keywords: package cluster methods

### ** Examples

### Creation of joint-trajectories
mat <- array(c(1,NA,3,2,3,6,1,8,10,1,NA,1,2,NA,3,2,3,2),dim=c(3,3,2))
(ldJoint <- longData3d(mat,varNames=c("Hyp","Som")))

### Extraction of the first variable-trajectory
(ldHyp <- longDataFrom3d(ldJoint,variable="Hyp"))

### Extraction of the second variable-trajectory
(ldSom <- longDataFrom3d(ldJoint,variable="Som"))

### Extraction of the second variable-trajectory, using number
(ldSom <- longDataFrom3d(ldJoint,variable=2))



cleanEx()
nameEx("longDataTo3d")
### * longDataTo3d

flush(stderr()); flush(stdout())

### Name: longDataTo3d
### Title: ~ Function: longDataTo3d ~
### Aliases: longDataTo3d
### Keywords: package cluster methods

### ** Examples

### Creation of single variable-trajectory
mat <- matrix(c(1,NA,3,2,3,6,1,8,10,1,NA,1,2,NA,3,2,3,2),6,3)
(ldSingle <- longData(mat))

### Extension to joint trajectories
(ldHyp <- longDataTo3d(ldSingle))



cleanEx()
nameEx("longitudinalData-package")
### * longitudinalData-package

flush(stderr()); flush(stdout())

### Name: longitudinalData-package
### Title: ~ Package overview: longitudinalData ~
### Aliases: longitudinalData longitudinalData-package
### Keywords: package dplot classif cluster

### ** Examples

### Generation of artificial longData
data <- gald3d(percentOfMissing=0.3)
part <- partition(rep(1:3,each=50))
plot3d(data,part)

### Imputation
data1 <- imputation(data,method="copyMean")

### Quality criterion
# qualityCriterion(data,part)



cleanEx()
nameEx("makeLatexFile")
### * makeLatexFile

flush(stderr()); flush(stdout())

### Name: makeLatexFile
### Title: ~ Function: makeLatexFile ~
### Aliases: makeLatexFile

### ** Examples

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



cleanEx()
nameEx("ordered")
### * ordered

flush(stderr()); flush(stdout())

### Name: ordered(ListPartition)
### Title: ~ Function: ordered(ListPartition) ~
### Aliases: ordered ordered,ListPartition ordered,ListPartition-method
### Keywords: methods

### ** Examples

##############
### Preparing data

data <- gald(30)["traj"]

### Some clustering
part2 <- partition(rep(c("A","B","A"),time=40),data)
part3 <- partition(rep(c("A","B","C"),time=40),data)
part3b <- partition(rep(c("A","B","C","B","C"),time=24),data)
part4 <- partition(rep(c("A","B","A","C","D"),time=24),data)


################
### ListPartition
listPart <- listPartition()
listPart['criterionActif'] <-"Davies.Bouldin"
plotCriterion(listPart)

listPart["add"] <- part2
listPart["add"] <- part3
listPart["add"] <- part3b
listPart["add"] <- part4
listPart["add"] <- part4
listPart["add"] <- part3
listPart["add"] <- part3b

plotCriterion(listPart)
ordered(listPart)
plotCriterion(listPart)

listPart['criterionActif'] <-"Calinski.Harabatz"
plotCriterion(listPart)
ordered(listPart)
plotCriterion(listPart)





cleanEx()
nameEx("parLongData")
### * parLongData

flush(stderr()); flush(stdout())

### Name: parLongData
### Title: ~ Functions: parLongData, parTraj and parMean~
### Aliases: parLongData parTRAJ parMEAN
### Keywords: methods

### ** Examples

##################
### Construction of LongData

time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)%%3-1) * t
g <- function(id,t)(id%%2+1)*t
ld2 <- longData3d(array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(120*8*2,0,3),dim=c(120,8,2)))

### Example with default value
plot(ld2)
plot(ld2,parTraj=parTRAJ())

### Example with default values for mean trajectories
plot(ld2,parTraj=parMEAN())

### Example with default value except for the color
plot(ld2,parTraj=parTRAJ(col="blue"))



cleanEx()
nameEx("parWindows")
### * parWindows

flush(stderr()); flush(stdout())

### Name: parWindows
### Title: ~ Function: parWindows ~
### Aliases: parWindows

### ** Examples

### Building ParWindows
(paramWin <- parWindows(3,2,FALSE,TRUE))

### Get
figsScreen <- paramWin['screenMatrix']

### Usage
listScreen <- split.screen(figsScreen)
screen(listScreen[1])
plot(-5:5/10,2.5-(-5:5)^2/20,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20,ylim=c(0,6),type="l",lwd=3)

screen(listScreen[3])
plot(-5:5/10,2.5-(-5:5)^2/20,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20,ylim=c(0,6),type="l",lwd=3)

screen(listScreen[5])
plot(-5:5/10,(-5:5)^2/10,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20+1.25,ylim=c(0,6),type="l",lwd=3)
close.screen(all.screens=TRUE)

### :-)



cleanEx()
nameEx("partition")
### * partition

flush(stderr()); flush(stdout())

### Name: partition
### Title: ~ Function: partition ~
### Aliases: partition partition,ANY,missing,ANY-method
###   partition,ANY,array,ANY-method partition,ANY,matrix,ANY-method
###   partition,ANY,LongData,ANY-method partition,ANY,LongData3d,ANY-method
###   partition,missing,missing,missing-method
### Keywords: cluster methods

### ** Examples

### Empty partition
partition()

### Small partition
partition(clusters=c("A","B","A","C","C"))

### Random partition
partition(clusters=LETTERS[floor(runif(100,1,5))])

### Partition that clusters correctly some data
###   Quality criterion are high
data <- gald()["traj"]
partition(clusters=rep(1:4,each=50),data)

### Partition that does not cluster correctly the data
###   Quality criterion are low
partition(clusters=rep(1:4,50),data)



cleanEx()
nameEx("pathFrechet")
### * pathFrechet

flush(stderr()); flush(stdout())

### Name: pathFrechet
### Title: ~ Function: Frechet distance ~
### Aliases: pathFrechet

### ** Examples

   P <- rnorm(7)
   Q <- rnorm(6)

   ### Function compiled in C
   pathFrechet(P,Q)

   ### Frechet using sum instead of max.
   pathFrechet(P,Q,method="sum")

   ### Frechet using "manhattan" distance
   pathFrechet(P,Q,Fdist=function(x)dist(x,method="manhattan"))



cleanEx()
nameEx("plot")
### * plot

flush(stderr()); flush(stdout())

### Name: plot,LongData
### Title: ~ function: plot for LongData or LongData3d~
### Aliases: plot plot,LongData plot,LongData-method
###   plot,LongData,missing-method plot,LongData,Partition-method
###   plot,LongData3d plot,LongData3d-method plot,LongData3d,missing-method
###   plot,LongData3d,Partition-method
### Keywords: package aplot

### ** Examples

##################
### Construction of the data

ld <- gald3d()
part <- partition(rep(1:3,each=50))

### Basic plotting
plot(ld)
plot(ld,part)

### Change the windows orientation
plot(ld,parWin=windowsCut(c(1,2),addLegend=FALSE))


##################
### Changing graphical parameters 'par'

### No letters on the mean trajectories
plot(ld,part,parMean=parMEAN(type="l"))

### Only one letter on the mean trajectories
plot(ld,part,parMean=parMEAN(pchPeriod=Inf))

### Color individual according to its clusters (col="clusters")
plot(ld,part,parTraj=parTRAJ(col="clusters"))

### Mean without individual
plot(ld,part,parTraj=parTRAJ(type="n"))


### No mean trajectories (type="n")
### Color individual according to its clusters (col="clusters")
plot(ld,part,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"))

### Only few trajectories
plot(ld,part,nbSample=10,parTraj=parTRAJ(col='clusters'),parMean=parMEAN(type="n"))

##################
### single variable trajectory

ld2 <- gald()
part2 <- partition(rep(1:4,each=50))
plot(ld2)
plot(ld2,part2)



cleanEx()
nameEx("plot3d")
### * plot3d

flush(stderr()); flush(stdout())

### Name: plot3d,LongData
### Title: ~ Function: plot3d for LongData3d ~
### Aliases: plot3d plot3d,LongData3d plot3d,LongData3d-method
###   plot3d,LongData3d,Partition-method plot3d,LongData3d,missing-method
### Keywords: package ts aplot

### ** Examples


##################
### Construction of the data

time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)%%3-1) * t
g <- function(id,t)(id%%2+1)*t
h <- function(id,t)(id%%4-0.5)*(20-t)
ld <- longData3d(array(cbind(outer(id2,time,f),outer(id2,time,g),outer(id2,time,h))+rnorm(120*8*3,0,3),dim=c(120,8,3)))
part <- partition(rep(1:6,20))

### Basic plotting
plot3d(ld)
plot3d(ld,part)

### Variable 1 and 3, then 2 and 3
plot3d(ld,part)
plot3d(ld,part,varY=3,varZ=2)
plot3d(ld,part,varY=1,varZ=3)

##################
### Changing graphical parameters 'par'

### Color individual according to its clusters (col="clusters")
plot3d(ld,part,parTraj=parTRAJ(col="clusters"))
plot3d(ld,part,parTraj=parTRAJ(col="clusters"),varY=1,varZ=3)

### No mean trajectories (type="n"), only few trajectories
### Color individual according to its clusters (col="clusters")
plot3d(ld,part,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"),nbSample=10)



cleanEx()
nameEx("plot3dPdf")
### * plot3dPdf

flush(stderr()); flush(stdout())

### Name: plot3dPdf
### Title: ~ Function: plot3dPdf for LongData ~
### Aliases: plot3dPdf plot3dPdf,LongData3d-method
###   plot3dPdf,LongData3d,missing-method
###   plot3dPdf,LongData3d,numeric-method
###   plot3dPdf,LongData3d,Partition-method

### ** Examples

  ### Generating the data
  myLd <- gald3d()
  part <- partition(rep(1:3,each=50))
  plot3d(myLd,part)

  ### Creation of the scene
  scene <- plot3dPdf(myLd,part)
  drawScene.rgl(scene)

  ### Export in '.asy' file
  saveTrianglesAsASY(scene)

  ### Creation of a '.prc' file
  # Open a console, then run:
  # 'asy -inlineimage -tex pdflatex scene.asy'

  ### Creation of the LaTeX main document
  makeLatexFile()

  ### Creation of the '.pdf'
  # Open a console window, then run
  # pdfLatex main.tex



cleanEx()
nameEx("plotCriterion")
### * plotCriterion

flush(stderr()); flush(stdout())

### Name: plotCriterion
### Title: ~ Function: plotCriterion ~
### Aliases: plotCriterion plotCriterion-method plotCriterion,ListPartition
###   plotCriterion,ListPartition-method

### ** Examples

###############
### Data génération

data <- gald(30)["traj"]

### Some clustering
listPart <- listPartition()
listPart["add"] <- partition(rep(c("A","B","A"),time=40),data)
listPart["add"] <- partition(rep(c("A","B","B"),time=40),data)
listPart["add"] <- partition(rep(c("A","B","C"),time=40),data)
listPart["add"] <- partition(rep(c("A","B","C","B","C"),time=24),data)
listPart["add"] <- partition(rep(c("A","B","A","C","D"),time=24),data)
ordered(listPart)

################
### graphical display
plotCriterion(listPart)
plotCriterion(listPart,criterion=CRITERION_NAMES)



cleanEx()
nameEx("qualityCriterion")
### * qualityCriterion

flush(stderr()); flush(stdout())

### Name: qualityCriterion
### Title: ~ Function: qualityCriterion ~
### Aliases: qualityCriterion qualityCriterion,matrix,ANY-method
###   qualityCriterion,array,ANY-method
###   qualityCriterion,LongData,Partition-method
###   qualityCriterion,LongData3d,Partition-method
### Keywords: package cluster methods

### ** Examples

##################
### Preparation of some artificial data
par(ask=TRUE)
ld <- gald()

### Correct partition
part1 <- partition(rep(1:4,each=50))
plot(ld,part1)
(cr1 <- qualityCriterion(ld,part1))

### Random partition
part2 <- partition(floor(runif(200,1,5)))
plot(ld,part2)
(cr2 <- qualityCriterion(ld,part2))

### Partition with 3 clusters instead of 4
part3 <- partition(rep(c(1,2,3,3),each=50))
plot(ld,part3)
(cr3 <- qualityCriterion(ld,part3))


### Comparisons of the Partition
plot(c(cr1[[1]],cr2[[2]],cr3[[3]]),main="The highest give the best partition
(according to Calinski & Harabatz criterion)")
par(ask=FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("regroup")
### * regroup

flush(stderr()); flush(stdout())

### Name: regroup
### Title: ~ Function: regroup ~
### Aliases: regroup
### Keywords: classes cluster ts

### ** Examples

### Some data
data <- gald(30)["traj"]

### Some clustering
part2 <- partition(rep(c("A","B","A"),time=40),data)
part3 <- partition(rep(c("A","B","C"),time=40),data)

################
### ListPartition
listPart <- listPartition()

listPart["add"] <- part2
listPart["add"] <- part3
listPart["add"] <- part2
listPart["add"] <- part3

### Some clustering has been found several time
### regroup will suppress the duplicate one
regroup(listPart)
plotCriterion(listPart)



cleanEx()
nameEx("restaureRealData")
### * restaureRealData

flush(stderr()); flush(stdout())

### Name: restaureRealData
### Title: ~ Function: restaureRealData ~
### Aliases: restaureRealData restaureRealData,LongData
###   restaureRealData,LongData3d restaureRealData,LongData-method
###   restaureRealData,LongData3d-method

### ** Examples

##################
### Building LongData

time=c(1,2,3,4,8,12,16,20)
id2=1:12
f <- function(id,t)((id-1)%%3-1) * t
g <- function(id,t)(id%%2+1)*t
ld1 <- longData3d(array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(12*8*2,0,1),dim=c(12,8,2)))
plot3d(ld1)

##################
### Scaling by 'mean' and 'standard deviation'
scale(ld1,scale=c(-1,-1))
plot(ld1)

##################
### Back to the first version of the data
restaureRealData(ld1)
plot(ld1)



cleanEx()
nameEx("saveTrianglesAsASY")
### * saveTrianglesAsASY

flush(stderr()); flush(stdout())

### Name: saveTrianglesAsASY
### Title: ~ Function: saveTrianglesAsASY ~
### Aliases: saveTrianglesAsASY

### ** Examples

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



cleanEx()
nameEx("scale")
### * scale

flush(stderr()); flush(stdout())

### Name: scale
### Title: ~ Function: scale for LongData ~
### Aliases: scale scale,LongData scale,LongData3d scale,LongData-method
###   scale,LongData3d-method
### Keywords: method

### ** Examples

##################
### Building LongData

time=c(1,2,3,4,8,12,16,20)
id2=1:12
f <- function(id,t)((id-1)%%3-1) * t
g <- function(id,t)(id%%2+1)*t
ld1 <- longData3d(array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(12*8*2,0,1),dim=c(12,8,2)))
plot3d(ld1)

##################
### Scaling by 'mean' and 'standard deviation'
plot(ld1)
scale(ld1)
plot(ld1)

### Scaling by some parameters
scale(ld1,center=c(10,100),scale=c(3,-1))
plot(ld1)

##################
### To restaure the data
restaureRealData(ld1)



cleanEx()
nameEx("windowsCut")
### * windowsCut

flush(stderr()); flush(stdout())

### Name: windowsCut
### Title: ~ Function: windowsCut ~
### Aliases: windowsCut

### ** Examples

  ### Simple cut with no space for legent
  windowsCut(3,FALSE)
  windowsCut(4,FALSE)
  windowsCut(5,FALSE)

  ### Simple cut with legend
  windowsCut(5)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
