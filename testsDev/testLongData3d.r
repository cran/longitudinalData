#source("./testFunction.r")
source("../R/longData3d.r")
### LongData contient des trajectoires (que l'on doit analyser) et leur identifiant
### Une trajectoire est une suite de numeric.
### 'Des' trajectoires est une matrice de numeric, chaque ligne est une trajectoire.
###  - Id est l'identifiant (indispensable)
###  - time est le temps ou les mesures ont été faite.
###  - varName est le nom de la variable.
###  - traj est la matrice des trajectoires
# id       : identifiant of the individual (or lines).
# time     : real time
# varName : nom of the variable (single now, several in the futur)
# traj    : array of the longData. Dim 1 is individual, 2 is time, 3 is variable(s)


cat("\n####################################################################
########################### Test  LongData3d ###########################
############################### Creation ###############################
####################################################################\n")

### Constructeurs
cleanProg(.LongData3d.validity,,,0)

new("LongData3d")
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),time=c(2,4,8),varNames="T")
new("LongData3d",traj=array(c(1,2,3,6,8,10),dim=c(3,2,1),dimnames=list(c(101,102,104),c("t2","t4"),c("V"))),idAll=as.character(c(101,102,104)),
    idFewNA=as.character(c(101,102,104)),time=c(2,4),varNames="V",maxNA=1)

cat("\n####################################################################
########################### Test  LongData3d ###########################
############################# Creation  3D #############################
####################################################################\n")


### Constructeurs
new("LongData3d")
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),time=c(2,4,8),varNames="T")

tr1 <- tr1n <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),
            dim=c(3,5,2),
            dimnames=list(c(101,102,104),c("t1","t2","t4","t8","t16"),c("P","A"))
            )
tr1n[1,2,1] <- NA; tr1n[2,4,2] <- NA; tr1n[3,1,2] <- NA; tr1n[3,3,2] <- NA;

new("LongData3d",
    traj=tr1,
    idAll=as.character(c(100,102)),
    idFewNA=as.character(c(101,102,104)),
    time=c(1,2,4,8,16),
    varNames=c("P","A"),
    maxNA=3
    )


tr2 <- array(c(1,2,3, 1,4,3, 6,1,8, 10,1,2,
              6,1,8, 10,1,2, 1,3,2, 4,2,5,
              1,3,2, 4,2,5, 6,3,4, 3,4,4,
              4,7,6, 5,5,4,  4,7,6, 5,5,4),
            dim=c(4,3,4),
            dimnames=list(c("i1","i2","i3","i4"),c("t1","t2","t4"),c("P","A","E","R"))
            )
new("LongData3d",
    traj=tr2,
    idFewNA=c("i1","i2","i3","i4"),
    idAll=c("i1","i2","i3","i4"),
    time=c(1,2,4),
    varNames=c("P","A","E","R"),
    maxNA=2
    )


tr3n <- array(c(1,NA,NA, 1,4,3,
              NA,1,8, 10,NA,2,
              4,NA,6, NA,5,4),
            dim=c(3,3,2),
            dimnames=list(c("i1","i2","i3"),c("t1","t2","t4"),c("P","A"))
            )
new("LongData3d",
    traj=tr3n,
    idAll=c("i1","i2","i3"),
    idFewNA=c("i1","i2","i3"),
    time=c(1,2,4),
    varNames=c("P","A"),
    maxNA=2
    )


tr4n <- array(c(NA,NA,2,
               NA,NA,1,
               NA,NA,NA,
               NA,3,2,
               2,NA,1,
               1,2,1,

               3,NA,NA,
               4,NA,6,
               5,2,4,
               2,NA,NA,
               NA,4,2,
               1,NA,2),
            dim=c(3,6,2),
            dimnames=list(c("t1","t2","t4"),c(101,102,103,105,106,107),c("P","A"))
            )
tr4n <- aperm(tr4n,c(2,1,3))

new("LongData3d",
    traj=tr4n,
    idAll=as.character(c(101,102,103,105,106,107)),
    idFewNA=as.character(c(101,102,103,105,106,107)),
    time=c(1,2,4),
    varNames=c("P","A"),
    maxNA=c(1,1)
    )






cat("\n###################################################################
######################### Test  LongData3d ########################
######################### Constructeur  3D ########################
###################################################################\n")

cleanProg(.longData3d.constructor,,,1) #all

longData3d()
#longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))

longData3d(traj=tr1,idAll=as.character(c(101,102,104)),time=c(1,2,4,8,16),varNames=c("P","A"),maxNA=3)
longData3d(traj=tr2,idAll=as.character(c(1,2,3,4)),time=c(1,2,4),varNames=c("P","A","E","R"),maxNA=2)
longData3d(traj=tr3n,idAll=as.character(c(1,2,3)),time=c(1,2,4),varNames=c("P","A"),maxNA=2)
longData3d(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=2)

### Vérification de l'exclusion des manquantes
longData3d(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=1)
longData3d(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=2)
longData3d(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=c(1,1))
longData3d(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=c(2,2))
longData3d(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),maxNA=c(2,1))


### Base de données
#cleanProg(as.longData3d.data.frame,,,0)
#cleanProg(as.longData3d.array,,,0)


LD0 <- longData3d()
LD1 <- longData3d(traj=tr1,idAll=c(101,102,104),time=c(1,2,4,8,16),varNames=c("Pa","Av"),maxNA=3)
LD1n <- longData3d(traj=tr1n,idAll=c(101,102,104),time=c(1,2,4,8,16),varNames=c("Pa","Av"),maxNA=3)

data <- read.csv2("example.csv")
LD2 <- longData3d(data,timeInData=list(A=c(2,4),P=c(5,7)),time=2:3,varNames=c("Av","Pe"))
LD2 <- longData3d(data,timeInData=list(A=c(2,4),P=c(5,7)),time=2:3)
LD2 <- longData3d(data,timeInData=list(c(2,4),c(5,7)),time=2:3)
LD2 <- longData3d(data,timeInData=list(A=c(2,4),P=c(5,7)),time=2:3,varNames=c("Av","Pe"))
LD2n <- longData3d(data,timeInData=list(A=c(2,NA,4),P=c(NA,5,7)),time=2:4)
LD2 <- longData3d(data,timeInData=list(V21=c(2,3,4),V4=c(5,6,7)),time=c(11,13,14))


time=c(1,2,3,4,8,12,16,20)
id1=1:8
id2=1:200
id3=1:2000
#varNames=c("A","B")
f <- function(id,t)((id-1)%%3-1) * t
g <- function(id,t)(id%%2+1)*t

tra3 <- array(cbind(outer(id1,time,f),outer(id1,time,g))+rnorm(8*8*2,0,2),dim=c(8,8,2))
LD3 <- longData3d(tra3)
for(i in 1:40){tra3[floor(runif(1,1,9)),floor(runif(1,1,9)),floor(runif(1,1,3))]<-NA}
LD3n <- longData3d(tra3,maxNA=c(5,6))

tra4 <- array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(200*8*2,0,3),dim=c(200,8,2))
LD4 <- longData3d(tra4)
for(i in 1:640){tra4[floor(runif(1,1,200)),floor(runif(1,1,9)),floor(runif(1,1,3))]<-NA}
LD4n <- longData3d(tra4,maxNA=3)

tra5 <- array(cbind(outer(id3,time,f),outer(id3,time,g))+rnorm(2000*8*2,0,4),dim=c(2000,8,2))
LD5 <- longData3d(tra5)
for(i in 1:6400){tra5[floor(runif(1,1,1200)),floor(runif(1,1,9)),floor(runif(1,1,3))]<-NA}
LD5n <- longData3d(tra5,maxNA=4)


dn6 <- read.csv("DatasetKML.csv")[1:200,,]
LD6n <- longData3d(dn6,time=1:6,timeInData=list(cred=3:8,creq=9:14,croq=c(24:28,NA)),maxNA=5)
traj6 <- LD6n["traj"]
traj6[is.na(traj6)] <- 0
LD6 <- longData3d(traj6)

traj7 <- array(rnorm(8*5*12),dim=c(8,5,12))
LD7 <- longData3d(traj7)
for(i in 1:150){traj7[floor(runif(1,1,9)),floor(runif(1,1,6)),floor(runif(1,1,13))]<-NA}
LD7n <- longData3d(traj7)






cat("\n####################################################################
########################### Test  LongData #########################
############################# Accesseurs ###########################
####################################################################\n")


LD0["idAll"]
LD1["idAll"]
LD1n["idAll"]
LD2["idAll"]
LD2n["idAll"]
LD3["idAll"]
LD3n["idAll"]
LD4["idAll"]
LD4n["idAll"]
LD5["idAll"]
LD5n["idAll"]
LD6["idAll"]
LD6n["idAll"]
LD7["idAll"]
LD7n["idAll"]

LD0["idFewNA"]
LD1["idFewNA"]
LD1n["idFewNA"]
LD2["idFewNA"]
LD2n["idFewNA"]
LD3["idFewNA"]
LD3n["idFewNA"]
LD4["idFewNA"]
LD4n["idFewNA"]
LD5["idFewNA"]
LD5n["idFewNA"]
LD6["idFewNA"]
LD6n["idFewNA"]
LD7["idFewNA"]
LD7n["idFewNA"]

LD0["time"]
LD1["time"]
LD1n["time"]
LD2["time"]
LD2n["time"]
LD3["time"]
LD3n["time"]
LD4["time"]
LD4n["time"]
LD5["time"]
LD5n["time"]
LD6["time"]
LD6n["time"]
LD7["time"]
LD7n["time"]

LD0["varNames"]
LD1["varNames"]
LD1n["varNames"]
LD2["varNames"]
LD2n["varNames"]
LD3["varNames"]
LD3n["varNames"]
LD4["varNames"]
LD4n["varNames"]
LD5["varNames"]
LD5n["varNames"]
LD6["varNames"]
LD6n["varNames"]
LD7["varNames"]
LD7n["varNames"]

LD0["traj"]
LD1["traj"]
LD1n["traj"]
LD2["traj"]
LD2n["traj"]
LD3["traj"]
LD3n["traj"]
LD4["traj"]
LD4n["traj"]
LD5["traj"]
LD5n["traj"]
LD6["traj"]
LD6n["traj"]
LD7["traj"]
LD7n["traj"]






cat("\n###################################################################
########################## Test  LongData #########################
############################# Affichage ###########################
###################################################################\n")

cleanProg(showLongData3d)
LD1
LD4
print(LD4)




cleanProg(.longData3d.scale,,,2) # meanNA sdNA
scale(LD1)
restaureRealData(LD1)
scale(LD1,center=c(2,3))
restaureRealData(LD1)
scale(LD1,center=c(2,3),scale=c(1,1))
restaureRealData(LD1)
scale(LD1,scale=c(1,1))
scale(LD1n)
scale(LD2)
scale(LD2n)
scale(LD3)
scale(LD3n)
scale(LD4)
scale(LD4n)
scale(LD5)
scale(LD5n)
scale(LD6)
scale(LD6n)
scale(LD7)
scale(LD7n)

LD3n
LD1
LD2

cat("###  restaureRealData ###\n")
cleanProg(.longData3d.restaureRealData)
restaureRealData(LD1)
restaureRealData(LD1n)
restaureRealData(LD2)
restaureRealData(LD2n)
restaureRealData(LD3)
restaureRealData(LD3n)
restaureRealData(LD4)
restaureRealData(LD4n)
restaureRealData(LD5)
restaureRealData(LD5n)
restaureRealData(LD6)
restaureRealData(LD6n)
restaureRealData(LD7)
restaureRealData(LD7n)



#GA1 <- gald3d()
#(GA2 <- gald3d(10))

## par(mfrow=c(2,3))
## part3 <- partition(rep(LETTERS[1:3],each=50))
## part4 <- partition(rep(LETTERS[1:4],each=50))

## aldB1 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,1)})
## plot(aldB1,part4,col="clusters",type.mean="n")

## aldB3 <- generateArtificialLongData()
## plot(aldB3,part4,type.mean="b")

## aldB7 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,7)})
## plot(aldB7,part4)

## aldA3 <- generateArtificialLongData(time=0:7,nbEachClusters=c(50,50,50),
##     functionClusters=list(function(x){0},function(x){x},function(x){-x}),
##     functionNoise=function(t){rnorm(1,0,3)}
## )
## plot(aldA3,part3)

## aldC3 <- generateArtificialLongData(time=0:6,nbEachClusters=c(50,50,50),
##     functionClusters=list(function(x){2},function(x){10},function(x){12-2*x}),
##     functionNoise=function(t){rnorm(1,0,3)}
## )
## plot(aldC3,part3)

## aldD3 <- generateArtificialLongData(time=5:45,nbEachClusters=c(50,50,50,50),
##     functionClusters=list(function(x){50*dnorm(x,20,2)},function(x){50*dnorm(x,25,2)},function(x){50*dnorm(x,30,2)},function(x){30*dnorm(x,25,5)}),
##     functionNoise=function(t){rnorm(1,0,3)}
## )
## plot(aldD3,part3)


cleanProg(varNumAndName)
varNumAndName("Pa",LD1["varNames"])
varNumAndName(1,LD1["varNames"])
varNumAndName(2,LD1["varNames"])
varNumAndName("Av",LD1["varNames"])



longDataFrom3d(LD3,"V1")
longDataFrom3d(LD3,"V2")
longDataFrom3d(LD3,1)
longDataFrom3d(LD3,2)



cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++ Fin Test  LongData ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
