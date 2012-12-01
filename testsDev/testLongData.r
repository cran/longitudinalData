#source("./testFunction.r")
source("../R/longData.r")
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
######################### Test  LongData ###########################
############################ Creation ##############################
####################################################################\n")

### Constructeurs
cleanProg(.LongData.validity,,,0)

new("LongData")
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),time=c(2,4,8),varNames="T")
new("LongData",traj=matrix(c(1,2,3,6,8,10),3,2,dimnames=list(c(101,102,104),c("t2","t4"))),idAll=as.character(c(101,102,104)),
    idFewNA=as.character(c(101,102,104)),time=c(2,4),varNames="V",maxNA=1)


cat("\n###################################################################
########################## Test  LongData #########################
########################### Constructeur ##########################
###################################################################\n")


longData()
#longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
longData(traj=matrix(c(1,2,3,1,4,6,1,8,10),3,3),idAll=c(1,2,3),time=c(2,4,8),varNames="T")

### Base de données
ld0 <- longData()
ld1 <- longData(traj=matrix(c(NA,2,3,NA,4,6,NA,8,10),3),idAll=1:3,time=1:3)
ld1 <- longData(traj=matrix(c(1,2,3,1,4,6,1,8,10),3,3),idAll=c(11,12,13),time=c(2,4,8),varNames="T")
ld1 <- longData(traj=matrix(c(1,2,3,1,4,6,1,8,10),3,3),idAll=c(11,12,13),time=c(2,4,8),varNames="T")
ld1n <- longData(idAll=c(1,2,3),time=c(2,4,8),varNames="T",traj=array(c(1,NA,3,1,4,NA,NA,6,10),dim=c(3,3)))

dn2 <- data.frame(idAll=c(10,17,28,29,31),t1=c(5,4,2,1,0),t2=c(5,4,3,2,1),t4=c(4,2,3,1,1),t5=c(5,6,2,1,0))
ld2 <- longData(dn2[,-1],idAll=dn2[,1],time=c(1,2,5,6))
ld2 <- longData(dn2,time=c(1,2,5,6))
dn2[1,2]<- NA;dn2[3,3]<- NA;dn2[2,2]<- NA;dn2[2,5]<- NA;dn2[4,3]<-NA;dn2[4,4]<-NA
ld2n <- longData(dn2,time=c(1,2,5,6),maxNA=1)



data3 <- rbind(c(1,2 ,NA,4 ),
               c(1,1 ,NA,1 ),
               c(2,3 ,4 ,5 ),
               c(2,2 ,2 ,2 ),
               c(3,NA,NA,6 ),
               c(3,NA,NA,3 ),
               c(2,4 ,4 ,NA),
               c(2,3 ,2 ,NA))
ld3n <- longData(data3,timeInData=c(1,3,4))
ld3n <- longData(data3)


data3Imp <- rbind(c(1,2,3,4),
                  c(1,3,1,1),
                  c(2,3,4,5),
                  c(2,2,2,12),
                  c(3,4,4,6),
                  c(3,3,3,3),
                  c(2,4,8,5),
                  c(2,3,4,1))
ld3 <- longData(data3Imp)


dn4 <- read.csv2("divergingLines.csv")[c(1:180,101:120),]
ld4 <- longData(dn4[,-1],idAll=(1:200)*2,time=c(0,1,2,3,4,6,8,10,12,16,20))

for(i in 1:600){dn4[floor(runif(1,1,201)),floor(runif(1,2,13))]<-NA}
dn4[2,-1]<-NA
dn4[,6]<-NA
ld4n <- longData(dn4[,-1],time=c(0,1,2,3,4,6,8,10,12,16,20),idAll=(1:200)*2)



dn5 <- read.csv2("../../../Recherche/Anorexie Tamara/trajectoires de soins.csv")[rep(1:200,10),-29]
ld5 <- longData(dn5[,-1],idAll=1:2000)

for(i in 1:5000){dn5[floor(runif(1,1,2000)),floor(runif(1,2,29))]<-NA}
ld5n <- longData(dn5[,-1],idAll=1:2000)




cat("\n####################################################################
########################### Test  LongData #########################
############################# Accesseurs ###########################
####################################################################\n")

ld0["idAll"]
ld1["idAll"]
ld1n["idAll"]
ld2["idAll"]
ld2n["idAll"]
ld3["idAll"]
ld3n["idAll"]
ld4["idAll"]
ld4n["idAll"]
ld5["idAll"]
ld5n["idAll"]

ld0["idFewNA"]
ld1["idFewNA"]
ld1n["idFewNA"]
ld2["idFewNA"]
ld2n["idFewNA"]
ld3["idFewNA"]
ld3n["idFewNA"]
ld4["idFewNA"]
ld4n["idFewNA"]
ld5["idFewNA"]
ld5n["idFewNA"]

ld0["time"]
ld1["time"]
ld1n["time"]
ld2["time"]
ld2n["time"]
ld3["time"]
ld3n["time"]
ld4["time"]
ld4n["time"]
ld5["time"]
ld5n["time"]

ld0["varNames"]
ld1["varNames"]
ld1n["varNames"]
ld2["varNames"]
ld2n["varNames"]
ld3["varNames"]
ld3n["varNames"]
ld4["varNames"]
ld4n["varNames"]
ld5["varNames"]
ld5n["varNames"]


ld0["traj"]
ld1["traj"]
ld1n["traj"]
ld2["traj"]
ld2n["traj"]
ld3["traj"]
ld3n["traj"]
ld4["traj"]
ld4n["traj"]
ld5["traj"]
ld5n["traj"]


cat("\n###################################################################
########################## Test  LongData #########################
############################## Setteur ############################
###################################################################\n")



tryBug(ld1["idAll"]<-1:4)
tryBug(ld1["time"]<-1:4)
tryBug(ld1["traj"]<-array(1:12,dim=c(3,4,1)))
tryBug(ld0["idFewNA"] <- 1:2)

tryBug(ld2["time"] <- c(1,2,7,9))

#ld3n["varNames"] <- "Hospit"
#ld4["varNames"] <- "U"


#ld2["traj"] <- matrix(c(2,5,4,7,8,9,6,3,6,5,7,8,3,3,4,5,6,7,4,5),5)
#ld2n["traj"][c(T,T,F),,] <- array(c(20,NA,40,70,NA,90,60,30),c(2,4,1))



cat("\n###################################################################
########################## Test  LongData #########################
############################# Affichage ###########################
###################################################################\n")

cleanProg(showLongData)
ld1
ld4
print(ld4)




cleanProg(.longData.scale,,,2) # meanNA sdNA
scale(ld1)
restoreRealData(ld1)
scale(ld1,center=2)
restoreRealData(ld1)
scale(ld1,center=2,scale=3)
restoreRealData(ld1)
scale(ld1,scale=2)
scale(ld1n)
scale(ld2)
scale(ld2n)
scale(ld3)
scale(ld3n)
scale(ld4)
scale(ld4n)
scale(ld5)
scale(ld5n)


cat("###  restoreRealData ###\n")
cleanProg(.longData.restoreRealData)
restoreRealData(ld1)
restoreRealData(ld1n)
restoreRealData(ld2)
restoreRealData(ld2n)
restoreRealData(ld3)
restoreRealData(ld3n)
restoreRealData(ld4)
restoreRealData(ld4n)
restoreRealData(ld5)
restoreRealData(ld5n)


#cleanProg(generateArtificialLongData,,,1)
#ga1 <- gald()
#ga2 <- gald(10)

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



cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++ Fin Test  LongData ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
