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
############################ Test  LongData ############################
############################### Creation ###############################
####################################################################\n")

### Constructeurs
new("LongData")
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),time=c(2,4,8),varName="T")
new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102,104)),time=c(2,4,8),varName="T")
new("LongData",traj=array(c(1,NA,3,NA,NA,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102,104)),time=c(2,4,8),varName="T")
try(new("LongData",traj=array(c(1,NA,3,NA,NA,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102,104)),time=c(2,4,8),varName="T"))
try(new("LongData",traj=array(c(1,NA,3,NA,NA,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102,104)),time=c(2,4,8),varName="T"))
try(new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2",NA,"T8"))),id=as.character(c(101,102,104)),time=c(2,NA,8),varName="T"))
try(new("LongData",traj=array(c(1,NA,3,NA,NA,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102)),time=c(2,4,8),varName="T"))


cat("\n###################################################################
########################## Test  LongData #########################
########################### Constructeur ##########################
###################################################################\n")


longData()
#longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),id=c(1,2,3),time=c(2,4,8),varName="T")

### Base de données
ld0 <- longData()
ld1 <- longData(traj=matrix(c(NA,2,3,NA,4,6,NA,8,10),3),id=1:3,time=1:3)
ld1 <- longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),id=c(11,12,13),time=c(2,4,8),varName="T")
ld1n <- longData(id=c(1,2,3),time=c(2,4,8),varName="T",traj=array(c(1,NA,3,1,4,NA,NA,6,10),dim=c(3,3)))

dn2 <- data.frame(id=c(10,17,28),T1=c(5,4,2),T2=c(5,4,3),T4=c(4,2,3),T5=c(5,6,2))
ld2 <- as.longData(dn2,timeReal=c(1,2,5,6))
dn2[1,2]<- NA;dn2[3,3]<- NA;dn2[2,2]<- NA;dn2[2,5]<- NA
ld2n <- as.longData(dn2,timeReal=c(1,2,5,6))

dn3 <- read.csv2("../../../../Anorexie Tamara/trajectoires de soins.csv")[,-29]
ld3 <- as.longData(dn3)

for(i in 1:2400){dn3[floor(runif(1,1,244)),floor(runif(1,2,29))]<-NA}
ld3n <- as.longData(dn3)

dn4 <- read.csv2("../../divergingLines.csv")
ld4 <-as.longData(dn4,id=(1:180)*2,timeReal=c(0,1,2,3,4,6,8,10,12,16,20))

for(i in 1:600){dn4[floor(runif(1,1,181)),floor(runif(1,2,13))]<-NA}
#dn4[5,]<-NA
dn4[,6]<-NA
ld4n <-as.longData(dn4,id=(1:180)*2,timeReal=c(0,1,2,3,4,6,8,10,12,16,20))

data5 <- rbind(c(1,2 ,NA,4 ),
               c(1,1 ,NA,1 ),
               c(2,3 ,4 ,5 ),
               c(2,2 ,2 ,2 ),
               c(3,NA,NA,6 ),
               c(3,NA,NA,3 ),
               c(2,4 ,4 ,NA),
               c(2,3 ,2 ,NA))
dim(data5) <- c(8,4)
ld5n <- as.longData(as.data.frame(cbind(1:8,data5)))
data5Imp <- rbind(c(1,2,3,4),
                  c(1,1,1,1),
                  c(2,3,4,5),
                  c(2,2,2,2),
                  c(3,4,5,6),
                  c(3,3,3,3),
                  c(2,4,4,5),
                  c(2,3,2,2))
dim(data5Imp) <- c(8,4)
ld5 <- as.longData(as.data.frame(cbind(1:8,data5Imp)))



cat("\n####################################################################
########################### Test  LongData #########################
############################# Accesseurs ###########################
####################################################################\n")

ld0["id"]
ld1["id"]
ld1n["id"]
ld2["id"]
ld2n["id"]
ld3["id"]
ld3n["id"]
ld4["id"]
ld4n["id"]
ld5["id"]
ld5n["id"]

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

ld0["varName"]
ld1["varName"]
ld1n["varName"]
ld2["varName"]
ld2n["varName"]
ld3["varName"]
ld3n["varName"]
ld4["varName"]
ld4n["varName"]
ld5["varName"]
ld5n["varName"]

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

try(ld1["id"]<-1:4)
try(ld1["time"]<-1:4)
try(ld1["traj"]<-array(1:12,dim=c(3,4,1)))
try(ld0["id"] <- 1:2)

ld1["id"] <- 1:3
ld1n["id"] <- paste("E",1:3,sep="")

ld2["time"] <- c(1,2,7,9)

ld3n["varName"] <- "Hospit"
ld4["varName"] <- "U"


ld2["traj"] <- matrix(c(2,5,4,7,8,9,6,3,6,5,7,8),3)
ld2n["traj"][c(T,T,F),] <- matrix(c(20,50,40,70,80,90,60,30),2)


cat("\n###################################################################
########################## Test  LongData #########################
############################# Affichage ###########################
###################################################################\n")

ld1
ld4

selectSupTrajMinSize(ld2,3)
selectSupTrajMinSize(ld3n,3)
selectSupTrajMinSize(ld3n,20)
selectSupTrajMinSize(ld4n,6)


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++ Fin Test  LongData ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
