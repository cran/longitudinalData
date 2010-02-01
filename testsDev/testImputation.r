source("./testLongData.r")
source("./testPartition.r")
source("../R/imputation.r")

cat("###########################################################
########################## LOCF ###########################
###########################################################\n")

cat("### Sous fonctions ###\n")
cleanProg(trajImput.LOCB.begin,,,0)
cleanProg(trajImput.LOCF.middle,,,0)

a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a2 <- trajImput.LOCB.begin(a)
(a2 <- trajImput.LOCF.middle(a2))

a <- c(NA,NA,4,NA,NA,NA,NA)
a2 <- trajImput.LOCB.begin(a)
(a2 <- trajImput.LOCF.middle(a2))

a <- c(NA,NA,-4,5,NA,NA,NA)
a2 <- trajImput.LOCB.begin(a)
(a2 <- trajImput.LOCF.middle(a2))

a <- c(NA,NA,NA,NA,NA,NA)
a2 <- trajImput.LOCB.begin(a)
a2 <- trajImput.LOCF.middle(a2)

a <- c(1,NA)
a2 <- trajImput.LOCB.begin(a)
(a2 <- trajImput.LOCF.middle(a2))

a <- c(NA,-1)
a2 <- trajImput.LOCB.begin(a)
(a2 <- trajImput.LOCF.middle(a2))

a <- c(1)
a2 <- trajImput.LOCB.begin(a)
(a2 <- trajImput.LOCF.middle(a2))


cat("### function complete ###\n")
cleanProg(trajImput.LOCF,,,0)

par(mfrow=c(4,5))
a <- c(NA,2,NA,4,NA,1,2,NA,NA,-5,NA,3,NA,NA)
a2 <- trajImput.LOCF(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
a2 <- trajImput.LOCF(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,-4,5,NA,NA,NA)
a2 <- trajImput.LOCF(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,NA,NA,NA)
a2 <- trajImput.LOCF(a)

a <- c(1,NA)
a2 <- trajImput.LOCF(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,-1)
a2 <- trajImput.LOCF(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
trajImput.LOCF(a)




cat("###########################################################
########################## LOCB ###########################
###########################################################\n")

cat("### Sous fonctions ###\n")
cleanProg(trajImput.LOCF.end,,,0)
cleanProg(trajImput.LOCB.middle,,,0)

a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a <- trajImput.LOCF.end(a)
(a <- trajImput.LOCB.middle(a))

a <- c(NA,NA,4,NA,NA,NA,NA)
a <- trajImput.LOCF.end(a)
(a <- trajImput.LOCB.middle(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a <- trajImput.LOCF.end(a)
(a <- trajImput.LOCB.middle(a))

a <- c(NA,NA,NA,NA,NA,NA)
a <- trajImput.LOCF.end(a)
a <- trajImput.LOCB.middle(a)

a <- c(1,NA)
a <- trajImput.LOCF.end(a)
(a <- trajImput.LOCB.middle(a))

a <- c(NA,-1)
a <- trajImput.LOCF.end(a)
(a <- trajImput.LOCB.middle(a))

a <- c(1)
a <- trajImput.LOCF.end(a)
(a <- trajImput.LOCB.middle(a))

cat("### function complete ###\n")
cleanProg(trajImput.LOCF,,,0)

#par(mfrow=c(4,5))
a <- c(NA,2,NA,4,NA,1,2,NA,NA,-5,NA,3,NA,NA)
a2 <- trajImput.LOCB(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
a2 <- trajImput.LOCB(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,-4,5,NA,NA,NA)
a2 <- trajImput.LOCB(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,NA,NA,NA)
a2 <- trajImput.LOCB(a)

a <- c(1,NA)
a2 <- trajImput.LOCB(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,-1)
a2 <- trajImput.LOCB(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
trajImput.LOCB(a)



cat("###################################################################
###################### Interpolation Lineraire ####################
###################################################################\n")

cleanProg(trajImput.interpoLin.middle,,,0)
cleanProg(trajImput.globalSlope.beginEnd,,,0)
cleanProg(trajImput.interpoLin2,,,0)

cat("###############
### Linear interpolation 2 : global\n")

cat("### sous fonction ###\n")
a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a <- trajImput.globalSlope.beginEnd(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,4,NA,NA,NA,NA)
a <- trajImput.globalSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a <- trajImput.globalSlope.beginEnd(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,NA,NA,NA,NA)
a <- trajImput.globalSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1,NA)
a <- trajImput.globalSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,-1)
a <- trajImput.globalSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1)
a <- trajImput.globalSlope.beginEnd(a)
(a <- trajImput.interpoLin.middle(a))


cat("### Fonction complete ###\n")
a <- c(NA,2,NA,4,NA,1,2,NA,NA,NA,-25,NA,3,NA,NA)
a2 <- trajImput.interpoLin2(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
try(a2 <- trajImput.interpoLin2(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a2 <- trajImput.interpoLin2(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,NA,NA,NA)
try(trajImput.interpoLin2(a))

a <- c(1,NA)
try(a2 <- trajImput.interpoLin2(a))

a <- c(1,2,NA)
try(a2 <- trajImput.interpoLin2(a))
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,-1,2)
a2 <- trajImput.interpoLin2(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
a2 <- trajImput.interpoLin2(a)




cat("###############
### Linear interpolation 3 : Local\n")

cleanProg(trajImput.localSlope.begin,,,0)
cleanProg(trajImput.interpoLin3,,,0)

cat("### sous fonction ###\n")
a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a <- trajImput.localSlope.beginEnd(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,4,NA,NA,NA,NA)
a <- trajImput.localSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a <- trajImput.localSlope.beginEnd(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,NA,NA,NA,NA)
a <- trajImput.localSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1,NA)
a <- trajImput.localSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,-1)
a <- trajImput.localSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1)
a <- trajImput.localSlope.beginEnd(a)
(a <- trajImput.interpoLin.middle(a))


cat("### Fonction complete ###\n")
a <- c(NA,2,NA,4,NA,1,2,NA,NA,NA,-5,NA,3,NA,NA)
a2 <- trajImput.interpoLin3(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
try(a2 <- trajImput.interpoLin3(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a2 <- trajImput.interpoLin3(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,NA,NA,NA)
try(trajImput.interpoLin(a))

a <- c(1,NA)
try(a2 <- trajImput.interpoLin3(a))

a <- c(1,2,NA)
a2 <- trajImput.interpoLin3(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,-1,2)
a2 <- trajImput.interpoLin3(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
a2 <- trajImput.interpoLin3(a)




cat("###############
### Linear interpolation 4 : LOCF\n")

cat("### Fonction complete ###\n")
a <- c(NA,2,NA,4,NA,1,2,NA,NA,NA,-5,NA,3,NA,NA)
a2 <- trajImput.interpoLin4(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
try(a2 <- trajImput.interpoLin4(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a2 <- trajImput.interpoLin4(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,NA,NA,NA)
try(trajImput.interpoLin(a))

a <- c(1,NA)
try(a2 <- trajImput.interpoLin4(a))

a <- c(1,2,NA)
a2 <- trajImput.interpoLin4(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,-1,2)
a2 <- trajImput.interpoLin4(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
a2 <- trajImput.interpoLin4(a)


cat("###############
### Linear interpolation : bissectrice\n")

cleanProg(trajImput.localGlobalSlope.beginEnd,,,0)
cleanProg(trajImput.interpoLin,,,0)

cat("### sous fonction ###\n")
a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a <- trajImput.localGlobalSlope.beginEnd(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,4,NA,NA,NA,NA)
a <- trajImput.localGlobalSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a <- trajImput.localGlobalSlope.beginEnd(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,NA,NA,NA,NA)
a <- trajImput.localGlobalSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1,NA)
a <- trajImput.localGlobalSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,-1)
a <- trajImput.localGlobalSlope.beginEnd(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1)
a <- trajImput.localGlobalSlope.beginEnd(a)
(a <- trajImput.interpoLin.middle(a))


cat("### Fonction complete ###\n")
a <- c(NA,2,NA,4,NA,1,2,NA,NA,NA,-5,NA,3,NA,NA)
a2 <- trajImput.interpoLin(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
try(a2 <- trajImput.interpoLin(a))

a <- c(NA,NA,-4,NA,NA,NA)
a2 <- trajImput.interpoLin(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,NA,NA,NA)
try(trajImput.interpoLin(a))

a <- c(1,NA)
try(a2 <- trajImput.interpoLin(a))

a <- c(1,2,NA)
try(a2 <- trajImput.interpoLin(a))
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,-1,2)
a2 <- trajImput.interpoLin(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
a2 <- trajImput.interpoLin(a)




cat("############################################################
######################## CopyBegin #########################
############################################################\n")

cat("### Sous fonction ###\n")
cleanProg(trajImput.copy.begin,,,0)
cleanProg(trajImput.copy.end,,,0)
cleanProg(trajImput.copy.middle,,,0)
cleanProg(trajImput.copyMean,,,0)

(a <- c(NA,-2,NA,4,NA,1,NA,NA,NA,5,NA,3,NA,NA))
(m <- rep(1,14))
a <- trajImput.copy.begin(a,m)
a <- trajImput.copy.end(a,m)
(a <- trajImput.copy.middle(a,m))

(a <- c(NA,-2,NA,4,NA,1,NA,NA,NA,5,NA,3,NA,NA))
(m <- (14:1)*2)
a <- trajImput.copy.begin(a,m)
a <- trajImput.copy.end(a,m)
(a <- trajImput.copy.middle(a,m))

(a <- c(NA,2,NA,4,NA,1,NA,NA,NA,5,NA,-3,NA,NA))
(m <- (14:1)*2+10)
a <- trajImput.copy.begin(a,m)
a <- trajImput.copy.end(a,m)
(a <- trajImput.copy.middle(a,m))

(a <- c(NA,NA,4,NA,NA,NA,NA))
(m <- 1:7)
a <- trajImput.copy.begin(a,m)
a <- trajImput.copy.end(a,m)
(a <- trajImput.copy.middle(a,m))

(a <- c(NA,NA,-4,3,NA,NA,NA))
(m <- 1:7)
a <- trajImput.copy.begin(a,m)
a <- trajImput.copy.end(a,m)
(a <- trajImput.copy.middle(a,m))

(a <- c(NA,NA,NA,NA,NA,NA))
(m <- 1:6)
try(a <- trajImput.copy.begin(a,m))
try(a <- trajImput.copy.end(a,m))
try(a <- trajImput.copy.middle(a,m))


(a <- c(1,NA))
(m <- 1:2)
a <- trajImput.copy.begin(a,m)
a <- trajImput.copy.end(a,m)
(a <- trajImput.copy.middle(a,m))

(a <- c(NA,1))
(m <- 1:2)
a <- trajImput.copy.begin(a,m)
a <- trajImput.copy.end(a,m)
(a <- trajImput.copy.middle(a,m))

(a <- c(1))
(m <- 2)
a <- trajImput.copy.begin(a,m)
a <- trajImput.copy.end(a,m)
(a <- trajImput.copy.middle(a,m))

(a <- c(NA,NA,NA,4,3,6,NA,NA))
(m <- c(8, NA, 2,3,3,5,NA,4))
a <- trajImput.copy.begin(a,m)
a <- trajImput.copy.end(a,m)
try(a <- trajImput.copy.middle(a,m))


cat("### Fonction complete ###\n")
a <- c(NA,-2,NA,4,NA,1,2,NA,NA,NA,5,NA,3,NA,NA)
(m <- rep(1,15))
a2 <- trajImput.copyMean(a,m)
plot(a2,type="o")
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
(m <- 1:7)
a2 <- trajImput.copyMean(a,m)
plot(a2,type="o",ylim=c(1,10))
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,-4,3,NA,NA,NA)
(m <- 1:7)
a2 <- trajImput.copyMean(a,m)
plot(a2,type="o",ylim=c(-10,8))
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

(a <- c(NA,NA,NA,NA,NA,NA))
(m <- 1:7)
try(trajImput.copyMean(a,m))

a <- c(1,NA)
(m <- 1:2)
a2 <- trajImput.copyMean(a,m)
plot(a2,type="o")
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,1)
(m <- 1:2)
a2 <- trajImput.copyMean(a,m)
plot(a2,type="o")
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
(m <- 2)
trajImput.copyMean(a,m)

a <- c(NA,-2,NA,4,NA,1,2,NA,NA,NA,5,NA,3,NA,NA)
(m <- (15:1)*2)
a2 <- trajImput.copyMean(a,m)
plot(a2,type="o",ylim=c(-8,30))
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,2,NA,4,NA,1,2,NA,NA,NA,5,NA,-3,NA,NA)
(m <- (15:1)*2-10)
a2 <- trajImput.copyMean(a,m)
plot(a2,type="o",ylim=c(-8,20))
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")


a <- c(NA,NA,NA,4,3,6,NA,NA)
(m <- c(8, NA, 2,3,3,5,NA,4))
try(trajImput.copyMean(a,m))



cat("############################################################
#################### Imputation Method #####################
############################################################\n")
cleanProg(matrixImput,,,7) # trajImput.interpoLin  .LongData.Locf meanNA
cleanProg(trajImput,,,7) # trajImput.interpoLin trajImput.interpoLinS trajImput.LOCF trajImput.LOCB meanNA

imputation(ld2n,method="LOCF")
imputation(ld2n,method="LOCB")
imputation(ld2n,method="linearInterpolation")
imputation(ld2n,method="linearInterpolation2")
imputation(ld3n,method="LOCF")
imputation(ld3n,method="linearInterpolation")
imputation(ld2n,p2a,method="copyMean")


traj <- ld2n@traj
part <- p2a@clusters
imputation(traj,part,method="copyMean")



# Pour la premiere ou la derniere, on copie la variation de la moyenne.
# Pour les manquantes du millieu, on fait une interpolation qui copie, a l'échelle pret, la forme de la moyenne.
# S'il n'y a que des manquantes, on retourne que des manquantes



### Calcule les ordonnées des points situés sur la droite qui relie line[FirstNA] a line[LastNA]

### Plus précisément : on calcule les variations de la moyenne par rapport à une ligne moyenne qui irait tout droit
###   On ajoute ces variations a la ligne reliant InfNA et SupNA
### Une autre méthode est possible : calculer les variations par rapport a meanLine[infNA]
###   puis les normaliser en divisant par (meanLine[SupNA]-meanLine[InfNA])
###   puis les adapter a line en multipliant par (line[SupNA]-line[InfNA])
###   et enfin ajouter a line[InfNA]

### Fonctions moyenne, ecart type et which.min résistante aux NA.
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++ Fin Test  imputation +++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")



