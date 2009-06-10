source("../R/imputation.r")

cat("###########################################################
########################## LOCF ###########################
")

cat("### Sous fonctions ###\n")
a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a <- trajImput.LOCB.begin(a)
(a <- trajImput.LOCF.middle(a))

a <- c(NA,NA,4,NA,NA,NA,NA)
a <- trajImput.LOCB.begin(a)
(a <- trajImput.LOCF.middle(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a <- trajImput.LOCB.begin(a)
(a <- trajImput.LOCF.middle(a))

a <- c(NA,NA,NA,NA,NA,NA)
a <- trajImput.LOCB.begin(a)
a <- trajImput.LOCF.middle(a)

a <- c(1,NA)
a <- trajImput.LOCB.begin(a)
(a <- trajImput.LOCF.middle(a))

a <- c(NA,-1)
a <- trajImput.LOCB.begin(a)
(a <- trajImput.LOCF.middle(a))

a <- c(1)
a <- trajImput.LOCB.begin(a)
(a <- trajImput.LOCF.middle(a))

cat("### function complete ###\n")
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
")

cat("### Sous fonctions ###\n")
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
###################### Interpolation Lineraire ####################\n")

cat("### sous fonction ###\n")
a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a <- trajImput.globalSlope.begin(a)
a <- trajImput.globalSlope.end(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,4,NA,NA,NA,NA)
a <- trajImput.globalSlope.begin(a)
a <- trajImput.globalSlope.end(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a <- trajImput.globalSlope.begin(a)
a <- trajImput.globalSlope.end(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,NA,NA,NA,NA)
a <- trajImput.globalSlope.begin(a)
a <- trajImput.globalSlope.end(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1,NA)
a <- trajImput.globalSlope.begin(a)
a <- trajImput.globalSlope.end(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,-1)
a <- trajImput.globalSlope.begin(a)
a <- trajImput.globalSlope.end(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1)
a <- trajImput.globalSlope.begin(a)
a <- trajImput.globalSlope.end(a)
(a <- trajImput.interpoLin.middle(a))


cat("### Fonction complete ###\n")
a <- c(NA,2,NA,4,NA,1,2,NA,NA,NA,-5,NA,3,NA,NA)
a2 <- trajImput.interpoLin(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
try(a2 <- trajImput.interpoLin(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
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



cat("###################################################################
###################### Interpolation Lineraire ####################\n")

cat("### sous fonction ###\n")
a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a <- trajImput.localSlope.begin(a)
a <- trajImput.localSlope.end(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,4,NA,NA,NA,NA)
a <- trajImput.localSlope.begin(a)
a <- trajImput.localSlope.end(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a <- trajImput.localSlope.begin(a)
a <- trajImput.localSlope.end(a)
(a <- trajImput.interpoLin.middle(a))

a <- c(NA,NA,NA,NA,NA,NA)
a <- trajImput.localSlope.begin(a)
a <- trajImput.localSlope.end(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1,NA)
a <- trajImput.localSlope.begin(a)
a <- trajImput.localSlope.end(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(NA,-1)
a <- trajImput.localSlope.begin(a)
a <- trajImput.localSlope.end(a)
try(a <- trajImput.interpoLin.middle(a))

a <- c(1)
a <- trajImput.localSlope.begin(a)
a <- trajImput.localSlope.end(a)
(a <- trajImput.interpoLin.middle(a))


cat("### Fonction complete ###\n")
a <- c(NA,2,NA,4,NA,1,2,NA,NA,NA,-5,NA,3,NA,NA)
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
try(trajImput.interpoLin(a))

a <- c(1,NA)
try(a2 <- trajImput.interpoLin(a))

a <- c(1,2,NA)
a2 <- trajImput.interpoLin2(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,-1,2)
a2 <- trajImput.interpoLin2(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
a2 <- trajImput.interpoLin(a)




imputation(ld2n,method="LOCF")
imputation(ld2n,method="LOCB")
imputation(ld2n,method="linearInterpolation")
imputation(ld2n,method="linearInterpolation2")
imputation(ld3n,method="LOCF")
imputation(ld3n,method="linearInterpolation")



cat("############################################################
######################## CopyBegin #########################\n")

cat("### Sous fonction ###\n")
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


imputation(ld2n,p2a,method="copyMean")


traj <- ld2n@traj
part <- p2a@clusters
imputation(traj,part,method="copyMean")
cat("### Fin test Impute ###\n")



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


