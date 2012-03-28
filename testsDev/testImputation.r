source("../R/imputTraj.r")
source("../R/imputCross.r")
source("../R/imputLinearInterpol.r")
source("../R/imputCopyMean.r")
source("../R/imputation.r")

cat("###########################################################
####################### imput Traj #######################
###########################################################\n")

cat("### Sous fonctions ###\n")

#a0 <- c(9 , 8, 6, 5,5.5,7,8.5,9.5,9,10)
a1 <- c(NA, 2, NA, NA, NA, -5,NA,NA, 0, NA,NA)
a2 <- c(NA,NA,  4, NA, NA, NA,NA,NA,NA, NA,NA)+1
a3 <- c(NA,NA, NA,  4,  2, NA,NA,NA,NA, NA,NA)+4
a4 <- c(NA,NA, NA, NA, NA, NA,NA,NA,NA, NA,NA)
a5 <- c( 1,NA, NA, NA, NA, NA,NA,NA,NA, NA,NA)+9
a6 <- c(NA,NA, NA, NA, NA, NA,NA,NA,NA, NA, 1)+11
a7 <- c( 3, 4,4.1,3.5,3.9,2.5,NA,NA, 3,3.2, 4)
A <- rbind(-a1,-a2,-a3,-a4,-a5,-a6,-a7-0.2,-a7+0.5)+12


cleanProg(imput.locf.traj,,,0)
cleanProg(imput.locf,,,1)  ## imput.locf.traj

imput.locf.traj(a1)
(imp <- imput.locf(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)

cleanProg(imput.nocb,,,0)  ## imput.nocb.traj
(imp <- imput.nocb(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)



cleanProg(imput.trajMean.traj,,,0)
cleanProg(imput.trajMean,,,1)  ## imput.trajMean.traj

imput.trajMean.traj(a1)
(imp <- imput.trajMean(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cleanProg(imput.trajMedian.traj,,,0)
cleanProg(imput.trajMedian,,,1)  ## imput.trajMedian.traj

imput.trajMedian.traj(a1)
(imp <- imput.trajMedian(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cleanProg(imput.trajHotDeck.traj,,,0)
cleanProg(imput.trajHotDeck,,,1)  ## imput.trajHotDeck.traj

imput.trajHotDeck.traj(a1)
(imp <- imput.trajHotDeck(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cleanProg(imput.spline.traj,,,0)
cleanProg(imput.spline,,,1)  ## imput.trajSpline.traj

imput.spline.traj(a1)
(imp <- imput.spline(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cat("###########################################################
####################### imput Cross #######################
###########################################################\n")

cleanProg(imput.mean.col,,,0)
cleanProg(imput.crossMean,,,1)  ## imput.crossMean.cross

imput.mean.col(a1)

(imp <- imput.crossMean(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)

(imp <- imput.crossMean(A,force=TRUE))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)

(imp <- imput.crossMean(A,force=5))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)





cleanProg(imput.median.col,,,0)
cleanProg(imput.crossMedian,,,1)  ## imput.crossMedian.cross

imput.median.col(a1)

(imp <- imput.crossMedian(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)

(imp <- imput.crossMedian(A,force=TRUE))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)

(imp <- imput.crossMedian(A,force=5))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)



cleanProg(imput.hotDeck.col,,,0)
cleanProg(imput.crossHotDeck,,,1)  ## imput.crossHotDeck.cross

imput.hotDeck.col(a1)

(imp <- imput.crossHotDeck(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)

(imp <- imput.crossHotDeck(A,force=TRUE))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)

(imp <- imput.crossHotDeck(A,force=5))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)





cat("###################################################################
###################### Interpolation Lineraire ####################
###################################################################\n")

cleanProg(bisector,,,0)
bisector(0,0,1,0,0,2)
bisector(0,0,1,-2,0,1)
bisector(0,0,1,1,2,2)

cleanProg(imput.linearInterpol.centerTrajAux)
imput.linearInterpol.centerTrajAux(a1[2:9])
imput.linearInterpol.centerTrajAux(a7)

imput.linearInterpol.centerTraj(a1)
imput.linearInterpol.centerTraj(a7)


cleanProg(imput.linearInterpol.locfTraj)
cleanProg(imput.linearInterpol.locf,,,1)

imput.linearInterpol.locfTraj(a1)
(imp <- imput.linearInterpol.locf(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cleanProg(imput.linearInterpol.globalTraj)
cleanProg(imput.linearInterpol.global,,,1)

imput.linearInterpol.globalTraj(a1)
(imp <- imput.linearInterpol.global(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cleanProg(imput.linearInterpol.localTraj)
cleanProg(imput.linearInterpol.local,,,1)

imput.linearInterpol.localTraj(a1)
(imp <- imput.linearInterpol.local(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cleanProg(imput.linearInterpol.bisectorTraj)
cleanProg(imput.linearInterpol.bisector,,,1)

imput.linearInterpol.bisectorTraj(a1)
(imp <- imput.linearInterpol.bisector(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)







cat("###################################################################
############################# copyMean ############################
###################################################################\n")

cleanProg(imput.copyMean.centerTrajAux)
imput.copyMean.centerTrajAux(a1[2:9],1:8)
imput.copyMean.centerTrajAux(a7,1:11)

imput.copyMean.centerTraj(a1,1:11)
imput.copyMean.centerTraj(a7,1:11)


cleanProg(imput.copyMean.locfTraj)
cleanProg(imput.copyMean.locf,,,2)

imput.copyMean.locfTraj(a1,1:11)
(imp <- imput.copyMean.locf(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cleanProg(imput.copyMean.globalTraj)
cleanProg(imput.copyMean.global,,,2)

imput.copyMean.globalTraj(a1,1:11)
(imp <- imput.copyMean.global(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cleanProg(imput.copyMean.localTraj)
cleanProg(imput.copyMean.local,,,2)

imput.copyMean.localTraj(a1,1:11)
(imp <- imput.copyMean.local(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cleanProg(imput.copyMean.bisectorTraj)
cleanProg(imput.copyMean.bisector,,,2)

imput.copyMean.bisectorTraj(a1,1:11)
(imp <- imput.copyMean.bisector(A))
matplot(t(imp),type="l",lty=1)
matlines(t(A),lwd=4,type="o",pch=1,lty=1)


cat("############################################################
#################### Imputation Method #####################
############################################################\n")


cleanProg(.imputationMatrix,,,2) # min max
imputation(ld3n["traj"])
matplot(t(imputation(ld3n["traj"])),type="l",lty=1,col=1)
matlines(t(ld3n["traj"]),type="l",lty=1,col=2)

imputation(LD3n["traj"])

imputation(ld1n)
imputation(ld2n)
imputation(ld3n)
imputation(ld4n)
imputation(ld5n)

imputation(LD1n)
imputation(LD2n)
imputation(LD3n)
imputation(LD4n)
imputation(LD5n)
imputation(LD6n)
imputation(LD7n)


B <- rbind(c( NA, NA,  4,  2,  15, NA, NA),
           c( 15, 16, 24, 26, 20, 14, 12)-3,
           c( -5,  0,  3,  4,  3,  0,  1)-3,
           c( NA,  2, NA, 12, NA,  1, NA))
lB <- longData(B)
meanB <- apply(B,2,meanNA)


####################
### cross

matplot(t(imputation(lB,method="locf")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="locf",upperBound=Inf)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="locf",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



matplot(t(imputation(lB,method="nocb")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="nocb",upperBound=Inf)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="nocb",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



matplot(t(imputation(lB,method="crossMean")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="crossMedian")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="crossHotDeck")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



####################
### traj

matplot(t(imputation(lB,method="trajMean")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="trajMedian")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="trajHotDeck")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



matplot(t(imputation(lB,method="spline")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="spline",lowerBound="globalMin",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="spline",lowerBound=NA,upperBound=NA)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="spline",lowerBound=-5,upperBound=15+1:11)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



####################
### linearInterpol

matplot(t(imputation(lB,method="linearInterpol")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="linearInterpol",lowerBound="globalMin",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="linearInterpol",lowerBound=NA,upperBound=NA)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



matplot(t(imputation(lB,method="linearInterpol.global")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="linearInterpol.global",lowerBound="globalMin",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="linearInterpol.global",lowerBound=NA,upperBound=NA)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



matplot(t(imputation(lB,method="linearInterpol.local")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="linearInterpol.local",lowerBound="globalMin",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="linearInterpol.local",lowerBound=NA,upperBound=NA)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



matplot(t(imputation(lB,method="linearInterpol.bisector")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="linearInterpol.bisector",lowerBound="globalMin",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="linearInterpol.bisector",lowerBound=NA,upperBound=NA)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



####################
### copyMean

matplot(t(imputation(lB,method="copyMean")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="copyMean",lowerBound="globalMin",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="copyMean",lowerBound=NA,upperBound=NA)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



matplot(t(imputation(lB,method="copyMean.global")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="copyMean.global",lowerBound="globalMin",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="copyMean.global",lowerBound=NA,upperBound=NA)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



matplot(t(imputation(lB,method="copyMean.local")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="copyMean.local",lowerBound="globalMin",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="copyMean.local",lowerBound=NA,upperBound=NA)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")



matplot(t(imputation(lB,method="copyMean.bisector")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="copyMean.bisector",lowerBound="globalMin",upperBound="globalMax")["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")

matplot(t(imputation(lB,method="copyMean.bisector",lowerBound=NA,upperBound=NA)["traj"]),type="l",lty=1,col=1)
matlines(t(B),type="b",lty=1,col=2,lwd=2,pch=16)
lines(meanB,col=1,lwd=3,type="b")


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



