source("plotOneByOne.R")

##-----------------------------------------------------------------------------------------------#
##  Graphe des profils individuels obtenus après clustering    - Inserm umr 1027 / Equipe 2      #
##-----------------------------------------------------------------------------------------------#


## 0) Jeu de données simulées - Exemple  : 200 individus / 10 mesures
##--------------------------------------------------------------------

traj1 <- matrix(ncol=10,nrow=200)

for (j in 1:10){traj1[,j] <- rnorm(200,0,1)}
colnames(traj1) <- paste("Time",1:dim(traj1)[2],sep="")
row.names(traj1) <- 1:dim(traj1)[1]


### Librairies utiles
library(cluster); library(grDevices)

### CAH

hc <- hclust(as.dist(1-cor(t(traj1))),method="ward")
part5 <- factor(LETTERS[cutree(hc,k=5)])  # ... On opte pour 5 sous-groupes
table(part5)                     # ... Effectifs des groupes



###Définition d'un objet ParOneByOne

param2 <- parOneByOne(id=10,clusterActif="A",oneMean=TRUE,allMean=FALSE,allTraj=FALSE,allTrajGroup=FALSE,
                      extraTraj=FALSE,idExtraTraj=5,interval=FALSE,intervalSize=.95,legendPos="topleft")

#---------------
parUSER()
parUSER(id=3)
parUSER(id=3,idExtraTraj=5)
parUSER(id=3)



### choixUser() ###
plot(1)
while(TRUE)(param2 <- choixUser(param2,traj1,part5))


### colGraph ###
colGraph(1,part5)
colGraph(2,part5)
colGraph(20,part5)
colGraph(100,part5)

colGraph(1,part5,0.5)
colGraph(2,part5,0.2)

# 2) Fonction de graphe individuel
#-----------------------------------
# Vérification
plotByOne1(traj1,part5,param2)

#------------------



# 3) Fonction de synthèse
#-------------------------


 plotOneByOne1(traj1,part5,id=1)
 
 
 part6= as.factor(rep("G",nrow(traj1))); table(part6)
 
 plotByOne2(traj1,part6,param2)

  plotOneByOne1(traj1,part6,id=1)








